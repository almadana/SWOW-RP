
get_ancestors <- function(g, v) {
  return(igraph::subcomponent(g, v, "in"))
}

ancestors_graph <- function(g, v) {
  res <- igraph::induced_subgraph(g, get_ancestors(g, v))
  return(res)
}

get_root_node = function(g,v){
  ds = igraph::distances(ancestors_graph(g,v),to = v)
  root = rownames(ds)[ds==max(ds)]
  return(root)
}

extractEgoGraph = function(G,v,min_centrality,mode = 'all'){
  G.ego  = G %>% activate(nodes) %>%
    mutate(edge_node = node_is_adjacent(name %in% v,mode)) %>%
    filter(edge_node == TRUE) %>%
    mutate(component = group_components()) %>%
    filter(component == 1) %>% filter(!node_is_isolated()) %>%
    mutate(c_strength = centrality_degree(weights = weight,mode='in')) %>%
    filter(c_strength > min_centrality)

  return(G.ego)
}

pathfinderGraph = function(G,q=2,r=Inf){

  # Convert to distance
  mw  = G %>% activate(edges) %>% as_tibble() %>% summarise(max(weight)) %>% as_vector()
  G = G %>% activate(edges) %>% mutate(weight = 1-(weight/mw))
  M   = igraph::as_adjacency_matrix(G,attr='weight',names = TRUE)
  M = as.matrix(M)
  M[M==0] = Inf

  M.pf = pathfinder(M,q ,r )

  # Convert to similarity
  M.pf = 1-M.pf
  M.pf[M.pf < 0]=0

  G.pf = as_tbl_graph(M.pf)

  message('Graph density original: ', round(igraph::edge_density(G)*100,2))
  message('Graph density pathfinder: ', round(igraph::edge_density(G.pf)*100,2))


  G.pf = igraph::simplify(G.pf,remove.multiple = T, remove.loops = T)
  G.pf = tidygraph::as_tbl_graph(G.pf,directed = TRUE)
  pf_edges = G.pf %>% activate(edges) %>% as_tibble() %>% select(from,to)
  pf_edges$pf = 1
  tmp = G %>% activate(edges) %>% as_tibble()
  pf_edges = left_join(tmp,pf_edges,by=c('from','to'))
  G = G %>% activate(edges) %>% mutate(from = pf_edges$from,to = pf_edges$to,pf = pf_edges$pf)
  return(G)
}

addCommunity = function(G,minSize = 3){
  #G = G %>% activate(nodes) %>%  mutate(community = as.factor(group_infomap()))
  G = G %>% morph(to_undirected) %>% activate(nodes) %>%  mutate(community = as.factor(group_infomap())) %>% unmorph()
  #G = G %>% activate(nodes) %>%  mutate(community = as.factor(group_walktrap()))
  source = G %>% activate(edges) %>% pull(from) %>% data.frame(from = .)
  com = G %>% activate(nodes) %>% select(community) %>% data.frame() %>% mutate(index = as.numeric(row.names(.)))
  source = left_join(source,com,by=c('from'='index'))
  G = G %>% activate(edges) %>% mutate(edge_group = source$community)

  # get the community sizes
  communities = G %>% activate(nodes) %>% as_tibble() %>% group_by(community) %>% summarise(cs = n())
  communities = communities %>% filter(cs > minSize) # 5
  G = G %>% activate(nodes) %>% filter(community %in% communities$community)
  #G = G %>% activate(nodes) %>% filter(edge_node == TRUE) %>% mutate(component = group_components()) %>% filter(component == 1)
  G = G %>% activate(nodes) %>% mutate(component = group_components()) %>% filter(component == 1)

  return(G)
}

# q = 2, r = 18000 or Inf
plotEgoGraph = function(G,PF,q = 2,r = 10000,layoutAlg = 'nicely'){

    if(PF==TRUE){
      G = pathfinderGraph(G,q,r=400) %>% filter(pf==1)
    }

    # Add communities
    G = addCommunity(G,1)

    # Add betweenness centrality
    G = G %>% activate(nodes) %>% mutate(c_between = centrality_betweenness())

    # Color communities
    pal = colorRampPalette(c('#003f5c','#2f4b7c','#665191','#a05195','#d45087','#f95d6a','#ff7c43','#ffa600'))
    ncom = G %>% activate(nodes) %>% select(community) %>% as_tibble() %>% unique() %>% nrow()

    fig.ego = G %>%
        ggraph(layout = layoutAlg,niter = 15000) +
        geom_edge_fan(aes(alpha = weight,colour = edge_group,start_cap = label_rect(node1.name),
                          end_cap = label_rect(node2.name)), show.legend = FALSE,
                      arrow =  arrow(length = unit(1, 'mm'),type = 'closed',ends = 'last'),strength = 0.3) +
        geom_node_label(aes(label = name,fill=community,size = c_between), alpha = 1,
                        repel = F,show.legend = FALSE,colour='white',family='Roboto') +
        scale_size_continuous(range=c(3,5))+
        scale_edge_alpha_continuous(range=c(0.3,0.6)) +
        theme(legend.position = "none",
              plot.subtitle = element_text(color = '#F39B4C'),
              panel.background = element_rect(fill = "transparent", color = NA),
        ) +
        scale_fill_manual(values = pal(ncom)) +
        scale_colour_manual(values = pal(ncom)) +
        scale_edge_colour_manual(values = pal(ncom)) +
        coord_fixed()

      return(fig.ego)
}

plotCirclepack = function(G,maxHeight){

  fc = igraph::cluster_walktrap(G)
  g = as_tbl_graph(as.hclust(fc),mode = 'out',directed = TRUE)
  g = g %>% activate(nodes) %>% mutate(name = label)
  nodes = G.ego %>% activate(nodes) %>% as_tibble() %>% select(name,size = c_strength)

  g = g %>% activate('nodes') %>%  left_join(nodes,by = 'name') %>%
            mutate(size = ifelse(is.na(size),0,size))

  g2 = g %>% activate('nodes') %>% filter(height < maxHeight) %>%
            mutate(numAncestors = local_size(order = graph_order(), mode = 'in'))

  g2  = g2 %>% activate(nodes) %>% mutate(name = ifelse(name=='',paste0('leaf_',row_number()),name))
  g2i = as.igraph(g2)

  # get root nodes
  roots = tibble(name  = igraph::V(g2i)$name,root = sapply(igraph::V(g2i)$name,get_root_node,g = g2i))
  g2 = g2 %>% activate(nodes) %>% left_join(.,roots,by='name') %>%select(-leaf)
  g2 = g2 %>% mutate(size = ifelse(is.na(size),0,size))

  # Create layout and palet
  g.circle = ggraph::create_layout(g2, 'circlepack', weight = size,sort.by = size)
  pal = viridis(nlevels(as.factor(g.circle$root)),option = 'C',end = 0.8,begin = 0.2)

  fig = ggraph(g.circle) +
    geom_node_circle(aes(fill=as.factor(root),alpha = depth),size=0.2,colour='grey')  +
    #geom_edge_link() +
    geom_node_text(data = subset(g.circle,leaf==TRUE),aes(label=name,size = size),colour='black',alpha=0.6) +
    coord_fixed()  +
    scale_size(range = c(2.5,8)) +
    scale_alpha(range = c(0.08,0.35)) +
    scale_fill_manual(values = pal, na.value = 'white') +
    theme_void() +
    theme(legend.position="FALSE", plot.margin = unit(rep(0,4), "cm"))
  return(fig)
}
