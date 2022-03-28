
createGraph <- function(X,cutoff = 0){
  # Generate graphs
  ## Create a bipartite (digraph) graph
  Edges       = computeEdgeTable(X)

  Edges = Edges %>% filter(weight > cutoff)
  G.digraph   = igraph::graph_from_data_frame(d = Edges, directed = T)

  ## Transform the bipartite graph to a unipartite graph by removal of nodes with outdegree = 0
  G.raw       = igraph::delete_vertices(G.digraph,igraph::V(G.digraph)[igraph::degree(G.digraph,mode = 'out') ==0])
  G.raw       = igraph::simplify(G.raw,remove.multiple = F, remove.loops = T)

  return(G.raw)
}

# normalize the edge weights to strengths
normalizeEdgeWeights =  function(G){
  igraph::E(G)$weight = igraph::E(G)$weight / igraph::strength(G, mode="out")[igraph::get.edgelist(G)[,1]]
  return(G)
}

computeEdgeTable <- function(X){
  if('Freq' %in% colnames(X))
    Edges = X %>% select(cue,response,Freq)
  else{
    Edges = X %>% filter(complete.cases(response)) %>% group_by(cue,response) %>% dplyr::summarise(weight = n()) %>% select(cue,response,weight)
  }

  colnames(Edges) = c('source','target','weight')
  return(Edges)
}

extractComponent <- function(G,mode = c("weak","strong")){

  comp            = igraph::components(G, mode = mode)
  maxComp         = which(comp$csize==max(comp$csize))
  maxSize         = max(comp$csize)
  componentSizes  = comp$csize
  removedVertices = names(which(comp$membership!=maxComp))
  subGraph        = igraph::delete_vertices(G,which(comp$membership != maxComp ))

  result = list("maxComp" = maxComp, "maxSize" = maxSize,
                "componentSizes" = componentSizes,"removedVertices" = removedVertices,
                "subGraph" = subGraph)
  return(result)

}

# Summarise network (Note: many of these functions differ in terms of being weighted or not)
getNetworkStats <- function(G){
  networkstats                 = list()
  networkstats['transitivity'] = igraph::transitivity(G,'global')
  networkstats['diameter']     = igraph::diameter(G,weights=NA) # 41 for R1
  networkstats['mDist']        = igraph::mean_distance(G, directed = TRUE, unconnected = TRUE)
  networkstats['density']      = igraph::edge_density(G,loops=F)*100
  networkstats['reciprocity']  = igraph::reciprocity(G)
  networkstats['issimple']     = igraph::is.simple(G)
  networkstats['ecount']       = igraph::ecount(G)
  networkstats['vcount']       = igraph::vcount(G)
  networkstats['directed']     = igraph::is.directed(G)


  networkstats['k_in']         = mean(igraph::degree(G, v = igraph::V(G), mode = "in", loops = FALSE, normalized = FALSE))
  networkstats['k_out']        = mean(igraph::degree(G, v = igraph::V(G), mode = "out", loops = FALSE, normalized = FALSE))
  networkstats['s_in']         = mean(igraph::strength(G, v = igraph::V(G), mode = "in", loops = FALSE))
  networkstats['s_out']        = mean(igraph::strength(G, v = igraph::V(G), mode = "out", loops = FALSE))

  ## Add community detection
  cl.walktrap   = igraph::cluster_walktrap(G, steps = 5)
  networkstats['modularity_walktrap']   = igraph::modularity(cl.walktrap)

  # Get the number of components and the size of the largest strongly connected component
  C = igraph::components(G,mode='strong')
  networkstats['n_strong_comp'] = C$no
  networkstats['max_strong_comp'] = max(C$csize)

  return(networkstats)
}


# Get the entropy of the responses over the cue distribution
getEntropy <- function(X,type){

  if(!"Freq" %in% colnames(X)){
    X  = X %>% filter(complete.cases(response)) %>% group_by(cue,response)  %>% summarise (Freq = n())
  }

  X$cue           = factor(X$cue)
  X$response      = factor(X$response)
  S               = sparseMatrix(i = as.numeric(X$cue),j = as.numeric(X$response), x = X$Freq)

  switch(type,
         cues = {
           H           = pbapply(S,1,entropy,simplify = F)
           labels      = X %>% group_by(cue) %>% summarise()
         },
         response ={
           H          = pbapply(S,2,entropy,simplify = F)
           labels     = X %>% group_by(response) %>% summarise()
         })
  H           = cbind(labels,H)
  return(H)
}

thresholdStrength = function(G,minStrength,maxStrength){
  G2 <- delete.edges(G, which(E(G)$weight < minStrength))
  G2 <- delete.edges(G2, which(E(G2)$weight > maxStrength))
  G2 <- delete.vertices(G2,which(degree(G2)<1))

  return(G2)
}

writeAdjacency = function(G,dataset){
  labs                = igraph::V(G)$name
  rc                  = as_edgelist(G,names=F)
  df                  = cbind(rc,E(G)$weight)
  write.table(df,file = paste(dataset,'.adj.tsv',sep=''),sep = '\t',row.names = F,col.names = F)
  write.table(labs,file = paste(dataset,'.labels.txt',sep=''),sep = '\t',row.names = F,col.names = F, quote = F)
}



extractSubgraph <- function(.data,words){
  if(is.list(words)==F){
    words = c(words)
  }

  selected_nodes = igraph::V(.data)[name %in% words]
  if(length(selected_nodes)>0){
    selected_egoV <- igraph::ego(.data, order=1, nodes = selected_nodes, mode = "out", mindist = 0)

    # Turn the returned list of igraph.vs objects into a graph
    G.ego <- igraph::induced_subgraph(.data,unlist(selected_egoV))


    # Extract largest component
    component  = extractComponent(G.ego,'strong')
    G.ego = component$subGraph
    G.ego = tidygraph::as_tbl_graph(G.ego,directed = TRUE)

    # Add weights
    G.ego = G.ego %>% activate(nodes) %>% mutate(s_out = centrality_degree(weights = .E()$weight,normalized = FALSE, mode="all"))
    #G.ego = G.ego %>% activate(edges) %>% mutate(weight = weight / igraph::strength(G.ego, mode="out")[igraph::get.edgelist(G.ego)[,1]])



    # Reduce to top shortest paths
    z= t(1-igraph::distances(graph = G.ego, v = words,to = igraph::V(G.ego)))
    zz = data.frame(labels=rownames(z),sim = as.numeric(z))
    sp_cut = zz %>% top_n(50,sim) %>% summarise(min(sim)) %>% as.numeric()
    G.ego = G.ego %>% activate(nodes) %>% mutate(sp_prox = zz$sim)
    G.ego = G.ego %>% activate(nodes) %>% filter(sp_prox >= sp_cut)

    G.ego = G.ego %>%
      # Remove loops
      activate(edges) %>%
      filter(!edge_is_loop()) %>%
      # Remove isolated nodes
      activate(nodes) %>%
      filter(!node_is_isolated())

    # Add communities for visualization purposes
    G.ego = G.ego %>% activate(nodes) %>%  mutate(community = as.factor(group_walktrap(weights = weight,steps=5)))

    # # Filter small communities
    # min_community_size = 10
    # communities = G.ego %>% activate(nodes) %>% as_tibble() %>% group_by(community) %>% dplyr::summarise(cs = n())
    # communities = communities %>% filter(cs > min_community_size) # 5
    # G.ego = G.ego %>% activate(nodes) %>% filter(community %in% communities$community)
    #

    source = G.ego %>% activate(edges) %>% pull(from) %>% data.frame(from = .)
    com = G.ego %>% activate(nodes) %>% select(community) %>% data.frame() %>% mutate(index = as.numeric(row.names(.)))
    source = left_join(source,com,by=c('from'='index'))
    G.ego = G.ego %>% activate(edges) %>% mutate(edge_group = source$community)
    return(G.ego)
  }
}

generateLayout <- function(G,backbone_cut = 0.4,weight_exp=1.5,layout_alg='nicely'){

  G = G %>% activate(edges) %>%  mutate(weight = scales::rescale(weight)^weight_exp)

  # Differentiate communities better
  bb <- graphlayouts::layout_as_backbone(igraph::as.undirected(G,mode='collapse'),keep = backbone_cut)
  igraph::E(G)$col <- FALSE
  igraph::E(G)$col[bb$backbone] <- TRUE
  G.layout = G.ego %>%  create_layout(layout = layout_alg)
  G.layout = G %>%  create_layout(layout = layout_alg)
  return(G.layout)
}


plotEgoGraph <- function(layout){
  pal = colorRampPalette(c("white","orange","red","green"))

  fig =  ggraph(layout) +
    geom_edge_fan(aes(alpha = weight,colour = edge_group), show.legend = FALSE,strength = 2) +
    #geom_edge_link(aes(alpha = weight,colour = edge_group), show.legend = FALSE,strength = 2) +
    #geom_edge_fan(aes(alpha = weight), show.legend = FALSE,strength = 2) +
    geom_node_point(aes(colour = community, alpha=0.4),size=0.4,show.legend = FALSE) +
    geom_node_text(aes(label = name,colour = community),
                   repel = T, show.legend = FALSE,size = 4,
                   segment.color = 'transparent',force = 0.4,
                   family='Roboto',point.padding = 0.005,max.iter = 4000) +
    scale_edge_alpha_continuous(range = c(0.02,0.7)) +
    scale_colour_manual(values = pal(length(unique(layout$community)))) +
    scale_edge_colour_manual(values = pal(length(unique(layout$community)))) +
    theme(legend.position = "none",
          plot.subtitle = element_text(color = '#F39B4C'),
          plot.title = element_text(color = '#F39B4C',family='googlefont'),
          panel.background = element_rect(fill = "black", color = NA),
          plot.background = element_rect(fill = "black", color = NA))

  return(fig)
}

# Positive point-wise mutual information
PPMI = function(P){
  if(class(P)[1] == 'tbl_graph'){
    P   = igraph::as_adjacency_matrix(P,attr='weight',names = TRUE)
  }

  N   = dim(P)[1]
  D   = Matrix::Diagonal(x = 1/(Matrix::colSums(P)/N))
  P   = P %*% D
  P@x = log2(P@x)
  P2  = pmax(P,0)
  return(P2)
}

# Negative point-wise mutual information
NPMI = function(P){
  if(class(P)[1] == 'tbl_graph'){
    P   = igraph::as_adjacency_matrix(P,attr='weight',names = TRUE)
  }

  N   = dim(P)[1]
  D   = Matrix::Diagonal(x = 1/(Matrix::colSums(P)/N))
  P   = P %*% D
  P@x = log2(P@x)
  #P2  = -1 * pmin(P,0)
  P2 = P
  #P2   = normalize(P2,'l1')
  P2   = igraph::graph_from_adjacency_matrix(P2,weighted=T,mode = 'directed')
  return(P2)
}


# Normalize sparse matrices (to use with random walk representations)
# taken from https://github.com/dselivanov/text2vec/blob/master/R/utils_matrix.R
normalize = function(m, norm = c("l1", "l2", "none")) {
  norm = match.arg(norm)

  if (norm == "none")
    return(m)

  norm_vec = switch(norm,
                    l1 = 1 / Matrix::rowSums(m),
                    l2 = 1 / sqrt(Matrix::rowSums(m ^ 2))
  )
  # case when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] = 0

  if(inherits(m, "sparseMatrix"))
    Matrix::Diagonal(x = norm_vec) %*% m
  else
    m * norm_vec
}


getPPMI <- function(G){
    P   = igraph::as_adjacency_matrix(G,attr='weight',names = TRUE)
    P   = normalize(P,'l1')
    N   = dim(P)[1]
    D   = Matrix::Diagonal(x = 1/(Matrix::colSums(P)/N))
    P   = P %*% D
    P@x = log2(P@x)
    P   = pmax(P,0)
    P   = normalize(P,'l1')
    P   = igraph::graph_from_adjacency_matrix(P,weighted=T,mode = 'directed')
    return(P)
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


nodeDistanceFilter = function(G,k=40){
  n_nodes = G %>% pull('name') %>% length()
  G = G %>% activate(nodes) %>% mutate(idx = 1:n_nodes)
  word_idx = G %>% filter(name==targetword) %>% pull(idx) %>% as.numeric()

  # note weights are here interpreted differently (e.g community see it as strengths, shortest distance as cost
  G = G %>% activate(nodes) %>% mutate(nodeDistTo = node_distance_to(word_idx,weights = 1/weight),
                                       nodeDistFrom = node_distance_from(word_idx,weights = 1/weight),
                                       maxFlowTo = node_max_flow_to(word_idx,capacity=1/weight),
                                       maxFlowfrom  = node_max_flow_from(word_idx,capacity=1/weight))

  z = G %>% activate(nodes) %>% as_tibble()
  G = G %>% activate(nodes) %>% top_n(k,-nodeDistTo)
  return(G)

}

# only keep a subset of labels
labelFilter = function(G,labels){
  G = G %>% activate(nodes) %>% filter(name %in% labels)
  return(G)
}


pathfinderGraph = function(G2,q=1,r=Inf){

  # Convert to distance
  mw  = G2 %>% activate(edges) %>% as_tibble() %>% summarise(max(weight)) %>% as_vector()
  G2 = G2 %>% activate(edges) %>% mutate(weight = 1-(weight/mw))
  M   = igraph::as_adjacency_matrix(G2,attr='weight',names = TRUE)
  M = as.matrix(M)
  M[M==0] = Inf

  M.pf = pathfinder(M,q ,r )

  # Convert to similarity
  M.pf = 1-M.pf
  M.pf[M.pf < 0]=0

  #rownames(M.pf) = rownames(M)
  #colnames(M.pf) = rownames(M)
  G.pf = as_tbl_graph(M.pf)

  message('Graph density original: ', round(igraph::edge_density(G2)*100,2))
  message('Graph density pathfinder: ', round(igraph::edge_density(G.pf)*100,2))


  G.pf = igraph::simplify(G.pf,remove.multiple = T, remove.loops = T)
  G.pf = tidygraph::as_tbl_graph(G.pf,directed = TRUE)
  pf_edges = G.pf %>% activate(edges) %>% as_tibble() %>% select(from,to)
  pf_edges$pf = 1
  tmp = G2 %>% activate(edges) %>% as_tibble()
  pf_edges = left_join(tmp,pf_edges,by=c('from','to'))
  G2 = G2 %>% activate(edges) %>% mutate(from = pf_edges$from,to = pf_edges$to,pf = pf_edges$pf)
  return(G2)
}


# Filter path length
pathLengthFilter = function(G.ego,words){
# Reduce to top shortest paths
z= t(1-igraph::distances(graph = G.ego, v = words,to = igraph::V(G.ego)))
zz = data.frame(labels=rownames(z),sim = as.numeric(z))
sp_cut = zz %>% top_n(50,sim) %>% summarise(min(sim)) %>% as.numeric()
G.ego = G.ego %>% activate(nodes) %>% mutate(sp_prox = zz$sim)
G.ego = G.ego %>% activate(nodes) %>% filter(sp_prox >= sp_cut)

G.ego = G.ego %>%
  # Remove loops
  activate(edges) %>%
  filter(!edge_is_loop()) %>%
  # Remove isolated nodes
  activate(nodes) %>%
  filter(!node_is_isolated())

return(G.ego)
}

addTranslation = function(G,X.trans,s){
  labels = G %>% as_tibble() %>% select(name)
  translation = left_join(labels,X.trans,by=c('name'='word'))
  G = G %>% left_join(.,translation,by=c('name'))
  G = G %>%  mutate(translation = paste(name,English,sep = s))
  return(G)
}
