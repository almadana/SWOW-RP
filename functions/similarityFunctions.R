# Weight graph based on strength, PPMI or Katz Walks (RW using PPMI)
weightMatrix = function(X,weight,alpha){
  G     = createGraph(X)
  comp  = extractComponent(G,'strong')
  G     = comp$subGraph
  P     = igraph::as_adjacency_matrix(G,attr='weight',names = TRUE)
  P     = normalize(P,'l1')

  switch(weight,
         strength = {
           #message('calculating strength')
         },
         PPMI = {
           #message('calculating PPMI')
           P     = PPMI(P)
           P     = normalize(P,'l1')
         },
         RW   = {
           #message('calculating RW')
           P     = PPMI(P)
           P     = normalize(P,'l1')
           P     = katzWalk(P,alpha)
           P     = PPMI(P)
           P     = normalize(P,'l1')
         }
  )

  return(P)
}


# Add indirect paths using Katz walks
# Note: this function is very slow in R! Use matlab script instead
katzWalk = function(G,alpha){
  I = diag(1, dim(G)[1]);
  K = solve(I - alpha*G)
  K@Dimnames = G@Dimnames
  return(K)
}

# Convert to normalized katz Index graph ("random walk")
katzIndex = function(G,alpha){
  #P = igraph::as_adjacency_matrix(G,attr='weight',names = TRUE)
  I = diag(1, dim(P)[1]);
  K = solve(I - alpha*P);
  P = PPMI(K)
  diag(P) = 0
  P = normalize(P,'l1')
  return(igraph::graph_from_adjacency_matrix(Matrix::as.matrix(P),weighted = TRUE))
}


# Matrix cosine similarity
cosineMatrix = function(G){
  G = Matrix::as.matrix(G)
  if(class(G)[1] == 'dgeMatrix' || class(G)[1] == 'dgCMatrix' || class(G)[1] == 'matrix'){
    Gn = normalize(G,norm = 'l2')
    S = tcrossprod(Gn)
    S = as.array(S)
    return(S)
  }
  else{
    warning('G should be a dgeMatrix or a dgCMatrix')
  }
}

# For large matrices consider calculating cosine similarity for pairs of rows
cosineRows = function(a,b){
  s = (a/norm(as.matrix(a),type = 'f')) %*% (b/norm(as.matrix(b),type = 'f'))
  return(s)
}

constructSimilarityMatrix = function(X,weight,alpha){
  G     = createGraph(X)
  comp  = extractComponent(G,'strong')
  G     = comp$subGraph
  P     = igraph::as_adjacency_matrix(G,attr='weight',names = TRUE)
  P     = normalize(P,'l1')

  switch(weight,
         strength = {
           #message('calculating strength')
         },
         PPMI = {
           #message('calculating PPMI')
           P     = PPMI(P)
           P     = normalize(P,'l1')
         },
         RW   = {
           #message('calculating RW')
           P     = PPMI(P)
           P     = normalize(P,'l1')
           P     = katzWalk(P,alpha)
           P     = PPMI(P)
           P     = normalize(P,'l1')
         }
  )

  S     = cosineMatrix(P)
  return(S)
}

lookupSimilarityMatrix = function(S,X){
  v = rep(NaN,dim(X)[1])
  labels = dimnames(S)[[1]]

  for( i in 1:dim(X)[1]) {
    w_a = X$WordA[i]
    w_b = X$WordB[i]
    if(w_a %in% labels && w_b %in% labels){
      v[i] =  S[w_a,w_b]
    }
  }
  return(v)
}


# Utility function: correlation Table
# First part of extracting names seems highly inefficient
corrTable = function(x,rmethod,absolute){
  rr = psych::corr.test(x,method = rmethod)
  v1 = list();  v2 = list();  rn = row.names(rr$r); ctr= 1
  for(i in 1:(length(rn)-1)){
    for(j in (i+1):length(rn)){
      v1[ctr] = rn[i]
      v2[ctr] = rn[j]
      ctr = ctr + 1
    }
  }

  X = data.frame(var1 = unlist(v1),var2=unlist(v2),r= rr$r[lower.tri(rr$r)],
                  cl = rr$ci.adj$lower.adj, cu = rr$ci.adj$upper.adj,n = rr$n)

  if(absolute==TRUE){
    X = X %>% mutate(sign = ifelse(r<0,'neg','pos')) %>% mutate(sign = as.factor(sign))
    X = X %>% mutate(r = abs(r))
    X = X %>% mutate(cl = ifelse(cl < 0 & cu >0,cl,abs(cl)))
    X = X %>% mutate(cu = abs(cu))
  }
  # Make symmetrical
  tmp = X
  tmp$var1 = X$var2
  tmp$var2 = X$var1
  X = rbind(X,tmp)

  return(X)
}
