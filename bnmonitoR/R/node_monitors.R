#node diagnostics  


standardize <- function(vec,j){ #returns z score for the ith node with the jth worst level
  p <- vec[j] #pick the worst one
  sm <- -log(p)
  em <- - sum(vec[j]*log(vec[j]))
  vm <- sum(vec[j]*(log(vec[j])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}


chds.gs -> dag; df -> df; obs.vec <- rep(1,4)
marg.node.monitor <- function(dag,df,obs.vec){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  dag.junction = compile(as.grain(dag))#convert to a gRain object 
  querygrain(dag.junction, nodes=colnames(df), type="marginal") %>%
  map2(1:num.nodes,standardize) -> z.score
  return(z.score)
}

pass.ev <-function(i){
  ev <- querygrain(setEvidence(net1,evidence=list(df[dim(df)[1],-i]), nodes=colnames(df)[i]))
  evi <- ev[[i]]
  return(evi)
}



cond.node.monitor <- function(dag,df,obs.vec){
  dag.junction = compile(as.grain(dag))#convert to a gRain object 
  map(1:length(colnames(df)),pass.ev) %>% 
    map2(obs.vec,standardize)-> z.score
  return(z.score)
}


