#node diagnostics  
#TODO final outputs
#TODO temporal outputs 
#TODO graph output 


df=asia; dag = asia.dag

dag.bn.fit <- bn.fit(dag, df)
dag.grain = as.grain(dag.bn.fit)


standardize <- function(vec,j){ #returns z score for the ith node with the jth worst level
  p <- unlist(vec[j]) #pick the worst one
  sm <- -log(p)
  em <- - sum(p*log(p))
  vm <- sum(p*(log(p)^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}


marg.node.monitor <- function(dag,df){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  dag.junction = compile(as.grain(dag.grain))#convert to a gRain object 
  querygrain(dag.grain, nodes=colnames(df), type="marginal") %>%
  map2(1:num.nodes,standardize) -> z.score
  return(z.score)
}

marg.node.monitor(dag.grain,df)

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


