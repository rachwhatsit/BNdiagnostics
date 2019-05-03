#node diagnostics  
#TODO final outputs
#TODO temporal outputs 
#TODO graph output 


df=asia; dag = asia.dag



standardize <- function(vec,j){ #returns z score for the ith node with the jth worst level
  p <- unlist(vec) #pick the worst one
  sm <- -log(p)
  em <- - sum(p*log(p))
  vm <- sum(p*(log(p)^2)) - em^2
  zm <- sm[j]-em/sqrt(vm)
  return(zm)
}


marg.node.monitor <- function(dag,df){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  dag.bn.fit <- bn.fit(dag, df)
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  querygrain(dag.grain, nodes=colnames(df), type="marginal") %>%
  map2_dbl(.y=worst.level,standardize) -> final.z.score #this returns the vvery last marginal
  return(final.z.score)
}

marg.node.monitor.t <- function(dag,df, t){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  df[1:t,] -> df.cut
  dag.bn.fit <- bn.fit(dag, df.cut)
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df.cut[dim(df.cut)[1],])
  querygrain(dag.grain, nodes=colnames(df), type="marginal") %>%
    map2_dbl(.y=worst.level,standardize) -> z.score #this returns the vvery last marginal
  return(z.score)
}

#how to map it over a data frame.


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


mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
n <- list(1, 3, 5)
 
args2 <- list(mean = mu, sd = sigma, n = n)
pmap(args2, rnorm)

pmap_dbl(l,standardize)
