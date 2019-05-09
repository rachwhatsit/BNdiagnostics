#' the sequential parent child node monitor for Bayesian networks
#'@param df base R style data frame
#'@param dag bnlearn object with DAG structure, not a fitte dobject 
#'@param node.name name of child node
#'@param pa.names names of parents in the network
#'@param pa.val names of values to test
#'@param which.val which level of the value to test
seq.pa.ch.monitor <- function(df, dag, node.name, pa.names, pa.val,which.val){#takes input from bnlearn
  nodes <-nodes(dag)
  node.idx <- which(node.name ==colnames(df))
  num.nodes <- length(dag$nodes)
  num.ch <- map(dag$nodes, `[[`, "children")  %>% map_int(length)
  num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  
  alpha.vec <- rep(alpha/num.values[node.idx], num.values[node.idx])
  new.alpha <- alpha.vec #this is the one with learning
  counts <- rep(0,num.values[node.idx])
  #s <- rep(0,dim(df)[1]) ;s.learn <- rep(0,dim(df)[1])
  #e <- rep(0,dim(df)[1]) ;e.learn <- rep(0,dim(df)[1])
  #v <- rep(0,dim(df)[1]) ;v.learn <- rep(0,dim(df)[1])
  z <- rep(0,dim(df)[1]) ;z.learn <- rep(0,dim(df)[1])
  actual.prop <- rep(0,dim(df)[1]) 
  theor.prop <- rep(0,dim(df)[1]) ;theor.prop.learn <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    if(all(df[i,pa.names]==pa.val)){
      counts[as.numeric(df[i,node.idx])] <- counts[as.numeric(df[i,node.idx])] + 1
      new.alpha[as.numeric(df[i,node.idx])] <- new.alpha[as.numeric(df[i,node.idx])] + 1
    } else {
      new.alpha[as.numeric(df[i,node.idx])] <- new.alpha[as.numeric(df[i,node.idx])] + 1
    }
    which.val <- as.numeric(df[i,node.idx])
    p <- (alpha.vec +counts)/sum(alpha.vec+counts)
    actual.prop[i] <-counts[which.val]/sum(counts)
    theor.prop[i] <- p[which.val]/sum(p)
    s <- -log(p)[which.val]
    e <- sum(-(p*log(p)))
    v <- sum(p*log(p)^2) -e^2
    #z[i] <- (sum(s)-sum(e))/sqrt(sum(v))
    z[i] <- (s-e)/sqrt(v)
    p.learn <- (new.alpha) /sum(new.alpha)
    theor.prop.learn[i] <- p.learn[which.val]/sum(p.learn)
    s.learn <- -log(p)[which.val]
    e.learn <- sum(-(p.learn*log(p.learn)))
    v.learn <- sum(p.learn*log(p.learn)^2) -e.learn^2
    #z.learn <- (sum(s.learn)-sum(e.learn))/sqrt(sum(v.learn))
    z.learn[i] <- ((s.learn)-(e.learn))/sqrt((v.learn))
  }
  t <- 1:dim(df)[1]
  as.data.frame(cbind(t,z,z.learn)) %>% 
    reshape2::melt(id='t') %>% #TODO add the variable ggtitle to this plot 
    ggplot(aes(x=t, y=value, colour=variable)) + geom_point() + xlab('Relevant sample size') + ylab('Standardized Z Statistic') + theme_minimal() + 
    scale_colour_discrete(name="", labels=c("Expert's prior, without learning", "Expert's prior with learning")) + theme(legend.position="bottom")
}
