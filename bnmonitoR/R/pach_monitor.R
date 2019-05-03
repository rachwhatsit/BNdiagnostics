#' the sequential parent child node monitor for Bayesian networks
#'@param df base R style data frame
#'@param dag bnlearn object with DAG structure, not a fitte dobject 
#'@param node.name name of child node
#'@param pa.names names of parents in the network
#'@param pa.val names of values to test
#'@param which.val which level of the value to test
seq.pa.ch.monitor <- function(df, dag, node.name, pa.names, pa.val,which.val){#takes input from bnlearn
  nodes <-nodes(dag)
  node.idx <- which(node.name ==names(dag$nodes))
  num.nodes <- length(dag$nodes)
  num.ch <- map(dag$nodes, `[[`, "children")  %>% map_int(length)
  num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  
  alpha.vec <- rep(alpha/num.values[node.idx], num.values[node.idx])
  new.alpha <- alpha.vec #this is the one with learning
  counts <- rep(0,num.values[node.idx])
  z <- rep(0,dim(df)[1])
  z.learn <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    if(all(df[i,pa.names]==pa.val)){#TODO change this to accommodate parent pairs (use any)
      counts[as.numeric(df[i,node.idx])] <- counts[as.numeric(df[i,node.idx])] + 1
      new.alpha[as.numeric(df[i,node.idx])] <- new.alpha[as.numeric(df[i,node.idx])] + 1
    } else {
      counts <- counts
      new.alpha[as.numeric(df[i,node.idx])] <- new.alpha[as.numeric(df[i,node.idx])] + 1
    }
    p <- (alpha.vec + counts)/sum(alpha.vec + counts)
    s <- -log(p)
    e <- -sum(p*(log(p)))
    v <- sum(p*(log(p))^2)
    z[i] <- (s[which.val]-e)/sqrt(v)
    p.learn <- (new.alpha + counts)/sum(new.alpha + counts)
    s.learn <- -log(p.learn)
    e.learn <- -sum(p.learn*(log(p.learn)))
    v.learn <- sum(p.learn*(log(p.learn))^2)
    z.learn[i] <- (s.learn[which.val]-e.learn)/sqrt(v.learn)
  }
  t <- 1:dim(df)[1]
  as.data.frame(cbind(t,z,z.learn)) %>% 
    reshape2::melt(id='t') %>% #TODO add the variable ggtitle to this plot 
    ggplot(aes(x=t, y=value, colour=variable)) + geom_line() + xlab('Relevant sample size') + ylab('Standardized Z Statistic') + theme_minimal() + 
    scale_colour_discrete(name="", labels=c("Expert's prior, without learning", "Expert's prior with learning")) + theme(legend.position="bottom")
}
