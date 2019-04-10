#pa.names different from those of the global monitor 
#pa.val different from that of the global monitors (user inputed)
seq.pa.ch.monitor <- function(df, dag, node.idx, pa.names, pa.val,which.val){#takes input from bnlearn
  nodes <-nodes(dag)
  num.nodes <- length(dag$nodes)
  num.ch <- map(dag, `[[`, "children")  %>% map_int(length)
  num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  #num.pa <-map(dag, `[[`, "parents")  %>% map_int(length)
  #pa.val <- map(dag, `[[`, "parents")   
  
  alpha.vec <- rep(alpha/num.values[node.idx], num.values[node.idx])
  new.alpha <- alpha.vec #this is the one with learning
  #TODO we should learn over the whole data set, right? 
  counts <- rep(0,num.values[node.idx])
  z <- rep(0,dim(df)[1])
  z.learn <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    if(df[i,][[pa.names]]==pa.val){#TODO change this to accommodate parent pairs (use any)
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
