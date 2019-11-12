library(bnlearn); library(tidyverse); library(rlang)

setwd("C:/Users/rachel/Documents/BNdiagnostics/")
df <- read.csv("CHDS.latentexample1.csv")
chds.dag = empty.graph(nodes = c("Social", "Economic", "Events", "Admission"))
arc.set = matrix(c("Social", "Economic",
                   "Social", "Events",
                   "Social", "Admission",
                   "Economic", "Events",
                   "Events", "Admission"),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("from", "to")))
arcs(chds.dag) = arc.set

chds.dag <- bn.fit(chds.dag,df)
skeleton(chds.dag)
moral(chds.dag)
graphviz.plot(chds.dag)

#find a BN from the chds data
chds.bn.fit = gs(df)
chds.gs = bn.fit(chds.bn.fit, df)
#new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1), #adjust the CPT
#                 byrow = TRUE, ncol = 3,
#                 dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
#fitted$B = as.table(new.cpt)
graphviz.plot(chds.gs)


#worked example of the monitors from an elicited example

df = df; dag = chds.dag; alpha = 3

num.nodes <- length(dag$nodes)

num.ch <- map(dag$nodes, `[[`, "children")  %>% map(length)
num.pa <- map(dag$nodes, `[[`, "parents") %>% map_int(length)
chds.pa.val <- map(dag$nodes, `[[`, "parents") 
num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})

get.num.pa.combo <- function(pa.val){
  if(is_empty(pa.val)==TRUE){
    num.pa.combo <- 1
  } else {
    num.pa.combo <- prod(num.values[which(colnames(df) %in% pa.val)])  
  }
  return(num.pa.combo)
}


get.pa.combo.score <- function(k, counts.vec, alpha.vec){
  c.vec <- counts.vec[(((k-1)*length(alpha.vec))+1): (((k-1)*length(alpha.vec))+length(alpha.vec))]
  lgamma(sum(alpha.vec)) - lgamma(sum(alpha.vec + c.vec)) + sum(lgamma(alpha.vec + c.vec)) - sum(lgamma(alpha.vec))
}

chds.num.pa.combo <- map_dbl(chds.pa.val, get.num.pa.combo)#example of the node in action 
num.pa.combo <- chds.num.pa.combo #for the example

#code the global monitor for a chunk of the combos
#works for elicited graph structure 
global.monitor.bn.node <- function(i,num.pa.combo,alpha){#j is the index of the parent set
  alpha.vec <- rep(alpha/num.values[i], num.values[i])
  pa.names <- c(dag$nodes[colnames(df)[i]][[1]][[3]], names(dag$nodes[colnames(df)[i]]))
  df %>% count(!!!(syms(pa.names))) %>% complete #%>% pull(n) -> counts.vec
  map_dbl(1:num.pa.combo[i], ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec
  score <- sum(scores.vec)
  return(list(score,scores.vec))#returns global and pach monitor  
}  

global.monitor.bn.node(3, chds.num.pa.combo,alpha=3)
global.monitor.bn.node(4, chds.num.pa.combo,alpha=2)
global.monitor.bn.node(1, chds.num.pa.combo,alpha=2)

map(1:num.nodes,~global.monitor.bn.node(.x,num.pa.combo,alpha=3))#global monitor

#think of how to visualize this in shiny 

#worked example with a model of the class BN fit
dag.gs <- chds.gs
chds.gs.nodes <-nodes(chds.gs)

num.ch.gs <- map(chds.gs, `[[`, "children")  %>% map_int(length)
num.pa.gs <-map(chds.gs, `[[`, "parents")  %>% map_int(length)
chds.gs.pa.val <- map(chds.gs, `[[`, "parents")   


num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
chds.gs.num.pa.combo <- map_dbl(chds.gs.pa.val, get.num.pa.combo)#example of the node in action 

dag <- chds.gs; num.pa.combo <- chds.gs.num.pa.combo #for the example


#global and pach node monitors for things from the gs algorithm 
global.monitor.bn.node.gs <- function(i,num.pa.combo,alpha,df){#j is the index of the parent set
  alpha.vec <<- rep(alpha/num.values[i], num.values[i])
  pa.names <-c(unlist(chds.gs.pa.val[i] ), colnames(df)[i])
  df %>% count(!!!(syms(pa.names))) %>% complete %>% pull(n) ->> counts.vec
  map_dbl(1:num.pa.combo[i], ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec #TODO misfiring because of global variables
  score <- unlist(sum(scores.vec))
  return(list(scores.vec))#returns global and pach monitor  
}  

global.monitor.bn.node(2, chds.num.pa.combo,alpha=3)#as a comparison for the elicited model
global.monitor.bn.node.gs(node.idx = 1, dag = chds.gs,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(node.idx = 2, dag = chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(3, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(4, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model


map(1:num.nodes,~global.monitor.bn.node.gs(.x,chds.gs.num.pa.combo,alpha=3))#global monitor

pa.ch.bn.monitor <- function(node.idx, num.pa.combo, pa.combo.idx, df, alpha){
  z <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    df.cut <- df[1:i,]
    global.monitor.bn.node.gs(i = node.idx,num.pa.combo = num.pa.combo, df = df.cut, alpha = alpha)[[1]][[pa.combo.idx]] -> z[i]
  }
  return(z)
}

node.idx=3; dag = chds.gs;num.pa.combo=chds.gs.num.pa.combo; pa.combo.idx = 3; df = df;alpha = 3
pa.ch.bn.monitor(node.idx=3, num.pa.combo=chds.gs.num.pa.combo, pa.combo.idx = 3, df = df,alpha = 3)->test

seq.pa.ch.monitor <- function(df, node.idx, pa.names, pa.val,which.val){
  alpha.vec <- rep(alpha/num.values[node.idx], num.values[node.idx])
  new.alpha <- alpha.vec
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
    melt(id='t') %>% #TODO add the variable ggtitle to this plot 
    ggplot(aes(x=t, y=value, colour=variable)) + geom_line() + xlab('Relevant sample size') + ylab('Standardized Z Statistic') + theme_minimal() + 
    scale_colour_discrete(name="", labels=c("Expert's prior, without learning", "Expert's prior with learning")) + theme(legend.position="bottom")
}

df=df; node.idx = 3;pa.names = "Social";pa.val = "High"; which.val=1

seq.pa.ch.monitor(df=df, node.idx = 3,pa.names = "Social",pa.val = "High", which.val = 1)
