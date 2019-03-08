library(bnlearn)
library(tidyverse)
library(rlang)

chds.dag = empty.graph(nodes = c("Social", "Economic", "Events", "Admission"))
arc.set = matrix(c("Social", "Economic",
                   "Social", "Events",
                   "Social", "Admission",
                   "Economic", "Events",
                   "Events", "Admission"),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("from", "to")))
arcs(chds.dag) = arc.set

skeleton(chds.dag)
moral(chds.dag)

data("learning.test")
res = gs(learning.test)
res = set.arc(res, "A", "B")
fitted = bn.fit(res, learning.test)
new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1),
                 byrow = TRUE, ncol = 3,
                 dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
fitted$B = as.table(new.cpt)
all.equal(res, bn.net(fitted))
res = hc(gaussian.test)
fitted = bn.fit(res, gaussian.test)

graphviz.plot(fitted)
fitted[['A']]$children

length(fitted)
prior <- list()
prior <-

prior.pa <- function(i){
  alpha.ess <- 3
  num.ch <- length(fitted[[i]]$children)
  if(num.ch==0){
    alpha.mat <- NA
  } else {
    num.alpha.i <- length(levels(learning.test[[i]]))
    alpha <- rep(alpha.ess/num.alpha.i, num.alpha.i)
    alpha.mat <- as.data.frame(matrix(rep(alpha,num.ch),nrow = num.ch))
  }
  return(alpha.mat)
}

num.ch <- map(1:6, prior.pa)

#setting the prior 

df = df; dag = chds.dag; alpha = 3

num.nodes <- length(dag$nodes)

num.ch <- map(dag$nodes, `[[`, "children")  %>% map_int(length)
num.pa <- map(dag$nodes, `[[`, "parents") %>% map_int(length)
num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})

#initalize  

set.bn.prior <- function(i){
  alpha <- rep(alpha/num.values[i], num.values[i])
  pa.names <- c(dag$nodes[colnames(df)[i]][[1]][[3]], names(dag$nodes[colnames(df)[i]]))
  df %>% count(pa.names)
}

prior.nodes <- map(1:num.nodes, set.bn.prior) 

node.score <- function(i){ 
  num.pa[i]
  dag$nodes[[i]][[3]]
  df %>% count(dag$nodes[i][[3]])#pulls the parents out for the ith node
  
}

#global monitor 

df = df; dag = chds.dag; 

dag$nodes

score  


#parent child montitor

#node monitor 


