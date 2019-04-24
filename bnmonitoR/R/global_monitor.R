#TODO ouptut plot
#TODO does this go in the right place?? 
#subsidiary functions
get.num.pa.combo <- function(pa.val){#TODO delete this function
  if(is_empty(pa.val)==TRUE){
    num.pa.combo <- 1
  } else {
    num.pa.combo <- prod(num.values[which(colnames(df) %in% pa.val)])  
  }
  return(num.pa.combo)
} 

#subsidiary functions
get.pa.combo.score <- function(k, counts.vec, alpha.vec){
  c.vec <- counts.vec[(((k-1)*length(alpha.vec))+1): (((k-1)*length(alpha.vec))+length(alpha.vec))]
  lgamma(sum(alpha.vec)) - lgamma(sum(alpha.vec + c.vec)) + sum(lgamma(alpha.vec + c.vec)) - sum(lgamma(alpha.vec))
}

#function only works for output from the bnlearn algorithm
global.monitor.bn.node <- function(node.idx,dag,alpha,df){#j is the index of the parent set
  
  num.nodes <- length(dag$nodes)
  pa.val <- map(dag$nodes, `[[`, "parents") #proeprocessing
  num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  num.pa.combo <- if_else(is_empty(pa.val[node.idx]),1,prod(num.values[which(colnames(df) %in% unlist(pa.val[node.idx]))]))
  
  
  alpha.vec <<- rep(alpha/num.values[node.idx], num.values[node.idx]) #TODO adjust so there is a different way to code alpha
  pa.names <-c(unlist(pa.val[node.idx] ), colnames(df)[node.idx])
  df %>% count(!!!(syms(pa.names))) %>% complete %>% pull(n) ->> counts.vec
  map_dbl(1:num.pa.combo[node.idx], ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec 
  score <- unlist(sum(scores.vec))
  return(score)#returns global and pach monitor  
}  
