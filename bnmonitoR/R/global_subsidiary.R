#' subsidiary functions for global.monitor.bn.node 
#' #' 
#subsidiary functions
#' @param pa.val
#' @param k 
#' @param counts.vec
#' @param alpha.vec 
get.pa.combo.score <- function(k, counts.vec, alpha.vec){
  c.vec <- counts.vec[(((k-1)*length(alpha.vec))+1): (((k-1)*length(alpha.vec))+length(alpha.vec))]
  lgamma(sum(alpha.vec)) - lgamma(sum(alpha.vec + c.vec)) + sum(lgamma(alpha.vec + c.vec)) - sum(lgamma(alpha.vec))
}

#' @describeIn  pa.val
get.num.pa.combo <- function(pa.val){
  if(is_empty(pa.val)==TRUE){
    num.pa.combo <- 1
  } else {
    num.pa.combo <- prod(num.values[which(colnames(df) %in% pa.val)])  
  }
  return(num.pa.combo)
} 

