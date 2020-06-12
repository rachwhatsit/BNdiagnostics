#' global monitors and the dependent functions 
#' @importClassesFrom bnlearn bn.fit
#' @param node.idx index from the dag nodes
#' @param dag bnlearn object specifying a dag 
#' @param alpha single integer, usually the number of max levels in df
#' @param df a base R style dataframe 
#'@importFrom purrr map map_int map_dbl 
#'@importFrom tidyr complete 
#'@import rlang 
#'@import dplyr   
#'@export
global.monitor.bn.node <- function(node.idx,dag,alpha,df){#j is the index of the parent set
  num.nodes <- length(dag$nodes)
  pa.val <- purrr::map(dag$nodes, `[[`, "parents") #proeprocessing
  num.values <- purrr::map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  num.pa.combo <- dplyr::if_else(rlang::is_empty(pa.val[node.idx]),1,prod(num.values[which(colnames(df) %in% unlist(pa.val[node.idx]))]))
  
  
  alpha.vec <<- rep(alpha/num.values[node.idx], num.values[node.idx]) 
  pa.names <-c(unlist(pa.val[node.idx] ,use.names = FALSE), names(pa.val)[[node.idx]])
  df %>% count(!!!(syms(pa.names))) %>% complete(!!!(syms(pa.names)),fill = list(n = 0)) %>% pull(n) ->> counts.vec 
  map_dbl(1:num.pa.combo, ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec 
  score <- unlist(-sum(scores.vec))
  return(score)#returns global and pach monitor  
}  

global.monitor.bn.node.t <- function(node.idx,dag,alpha,df){#j is the index of the parent set
  num.nodes <- length(dag$nodes)
  pa.val <- map(dag$nodes, `[[`, "parents") #proeprocessing
  num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
  num.pa.combo <- if_else(is_empty(pa.val[node.idx]),1,prod(num.values[which(colnames(df) %in% unlist(pa.val[node.idx]))]))
  
  
  alpha.vec <<- rep(alpha/num.values[node.idx], num.values[node.idx]) 
  pa.names <-c(unlist(pa.val[node.idx] ,use.names = FALSE), names(pa.val)[[node.idx]])
  score <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    df.cut <- df[1:i,]
    df.cut %>% count(!!!(syms(pa.names))) %>% complete(!!!(syms(pa.names)),fill = list(n = 0)) %>% pull(n) ->> counts.vec 
    map_dbl(1:num.pa.combo, ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec 
    score[i] <- unlist(-sum(scores.vec))
  }
  score2 <- score
  score2[-1] -> score2
  plot(log(score2/score[-dim(df)[1]]))
  title("Global Monitor")
  return(score)#returns global and pach monitor  
}  


#'@describeIn  global.monitor.bn.node 
#'@importFrom purrr map_dbl
#'@export
global.monitor.tbl <- function(dag, alpha, df){#node.scores output from global.bn
  
  node.scores <- map_dbl(.x=1:length(dag$nodes), dag, alpha, df, .f= global.monitor.bn.node)
  result <-as.data.frame(cbind(names(dag$nodes),as.numeric(-node.scores)))
  return(result)
}

#'@describeIn  global.monitor.bn.node
#'@importFrom RColorBrewer brewer.pal
#'@importFrom purrr map_dbl map
#'@importFrom grDevices colorRampPalette
#'@importFrom DiagrammeR create_node_df create_edge_df create_graph render_graph
#'
#'@export
global.monitor.graph <- function(dag, alpha, df){#node.scores output from global.bn
  
  node.scores <- map_dbl(.x=1:length(dag$nodes), dag, alpha, df, .f= global.monitor.bn.node)
  result <-data.frame(cbind(names(dag$nodes),node.scores))
  result$node.scores <- as.numeric(result$node.scores)#return this result for each node 
  
  my.colors = brewer.pal(length(names(dag$nodes)),"Blues")
  max.val <- ceiling(max(abs(node.scores)))
  my.palette <- colorRampPalette(my.colors)(max.val)
  node.colors <- my.palette[floor(abs(node.scores))]
  nodes <- create_node_df(n=length(dag$nodes),
                          type= names(dag$nodes),
                          label=names(dag$nodes),
                          style="filled",
                          fontcolor="black",
                          fillcolor=node.colors)
  
  from.nodes <- map(dag$nodes, `[[`, "parents") %>% unlist %>% unname
  to.nodes <-map(dag$nodes, `[[`, "parents") %>% unlist %>% names %>% substr(1,1)
  
  edges <- create_edge_df(from=match(from.nodes,names(dag$nodes)),
                          to=match(to.nodes,names(dag$nodes)))
  create_graph(
    nodes_df = nodes,
    edges_df = edges) %>%
    render_graph(title="Global Monitors",layout="tree")#remove tree when you've got model 0 to model 1 diff
}

