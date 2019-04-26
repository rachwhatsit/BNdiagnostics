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
  pa.names <-c(unlist(pa.val[node.idx] ,use.names = FALSE), names(pa.val)[[node.idx]])
  df %>% count(!!!(syms(pa.names))) %>% complete(!!!(syms(pa.names)),fill = list(n = 0)) %>% pull(n) ->> counts.vec 
  map_dbl(1:num.pa.combo, ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec 
  score <- unlist(sum(scores.vec))
  return(score)#returns global and pach monitor  
}  


global.monitor.graph <- function(dag, alpha, df){#node.scores output from global.bn
  
  node.scores <- map_dbl(.x=1:length(dag$nodes), dag, alpha, df, .f= global.monitor.bn.node)
  result <-as_tibble(cbind(names(dag$nodes),node.scores))
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
    render_graph(output = "graph")
  return(result)#TODO return the graph as well 
}
