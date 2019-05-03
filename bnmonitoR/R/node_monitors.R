#' Node Monitors 
#' @param dag bnlearn object 
#' @param df dataset 

marg.node.monitor <- function(dag,df){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  dag.bn.fit <- bn.fit(dag, df)
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  querygrain(dag.grain, nodes=colnames(df), type="marginal") ->ev
  ev[match(names(df),names(ev))] %>%
    map2_dbl(.y=worst.level,standardize) -> final.z.score #this returns the vvery last marginal
  final.z.scores <- final.z.score[match(names(df),names(final.z.score))]
  return(final.z.scores)
}

#'@describeIn marg.node.monitor
marg.node.monitor.graph <- function(dag, df){#node.scores output from global.bn
  
  num.nodes <- length(nodes(dag))
  dag.bn.fit <- bn.fit(dag, df)
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  querygrain(dag.grain, nodes=colnames(df), type="marginal") ->ev
  ev[match(names(df),names(ev))] %>% #reordering to match 
    map2_dbl(.y=worst.level,standardize) -> final.z.score #this returns the vvery last marginal
  
  my.colors = brewer.pal(length(names(dag$nodes)),"Greens")
  max.val <- ceiling(max(abs(final.z.score)))
  my.palette <- colorRampPalette(my.colors)(max.val)
  node.colors <- my.palette[floor(abs(final.z.score))+1]
  nodes <- create_node_df(n=length(names(df)),
                          type= names(df),
                          label=names(df),
                          style="filled",
                          fontcolor="black",
                          fillcolor=node.colors)
  
  from.nodes <- map(dag$nodes, `[[`, "parents") %>% unlist %>% unname
  to.nodes <-map(dag$nodes, `[[`, "parents") %>% unlist %>% names %>% substr(1,1)
  
  edges <- create_edge_df(from=match(from.nodes,names(df)),
                          to=match(to.nodes,names(df)))
  create_graph(
    nodes_df = nodes,
    edges_df = edges) %>%
    render_graph(output = "graph",title="Marginal Node Monitors",layout='tree')
}

#'@describeIn marg.node.monitor
cond.node.monitor <- function(dag,df){
  dag.bn.fit <- bn.fit(dag, df[1:(dim(df)[1]-1),])
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  map(1:length(colnames(df)),pass.ev) %>% 
    map2(worst.level,standardize) %>% 
    unlist %>% unname-> z.scores
  names(z.scores) <- names(df)
  return(z.scores)
}

#'@describeIn marg.node.monitor
cond.node.monitor.graph <- function(dag, df){#node.scores output from global.bn
  
  dag.bn.fit <- bn.fit(dag, df[1:(dim(df)[1]-1),])
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  map(1:length(colnames(df)),pass.ev) %>% 
    map2(worst.level,standardize) %>% 
    unlist %>% unname-> z.scores
  names(z.scores) <- names(df)
  
  my.colors = brewer.pal(length(names(dag$nodes)),"Greens")
  max.val <- ceiling(max(abs(z.scores)))
  my.palette <- colorRampPalette(my.colors)(max.val)
  node.colors <- my.palette[floor(abs(z.scores))+1]
  nodes <- create_node_df(n=length(names(df)),
                          type= names(df),
                          label=names(df),
                          style="filled",
                          fontcolor="black",
                          fillcolor=node.colors)
  
  from.nodes <- map(dag$nodes, `[[`, "parents") %>% unlist %>% unname
  to.nodes <-map(dag$nodes, `[[`, "parents") %>% unlist %>% names %>% substr(1,1)
  
  edges <- create_edge_df(from=match(from.nodes,names(df)),
                          to=match(to.nodes,names(df)))
  create_graph(
    nodes_df = nodes,
    edges_df = edges) %>%
    render_graph(output = "graph",title="Conditional Node Monitors",layout='tree')
}

#'@describeIn marg.node.monitor
node.monitor.tbl <- function(dag, df){#node.scores output from global.bn
  num.nodes <- length(nodes(dag))
  dag.bn.fit <- bn.fit(dag, df[1:(dim(df)[1]-1),])
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df[dim(df)[1],])
  map(1:length(colnames(df)),pass.ev) %>% 
    map2(worst.level,standardize) %>% 
    unlist %>% unname-> cond.z.scores
  
  dag.bn.fit.marg <- bn.fit(dag, df)
  dag.grain.marg <- as.grain(dag.bn.fit.marg)
  querygrain(dag.grain.marg, nodes=colnames(df), type="marginal") ->ev
  ev[match(names(df),names(ev))] %>%
    map2_dbl(.y=worst.level,standardize) -> marg.z.score #this returns the vvery last marginal
  marg.z.scores <- marg.z.score[match(names(df),names(marg.z.score))]
  
  result <-as_tibble(cbind(names(df),as.numeric(marg.z.scores),as.numeric(cond.z.scores)))
  names(result) <- c('node','marg.z.score','cond.z.score')
  return(result)#TODO return the graph as well 
}
