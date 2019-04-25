library(bnlearn)

data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)

node.idx = 4;dag = asia.dag;alpha = 2;df = asia

global.monitor.bn.node(node.idx = 7,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)

#returns the global monitor for each node 

node.scores <- map_dbl(.x=1:length(asia.dag$nodes), dag=asia.dag, alpha=2, df=asia, .f= global.monitor.bn.node)
result <-as_tibble(cbind(names(asia.dag$nodes),node.scores))
result$node.scores <- as.numeric(result$node.scores)#return this result for each node 


library(RColorBrewer)
my.colors = brewer.pal(length(names(asia.dag$nodes)),"Blues")
max.val <- ceiling(max(abs(node.scores)))
my.palette <- colorRampPalette(my.colors)(max.val)

node.colors <- my.palette[floor(abs(node.scores))]

##########


###
# Create a graph
###

library(DiagrammeR)
nodes <-
  create_nodes(nodes = LETTERS,
               type = "letter",
               shape = sample(c("circle", "rectangle"),
                              length(LETTERS),
                              replace = TRUE),
               fillcolor = sample(c("aqua", "gray80",
                                    "pink", "lightgreen",
                                    "azure", "yellow"),
                                  length(LETTERS),
                                  replace = TRUE))

edges <-
  create_edges(from = sample(LETTERS, replace = TRUE),
               to = sample(LETTERS, replace = TRUE),
               rel = "letter_to_letter")

# Use the magrittr %>% operator between 'create_graph',
# and 'render_graph' calls
create_graph(nodes_df = nodes,
             edges_df = edges,
             graph_attrs = "layout = neato",
             node_attrs = c("fontname = Helvetica",
                            "style = filled"),
             edge_attrs = c("color = gray20",
                            "arrowsize = 0.5")) %>%
  render_graph

# Use the %>% operator between 'create_graph',
# 'render_graph', and 'cat' calls; the 'dot.gv' can be
# directly opened in Rstudio and further edited
create_graph(nodes_df = nodes,
             edges_df = edges,
             graph_attrs = "layout = neato",
             node_attrs = c("fontname = Helvetica",
                            "style = filled"),
             edge_attrs = c("color = gray20",
                            "arrowsize = 0.5")) %>%
  render_graph(output = "DOT") %>% cat(file = "~/dot.gv")

g <- graphviz.plot(asia.dag)
graph.par(list(nodes=list(fill=node.colors)))
graphviz.plot(g)
graph::nodeRenderInfo(g)
Rgraphviz::renderGraph(Rgraphviz::layoutGraph(bnlearn::as.graphNEL(g)))
graph::nodeRenderInfo(g)$fill  = node.colors
graphviz.plot(g)


g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(asia.dag))
graph::nodeRenderInfo(g) <- list(fill="EAF3FB")
Rgraphviz::renderGraph(g)
