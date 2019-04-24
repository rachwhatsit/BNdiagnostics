library(bnlearn)

data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)

node.idx = 4;dag = asia.dag;alpha = 2;df = asia

global.monitor.bn.node(node.idx = 7,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)

#returns the global monitor for each node 

map_dbl(.x=1:length(asia.dag$nodes), dag=asia.dag, alpha=2, df=asia, .f= global.monitor.bn.node)
