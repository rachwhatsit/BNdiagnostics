library(bnlearn)

data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
bnlearn::graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)

node.idx = 4;dag = asia.dag;alpha = 2;df = asia

global.monitor.bn.node(node.idx = 7,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)

#returns the global monitor for each node 

node.scores <- map_dbl(.x=1:length(asia.dag$nodes), dag=asia.dag, alpha=2, df=asia, .f= global.monitor.bn.node)
result <-as_tibble(cbind(names(asia.dag$nodes),node.scores))
result$node.scores <- as.numeric(result$node.scores)#return this result for each node 


global.monitor.graph(asia.dag, alpha = 2, df=asia)

seq.pa.ch.monitor(df=asia, dag=asia.dag, node.idx=7, pa.names = "A", pa.val = 'yes',which.val=2)#why is this so different??

#try for a node with two parents 
#E, pa(E) = {T,L}

df=asia; dag=asia.dag; node.idx=4; pa.names = c("T","L"); pa.val = c('no', 'no');which.val=1
seq.pa.ch.monitor(df=asia, dag=asia.dag, node.idx=4, pa.names = c("T","L"), pa.val = c('no', 'no'),which.val=1)
#NOTE THIS: which.val is which value for the CHILD node, not the parent
