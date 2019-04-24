library(bnlearn)

data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)

node.idx = 3;dag = asia.dag;alpha = 2;df = asia

global.monitor.bn.node(node.idx = 3,dag = asia.dag,alpha = 2,df = asia)
