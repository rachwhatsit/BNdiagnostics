
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
bnlearn::graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)

node.idx = 4;dag = asia.dag;alpha = 2;df = asia

global.monitor.bn.node(node.idx = 7,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)

#returns the global monitor for each node 


global.monitor.tbl(asia.dag, alpha = 2, df=asia)
global.monitor.graph(asia.dag, alpha = 2, df=asia)

asia.dag.model0 = model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E:S]") #this is the candidate model from pg 240
global.monitor.tbl(asia.dag.model0, alpha = 2, df=asia)
global.monitor.graph(asia.dag.model0, alpha = 2, df=asia)

seq.pa.ch.monitor(df=asia, dag=asia.dag, node.name="T", pa.names = "A", pa.val = 'yes',which.val=2)#why is this so different for learning and no learning??

#try for a node with two parents 
#E, pa(E) = {T,L}

df=asia; dag=asia.dag; node.idx=4; pa.names = c("T","L"); pa.val = c('no', 'no');which.val=1
seq.pa.ch.monitor(df=asia, dag=asia.dag, node.idx=4, pa.names = c("T","L"), pa.val = c('no', 'no'),which.val=1)
#NOTE THIS: which.val is which value for the CHILD node, not the parent


#NODE monitors 
node.monitor.tbl(asia.dag,asia)

#final output table
marg.node.monitor(asia.dag,asia)
#grapical output 
marg.node.monitor.graph(asia.dag,asia)

#conditional ouptut table
cond.node.monitor(asia.dag,asia)
#conditional node monitor graph
cond.node.monitor.graph(asia.dag,asia)






test.dag = model2network("[A][B][C][H][D|A:H][F|B:C][E|B:D][G|A:D:E:F]")
graphviz.plot(test.dag)


#DETRITUS 


marg.node.monitor.t <- function(dag,df, t){#returns the mth node monitor 
  num.nodes <- length(nodes(dag))
  df[1:t,] -> df.cut
  dag.bn.fit <- bn.fit(dag, df.cut)
  dag.grain <- as.grain(dag.bn.fit)
  worst.level <- as.numeric(df.cut[dim(df.cut)[1],])
  querygrain(dag.grain, nodes=colnames(df), type="marginal") ->ev
  ev[match(names(df),names(ev))] %>%
    map2_dbl(.y=worst.level,standardize) -> z.score #this returns the vvery last marginal
  return(z.score)
}

marg.node.monitor.t(asia.dag,asia,10)
map(3000:3010, ~marg.node.monitor.t(.x, dag=asia.dag,df=asia))
my_df <- tibble(old_col = 3000:4000)


my_df_2 <- my_df %>%
  mutate(new_col = m)

my_df_2


map(c(3000:4000), marg.node.monitor.t(asia.dag,asia, t=.x))
map(c(3000:4000),function(x) marg.node.monitor(asia.dag,asia,x))
pmap(list(3000:4000,asia.dag,asia),marg.node.monitor.t)


