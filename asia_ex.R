data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240
bnlearn::graphviz.plot(asia.dag)

asia.bn.fit <- bn.fit(asia.dag, asia)


#can use the global monitors to compare models
asia.dag.1 = model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E:S]") #this is the candidate model from pg 240
bnlearn::graphviz.plot(asia.dag.1) #has an additional dependence between S and D
global.monitor.tbl(asia.dag, alpha = 2, df=asia)
global.monitor.tbl(asia.dag.1, alpha = 2, df=asia)
#get a higher penalty from alternative model for D
global.monitor.graph(asia.dag,2,asia)
global.monitor.graph(asia.dag.1,2,asia)
#examine sequential contributions to log likelihood for D for each model
global.sqtl.1 <- global.monitor.bn.node.t(3,asia.dag.1,2,asia)
global.sqtl.0 <- global.monitor.bn.node.t(3,asia.dag,2,asia)

marg.B.0 <-marg.node.monitor(dag = asia.dag,df = asia,node.name = "D")
marg.B.1 <-marg.node.monitor(dag = asia.dag.1,df = asia,node.name = "D")

global.monitor.tbl(asia.dag,2,df=asia)
nodes(asia.dag)
#looks like smoking and bronchitis are main contributors 
#first for smoking
global.monitor.bn.node(node.idx = 6,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node.t(node.idx = 6,dag = asia.dag,alpha = 2,df = asia)
#no wild outliers for the data 
#and then for bronchitis 
global.monitor.bn.node.t(node.idx = 2,dag = asia.dag,alpha = 2,df = asia)
#ditto this one... 

#proceed to node monitors 
marg.B <- marg.node.monitor(asia.dag,asia,"B")
plot(marg.B)
#we're wildly overestimating bronchitis?
#getting z score approaching 3
marg.B.t <- marg.node.monitor.t(asia.dag,asia,"B")#says poop for NaNs
marg.S <- marg.node.monitor(asia.dag,asia,"S")
plot(marg.S)
#similarly misfiring on S

#smoking is a root node, so there's not much we can do there... 
#what about bronchitis?
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="B", pa.names = "S", pa.val = 'yes',alpha = 2)
#predicting pretty well for smokers
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="B", pa.names = "S", pa.val = 'no',alpha = 2)
#having a hard time modelling non-smokers
#underestimating the number of nonsmokers that have broonchitis in the dataset 
#so what do we do here? have a context-specific conditional independence?
#there is some other node that we need to accurately model bronchitis in the dataset

marg.S.t <- marg.node.monitor.t(asia.dag,asia,"S")



#this gives the overall log likelihood useful for comparing contribution of each node in models
global.monitor.bn.node(node.idx = 4,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node.t(node.idx = 4,dag = asia.dag,alpha = 2,df = asia)
global.monitor.bn.node(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)
test<-global.monitor.bn.node.t(node.idx = 8,dag = asia.dag,alpha = 2,df = asia)
test2 <- test
test2[-1] -> test2
plot(log(test2/test[-dim(df)[1]]))
#returns the global monitor for each node 


global.monitor.tbl(asia.dag, alpha = 2, df=asia)
global.monitor.graph(asia.dag, alpha = 2, df=asia)

asia.dag.model0 = model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E:S]") #this is the candidate model from pg 240
global.monitor.tbl(asia.dag.model0, alpha = 2, df=asia)
global.monitor.graph(asia.dag.model0, alpha = 2, df=asia)

df=asia; dag=asia.dag; node.name="T"; pa.names = "A"; pa.val = 'no'#why is this so different for learning and no learning??
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="T", pa.names = "A", pa.val = 'no',alpha=2)#why is this so different for learning and no learning??
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="T", pa.names = "A", pa.val = 'yes',alpha=2)
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="L", pa.names = "S", pa.val = 'yes',alpha=2)
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="L", pa.names = "S", pa.val = 'yes',alpha=2)


sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="L", pa.names = "S", pa.val = 'no',alpha=2)


sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="D", pa.names = c("B","E"), pa.val = c('yes','yes'))
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="D", pa.names = c("B","E"), pa.val = c('no','yes'))
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="D", pa.names = c("B","E"), pa.val = c('yes','no'))
sqntl.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="D", pa.names = c("B","E"), pa.val = c('no','no'))
dframe=asia; dag=asia.dag; node.name="D"; pa.names = c("B","E"); pa.val = c('no','no')



asia %>% filter(B=="no" & E=="no") %>% count(D)
asia[1:2200,] %>% filter(B=="no" & E=="no") %>% count(D)
asia[2000:5000,] %>% filter(B=="no" & E=="no") %>% count(D)

asia[2500:5000,] %>% count(B,E)

sqntl.pa.ch.monitor(df=asia, dag=asia.dag, node.name="X", pa.names = "E", pa.val = 'yes')
sqntl.pa.ch.monitor(df=asia, dag=asia.dag, node.name="X", pa.names = "E", pa.val = 'no')

df=asia; dag=asia.dag; node.name="T"; pa.names = "A"; pa.val = 'no';which.val=2#why is this so different for learning and no learning??

#try for a node with two parents 
#E, pa(E) = {T,L}

df=asia; dag=asia.dag; node.name="D"; pa.names = c("B","E"); pa.val = c('yes', 'no');which.val=2
sqntl.pa.ch.monitor(df=asia, dag=asia.dag, node.name="D", pa.names = c("B","E"), pa.val = c('yes', 'no'),which.val=2)
sqntl.pa.ch.monitor(df=asia, dag=asia.dag, node.name="E", pa.names = c("T","L"), pa.val = c('yes', 'yes'),which.val=2)
#NOTE THIS: which.val is which value for the CHILD node, not the parent

marg.node.monitor(asia.dag,asia[1:200,],"T")->test
cond.node.monitor(asia.dag,asia[1:200,],"T")->test
#NODE monitors 
node.monitor.tbl(dag = asia.dag,df = asia)

#final output table
T.node <- marg.node.monitor(asia.dag,asia,"T")
#grapical output 
marg.node.monitor.graph(asia.dag,asia)
cond.node.monitor.graph(asia.dag,asia)
#conditional ouptut table
test<-cond.node.monitor(asia.dag,asia[1:500,], "T")
#conditional node monitor graph
cond.node.monitor.graph(asia.dag,df=asia)






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


plot(actual.prop, actual.prop, main="title", sub="subtitle",
     xlab="X-axis label", ylab="y-axix label",
     xlim=c(min(theor.prop.learn),1), ylim=c(min(theor.prop.learn),1))
lines(actual.prop,theor.prop,col='blue')
lines(actual.prop,theor.prop.learn,col='red')
