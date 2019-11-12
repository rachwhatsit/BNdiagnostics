library(bnlearn)
library(tidyverse)
library(rlang)
library(purrr)

setwd("C:/Users/rachel/Documents/BNdiagnostics/")
df <- read.csv("CHDS.latentexample1.csv")
chds.dag = empty.graph(nodes = c("Social", "Economic", "Events", "Admission"))
arc.set = matrix(c("Social", "Economic",
                   "Social", "Events",
                   "Social", "Admission",
                   "Economic", "Events",
                   "Events", "Admission"),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("from", "to")))
arcs(chds.dag) = arc.set

chds.elicited = bn.fit(chds.dag, df)#the elicited dag
skeleton(chds.dag)
moral(chds.dag)
graphviz.plot(chds.dag)

#find a BN from the chds data
chds.bn.fit = gs(df)
chds.gs = bn.fit(chds.bn.fit, df)
#new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1), #adjust the CPT
#                 byrow = TRUE, ncol = 3,
#                 dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
#fitted$B = as.table(new.cpt)
graphviz.plot(chds.gs)


#worked example of the monitors from an elicited example

df = df; dag = chds.dag; alpha = 3

num.nodes <- length(dag$nodes)

num.ch <- map(dag$nodes, `[[`, "children")  %>% map(length)
num.pa <- map(dag$nodes, `[[`, "parents") %>% map_int(length)
chds.pa.val <- map(dag$nodes, `[[`, "parents") 
num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})

get.num.pa.combo <- function(pa.val){
  if(is_empty(pa.val)==TRUE){
    num.pa.combo <- 1
  } else {
    num.pa.combo <- prod(num.values[which(colnames(df) %in% pa.val)])  
  }
    return(num.pa.combo)
}


get.pa.combo.score <- function(k, counts.vec, alpha.vec){
  c.vec <- counts.vec[(((k-1)*length(alpha.vec))+1): (((k-1)*length(alpha.vec))+length(alpha.vec))]
  lgamma(sum(alpha.vec)) - lgamma(sum(alpha.vec + c.vec)) + sum(lgamma(alpha.vec + c.vec)) - sum(lgamma(alpha.vec))
}

chds.num.pa.combo <- map_dbl(chds.pa.val, get.num.pa.combo)#example of the node in action 
num.pa.combo <- chds.num.pa.combo #for the example

#code the global monitor for a chunk of the combos
#works for elicited graph structure 
global.monitor.bn.node <- function(i,num.pa.combo,alpha){#j is the index of the parent set
  alpha.vec <- rep(alpha/num.values[i], num.values[i])
  pa.names <- c(dag$nodes[colnames(df)[i]][[1]][[3]], names(dag$nodes[colnames(df)[i]]))
  df %>% count(!!!(syms(pa.names))) %>% complete #%>% pull(n) -> counts.vec
  map_dbl(1:num.pa.combo[i], ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec
  score <- sum(scores.vec)
  return(list(score,scores.vec))#returns global and pach monitor  
}  

global.monitor.bn.node(3, chds.num.pa.combo,alpha=3)
global.monitor.bn.node(4, chds.num.pa.combo,alpha=2)
global.monitor.bn.node(1, chds.num.pa.combo,alpha=2)

map(1:num.nodes,~global.monitor.bn.node(.x,num.pa.combo,alpha=3))#global monitor

#think of how to visualize this in shiny 

#worked example with a model of the class BN fit
dag.gs <- chds.gs
chds.gs.nodes <-nodes(chds.gs)

num.ch.gs <- map(chds.gs, `[[`, "children")  %>% map_int(length)
num.pa.gs <-map(chds.gs, `[[`, "parents")  %>% map_int(length)
chds.gs.pa.val <- map(chds.gs, `[[`, "parents")   


num.values <- map_int(1:num.nodes, function(i){length(unique(df[,i]))})
chds.gs.num.pa.combo <- map_dbl(chds.gs.pa.val, get.num.pa.combo)#example of the node in action 

dag <- chds.gs; num.pa.combo <- chds.gs.num.pa.combo #for the example


#global and pach node monitors for things from the gs algorithm 
global.monitor.bn.node.gs <- function(i,num.pa.combo,alpha,df){#j is the index of the parent set
  alpha.vec <<- rep(alpha/num.values[i], num.values[i])
  pa.names <-c(unlist(chds.gs.pa.val[i] ), colnames(df)[i])
  df %>% count(!!!(syms(pa.names))) %>% complete %>% pull(n) ->> counts.vec
  map_dbl(1:num.pa.combo[i], ~get.pa.combo.score(.x, counts.vec, alpha.vec))->scores.vec #TODO misfiring because of global variables
  score <- unlist(sum(scores.vec))
  return(list(scores.vec))#returns global and pach monitor  
}  

global.monitor.bn.node(2, chds.num.pa.combo,alpha=3)#as a comparison for the elicited model
global.monitor.bn.node.gs(1, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(2, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(3, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model
global.monitor.bn.node.gs(4, chds.gs.num.pa.combo,alpha=3,df)#the grow shrink algorithm model


map(1:num.nodes,~global.monitor.bn.node.gs(.x,chds.gs.num.pa.combo,alpha=3))#global monitor

pa.ch.bn.monitor <- function(node.idx, num.pa.combo, pa.combo.idx, df, alpha){
  z <- rep(0,dim(df)[1])
  for(i in 1:dim(df)[1]){
    df.cut <- df[1:i,]
    global.monitor.bn.node.gs(i = node.idx,num.pa.combo = num.pa.combo, df = df.cut, alpha = alpha)[[1]][[pa.combo.idx]] -> z[i]
  }
  return(z)
}

node.idx=3; num.pa.combo=chds.gs.num.pa.combo; pa.combo.idx = 3; df = df;alpha = 3
pa.ch.bn.monitor(node.idx=3, num.pa.combo=chds.gs.num.pa.combo, pa.combo.idx = 3, df = df,alpha = 3)->test


df=df; node.name ="Admission" ;pa.names = c("Social", "EVents");pa.val = c("Low","High"); which.val=1

bn.pach <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Admission", pa.names = c("Social","Events"),pa.val = c("Low","High")) 
bn.pach.x <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Economic", pa.names = "Social",pa.val = "High") 
bn.pach2 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Economic", pa.names = "Social",pa.val = "Low") 

bn.pache1 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Events", pa.names = c("Social", "Economic"),pa.val = c("Low", "Low")) 
bn.pache2 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Events", pa.names = c("Social", "Economic"),pa.val = c("Low", "High")) 
bn.pache3 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Events", pa.names = c("Social", "Economic"),pa.val = c("High", "Low")) 
bn.pache4 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Events", pa.names = c("Social", "Economic"),pa.val = c("High", "High")) 
#node diagnostics  
t <- 1:length(bn.pache1)
as.data.frame(cbind(t,bn.pache1,bn.pache2,bn.pache3,bn.pache4)) %>%
  reshape2::melt(id='t') %>%
  ggplot(aes(x=t, y=value, colour=variable)) + geom_point() + xlab('Relevant sample size') + ylab('Standardized Z Statistic')  +theme_minimal(base_size = 22) +
  ggtitle("Parent-Child Monitor for Life Events") + #TODO fix the label here1:length(chds.ahc.pach31)
ggsave("C:/Users/rachel/Documents/diagpaper/ba/img/chdsBNevents.jpeg")

bn.pacheh1 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Admission", pa.names = c("Social", "Events"),pa.val = c("Low", "Low")) 
bn.pacheh2 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Admission", pa.names = c("Social", "Events"),pa.val = c("Low", "High")) 
bn.pacheh3 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Admission", pa.names = c("Social", "Events"),pa.val = c("High", "Low")) 
bn.pacheh4 <- seq.pa.ch.monitor(df = df, dag = chds.dag, node.name  = "Admission", pa.names = c("Social", "Events"),pa.val = c("High", "High")) 
#node diagnostics  
t <- 1:length(bn.pache1)
as.data.frame(cbind(t,bn.pacheh1,bn.pacheh2,bn.pacheh3,bn.pacheh4)) %>%
  reshape2::melt(id='t') %>%
  ggplot(aes(x=t, y=value, colour=variable)) + geom_point() + xlab('Relevant sample size') + ylab('Standardized Z Statistic')  +theme_minimal(base_size = 22) +
  ggtitle("Parent-Child Monitor for Admission") + #TODO fix the label here
  scale_colour_discrete(name="", labels=c( "Xs=Low, Xe=Low", "Xs=Low, Xe=High","Xs=High, Xe=Low","Xs=High, Xe=High")) + theme(legend.position="right")
ggsave("C:/Users/rachel/Documents/diagpaper/ba/img/chdsBNpachH.jpeg")







hl <- c('High', 'Low')
ahl <- c('Average', 'High', 'Low')
ny <- c("No", "Yes")

#to get values diff variations of this; 
df %>% filter(Social == 'Low', Events=='Low') %>% count(Admission)

#how to functionize these levels t
#THESE ARE THE M-1 COUNTS FOR THE DATA, LEAVING OUT THE LAST OBSERATIO 890 HIGH HIGH YES 
s <- cptable(~Social, values=c(506,383),levels=hl)
e.s <-cptable(~Economic|Social, values = c(236, 270,45,337), levels=hl)
l.se <- cptable(~Events|Social+Economic, values=c(86,31,119,900,64,115,14,10,21,105,157,74), levels=ahl)
h.sl <- cptable(~Admission|Social+Events, values=c(146,29,73,23,213,20,87,31,125,43,77,17),levels=ny)

plist <- compileCPT(list(s, e.s, l.se, h.sl))
plist#check the network out

net1 <- grain(plist)

marg <- querygrain(net1, nodes=c("Social", "Economic","Events","Admission"), type="marginal")

abs_stand <- function(i,j){ #returns z score for the ith node with the jth worst level
  p <- marg[[i]][j] #pick the worst one
  sm <- -log(p)
  em <- - sum(marg[[i]]*log(marg[[i]]))
  vm <- sum(marg[[i]]*(log(marg[[i]])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}

map2(1:4, c(2,2,3,2), abs_stand) #marginal node monitors for the CHDS 
map2(1:4, c(1,1,2,2), abs_stand) #marginal node monitors for the CHDS 


net1t <- setEvidence(net1,evidence=list(Social='High',Economic='High',Events='High',Admission='Yes'))
log(pEvidence(net1t))
s <- querygrain(net1s, nodes=c('Social'))

net1s <- setEvidence(net1,evidence=list(Economic='High',Events='High',Admission='Yes'))
s <- querygrain(net1s, nodes=c('Social'))
net1e <- setEvidence(net1,evidence=list(Social='High',Events='High',Admission='Yes'))
e <- querygrain(net1e, nodes=c('Economic'))
net1l <- setEvidence(net1,evidence=list(Social='High',Economic='High',Admission='Yes'))
l <- querygrain(net1l, nodes=c('Events'))
net1h <- setEvidence(net1,evidence=list(Social='High',Economic='High',Events='High'))
h <- querygrain(net1h, nodes=c('Admission'))

cond_abs_stand <- function(marg,j){ #returns z score for the ith node with the jth worst level
  p <- marg[j] #pick the worst one
  sm <- -log(p)
  em <- - sum(marg[[1]]*log(marg[[1]]))
  vm <- sum(marg[[1]]*(log(marg[[1]])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}

#conditional node monitors 
cond_abs_stand(s,1)
cond_abs_stand(e,1)
cond_abs_stand(l,2)
cond_abs_stand(h,2)


#taking the CPTs from the tree 
cond_abs_stand(c(0.57,0.43),1)
cond_abs_stand(c(0.47,0.53),1)
cond_abs_stand(c(0.2,0.34,0.46),2)
cond_abs_stand(c(0.74,0.26),2)

cond_abs_stand(c(0.14,0.36,0.5),3)

#how can we functionize this?

##UNCONDITIONAL CUT MONITORS FOR THE CEG 
standardize <- function(vec,j){ #returns z score for the ith node with the jth worst level
  p <- vec[j] #pick the worst one
  sm <- -log(p)
  em <- - sum(vec[j]*log(vec[j]))
  vm <- sum(vec[j]*(log(vec[j])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}

marg_monitor(c(.57,.43),1)
marg_monitor(c(.57,.43),2)
xe <-c(.57*.47+.43*.12,
.57*.53+.43*.88)
marg_monitor(xe,1)
marg_monitor(c(.3195,.6805),1)
marg_monitor(c(.3195,.6805),2)

xl <- c(.57*.47*.50 + .57*.53*.43 + .43*.12*.45 + .43*.88*.22,#LOW
        .57*.47*.36 + .57*.53*.33 + .43*.12*.31 + .43*.88*.31,#MEDIUM
        .57*.47*.14 + .57*.53*.24 + .43*.12*.24 + .43*.88*.47)#HIGH

marg_monitor(xl,3)

  .57*.47*.50*.25 + .57*.53*.43*.25 +
  .57*.47*.36*.17 + .57*.53*.33*.17+
  .57*.47*.14*.09 + .57*.53*.24*.09+
  .43*.47*.45*.26 + .43*.53*.22*.26+
  .43*.47*.31*.27 + .43*.53*.31*.27+
  .43*.47*.24*.19 + .43*.53*.47*.19 -> pyes

xh <-c(1-pyes,pyes)
marg_monitor(xh,2)

#CONDITIONAL NODE MONITORS FOR THE CEG 

#p(X_s = High |Xe = High, H_l = High, X_h = Yes)
Xs_sm <- .57*.47*.14*.25/(.57*.47*.14*.25 + .43*.47*.14*.25)
Xs_cond <- c(Xs_sm, 1-Xs_sm)

#p(X_e = High |Xs = High, H_l = High, X_h = Yes)
Xe_sm <- .57*.47*.14*.25/(.57*.47*.14*.25 + .57*.53*.14*.25)
Xe_cond <- c(Xe_sm, 1-Xe_sm)

#p(X_l = High |Xs = High, X_e = High, X_h = Yes)
Xl_sm1 <- .57*.47*.14*.25/(.57*.47*.14*.25 + .57*.47*.36*.25 + .57*.47*.5*.25 )
Xl_sm2 <- .57*.47*.36*.25/(.57*.47*.14*.25 + .57*.47*.36*.25 + .57*.47*.5*.25 )
Xl_sm3 <- .57*.47*.5*.25/(.57*.47*.14*.25 + .57*.47*.36*.25 + .57*.47*.5*.25 )
Xl_cond <- c(Xl_sm1, Xl_sm2,Xl_sm3)

#p(X_s = High |Xe = High, H_l = High, X_h = Yes)
X_h_sm <- .57*.47*.14*.25/(.57*.47*.14*.25 + .57*.47*.14*.75)
Xh_cond <- c(X_h_sm, 1-X_h_sm)

cond_abs_stand <- function(marg,j){ #returns z score for the ith node with the jth worst level
  p <- marg[j] #pick the worst one
  sm <- -log(p)
  em <- - sum(marg[[1]]*log(marg[[1]]))
  vm <- sum(marg[[1]]*(log(marg[[1]])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}

cond_abs_stand(Xs_cond, 2)
cond_abs_stand(Xe_cond, 2)
cond_abs_stand(Xl_cond, 3)
cond_abs_stand(Xh_cond, 2)

###compile with gRain 
library(gRain)
chds.junction = compile(as.grain(chds.gs))#convert to a gRain object 
querygrain(chds.junction, nodes=c("Social","Economic","Events","Admission"), type="marginal")

chds.ev <- setEvidence(chds.junction,nodes="Social",states="High")
querygrain(chds.ev, nodes=c("Economic","Events"),type="joint")
querygrain(chds.ev, nodes=c("Economic"),type="joint") %>% marg_monitor(1)#j corresponds to the 
querygrain(chds.ev, nodes=c("Economic","Events"),type="marginal")
chds.ev <- setEvidence(chds.junction,nodes=c("Social", "Economic","Events","Admission"),states=c("High", "High","Average","No"))
querygrain(chds.ev, nodes=c("Economic","Events"),type="joint")
querygrain(chds.ev, nodes=c("Economic","Events"),type="marginal")


chds.gs -> dag; df -> df; obs.vec <- rep(1,4)
marg.node.monitor <- function(dag,df,obs.vec){#returns the mth node monitor 
  dag.junction = compile(as.grain(dag))#convert to a gRain object 
  querygrain(dag.junction, nodes=colnames(df), type="marginal") %>%
    map2(obs.vec,standardize) -> z.score
  return(z.score)
}

marg.node.monitor(chds.gs,df,obs.vec)

cond.node.monitor <- function(dag,df,obs.vec){
  dag.junction = compile(as.grain(dag))#convert to a gRain object 
  map(1:length(colnames(df)),pass.ev) %>% 
    map2(obs.vec,standardize)-> z.score
  return(z.score)
}

cond.node.monitor(chds.gs,df,obs.vec)

pass.ev <-function(i){
  ev <- querygrain(setEvidence(net1,evidence=list(df[dim(df)[1],-i]), nodes=colnames(df)[i]))
  evi <- ev[[i]]
  return(evi)
}

z.s <- marg.node.monitor(chds.dag,df,"Social")
z.e <- marg.node.monitor(chds.dag,df,"Economic")
z.l <- marg.node.monitor(chds.dag,df,"Events")
z.h <- marg.node.monitor(chds.dag,df,"Admission")


plot(z.e)
plot(z.l)
plot(z.h)

z.s.cond <- cond.node.monitor(chds.dag,df,"Social")
head(z.s.cond)
z.e.cond <- cond.node.monitor(chds.dag,df,"Economic")
z.l.cond <- cond.node.monitor(chds.dag,df,"Events")
z.h.cond <- cond.node.monitor(chds.dag,df,"Admission")

t <- 1:length(z.l.cond[,1])
as.data.frame(cbind(t,z.l.cond[,1],z.l.cond[,2])) %>%
  reshape2::melt(id='t') %>%
  ggplot(aes(x=t, y=value, colour=variable)) + geom_point() + xlab('Relevant sample size') + ylab('Standardized Z Statistic')  +theme_minimal(base_size = 22) +
  ggtitle("Node Monitors for Life Events") + #TODO fix the label here
  scale_colour_discrete(name="", labels=c( "Marginal", "Conditional")) + theme(legend.position="right")
ggsave("C:/Users/rachel/Documents/diagpaper/ba/img/nodeBNevents.jpeg")

t <- 1:length(z.l.cond[,1])
as.data.frame(cbind(t,z.h.cond[,1],z.h.cond[,2])) %>%
  reshape2::melt(id='t') %>%
  ggplot(aes(x=t, y=value, colour=variable)) + geom_point() + xlab('Relevant sample size') + ylab('Standardized Z Statistic')  +theme_minimal(base_size = 22) +
  ggtitle("Node Monitors for Admission") + #TODO fix the label here
  scale_colour_discrete(name="", labels=c( "Marginal", "Conditional")) + theme(legend.position="right")
ggsave("C:/Users/rachel/Documents/diagpaper/ba/img/nodeBNh.jpeg")

