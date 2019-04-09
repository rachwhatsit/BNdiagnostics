#node diagnostics  
library(DiagrammeR)
library(tidyverse)
library(gRain)
library(bnlearn)

df <- read.csv(file = "CHDS.latentexample1.csv")

hl <- c('High', 'Low')
ahl <- c('Average', 'High', 'Low')
ny <- c("No", "Yes")

#to get values diff variations of this; 
df %>% filter(Social == 'Low', Events=='Low') %>% count(Admission)

#how to functionize these levels t
s <- cptable(~Social, values=c(507,383),levels=hl)
e.s <-cptable(~Economic|Social, values = c(237, 270,46,337), levels=hl)
l.se <- cptable(~Events|Social+Economic, values=c(86,32,119,900,65,115,14,11,21,105,158,74), levels=ahl)
h.sl <- cptable(~Admission|Social+Events, values=c(146,30,73,24,213,21,87,32,125,44,77,18),levels=ny)

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
map2(1:4, c(1,1,1,1), abs_stand) #marginal node monitors for the CHDS 


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
  p <- marg[[1]][j] #pick the worst one
  sm <- -log(p)
  em <- - sum(marg[[1]]*log(marg[[1]]))
  vm <- sum(marg[[1]]*(log(marg[[1]])^2)) - em^2
  zm <- sm-em/sqrt(vm)
  return(zm)
}

#conditional node monitors 
cond_abs_stand(s,2)
cond_abs_stand(e,2)
cond_abs_stand(l,3)
cond_abs_stand(h,2)


cond_abs_stand(c(0.75,0.25),1)
cond_abs_stand(c(0.57,0.43),1)
cond_abs_stand(c(0.14,0.36,0.5),1)
cond_abs_stand(c(0.14,0.36,0.5),2)
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

marg_monitor(c(.57,.43),2)
marg_monitor(c(.57,.43),2)
.57*.47+.43*.12
.57*.53+.43*.88
marg_monitor(c(.3195,.6805),1)
marg_monitor(c(.3195,.6805),2)

xl <- c(.57*.47*.50 + .57*.53*.43 + .43*.12*.45 + .43*.88*.22,
.57*.47*.36 + .57*.53*.33 + .43*.12*.31 + .43*.88*.31,
.57*.47*.14 + .57*.53*.24 + .43*.12*.24 + .43*.88*.47)#p(Xl = Low )

marg_monitor(xl,1)

.57*.47*.14*.25 + .57*.53*.24*.25 +.57*.47*.36*.17 + .57*.53*.33*.17+
.57*.47*.50*.09 + .57*.53*.33*.09+
.43*.47*.24*.26 + .43*.53*.47*.26+
,43*.47*.31*.27 + .43*.53*.31*.27+
.43*.47*.45*.19 + .43*.53*.22*.19 -> pyes

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

