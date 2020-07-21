install.packages("stargazer") 
library(stargazer)
PNP <- subset(PNPS, Condition != "Singleton")
PNP$ConditionDummy <- ifelse(PNP$Condition == "Patent",1,0)

PNPC <- subset(PNP, farsearch == 0)
library(data.table)
PNP <- as.data.table(PNP)
table(PNP$farsearch)
PNP4$ConditionDummy <- ifelse(PNP4$Condition == "Patent",1,0)
PNP4C$ConditionDummy <- ifelse(PNP4C$Condition == "Patent",1,0)

table(PNP4$farsearch)
table(PNP$ConditionDumm,PNP$Condition)
PNPI <- PNP4C[,.(search=mean(search)),by=.(ID,ConditionDummy,open,C)]
m1 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy , data = subset(PNP4,farsearch == 0))
summary(m3)
m2 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy + C*open, data = subset(PNP4,farsearch == 0))
summary(m1)
m3 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy+ C*open + C*ConditionDummy, data = subset(PNP4,farsearch == 0))
summary(m2)
m4 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy+  C*ConditionDummy , data = subset(PNP4,farsearch == 0))

#This is the main specification:

stargazer(m1,m2,m3,m4,type="html",
          dep.var.labels=c("search rate"),
          covariate.labels=c("Searching cost","Sequential discovery (sequential=1)","Condition (patent=1)", "Sequential*Condition" , "Cost*Sequential","Cost*Condition"),out="models6.doc")


stargazer(m3,m2,m1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models6.doc")
table(PNP$gender,PNP$Condition)

m15 <- glm(search ~ C + open + ConditionDummy + open*ConditionDummy + gender + factor(ID) , data = PNPI,family = "binomial")
m16 <- glm(search ~ C + open + ConditionDummy + open*ConditionDummy + gender + factor(ID) , data = PNP,family = "binomial")
m17 <- glm(search ~ C + open + ConditionDummy + open*ConditionDummy +  factor(ID) , data = PNP,family = "binomial")
table(PNP$gender,PNP$Condition)
summary(m16)

stargazer( m19,m18,m17,m16,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)","gender","age"),out="models9.doc")
summary(m16)
?lmer
m18 <- glm(search ~  C+ ConditionDummy + open + open*ConditionDummy + factor(ID)    , data = PNP4C,family = "binomial")
m19 <- lm(search ~ HighCost + ConditionDummy + open + HighCost*ConditionDummy*open, data = PNPI)
summary(m19)
PNPI$HighCost <- ifelse(PNPI$C>20,1,0)
m20 <- lm(search ~ C + open  + gender + factor(ID) , data = PNPI)

m2 <- lm(search ~ C + open + ConditionDummy + C*ConditionDummy + C*open + open*ConditionDummy + age + gender, data = PNPI)
m3 <- lm(search ~ C + open + ConditionDummy + gender, data = PNPI)
m4 <- lm(search ~ C + open + ConditionDummy + C*ConditionDummy + open*ConditionDummy + gender, data = PNPI)
m5 <- lm(search ~ C + open + ConditionDummy + C*ConditionDummy + C*open + open*ConditionDummy + gender, data = PNPI)
m6 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy, data = PNPI)
stargazer(m2, m3, m4, m5, m6, type="html",
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)",
                             "age","gender (male=1)"), out="models1.doc")

summary(m2)
anova(m1,m2)
m6 <- lm(search ~ C + open + ConditionDummy + C*ConditionDummy + C*open + open*ConditionDummy + gender + factor(ID), data = PNPC)
summary(m6)
m7 <- lm(Payoff ~ C + open + ConditionDummy + open * ConditionDummy, data = PNP4)
summary(m7)
m8 <- lm(Payoff ~ C + open + ConditionDummy + open * ConditionDummy , data = PNP4)
m9 <- lm(Payoff ~ C + open + ConditionDummy + C*ConditionDummy + C*open + open*ConditionDummy + age + gender, data = PNPC)
m10 <- lm(Payoff ~ C + open + ConditionDummy + C*ConditionDummy + C*open + open*ConditionDummy, data = PNPC)
stargazer(m7,m8,m9, m10, type="html",
          dep.var.labels=c("payoff"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)",
                             "age","gender (male=1)"), out="models2.doc")

m11 <- lm(Threshold ~ open + factor(Condition) -1, data = Threshold)

m12 <- lm(Threshold ~ open  + factor(Condition) + open*factor(Condition) -1, data = Threshold)

stargazer(m11, m12, type="html",
          dep.var.labels=c("Threshold"),
          out="models3.doc")
Threshold3 <- subset(Threshold,Condition != "Singleton")
m11 <- lm(Threshold ~ open + conditionDummy + open*conditionDummy, data = Threshold3)
summary(m11)
m12 <-  lm(Threshold ~ open + conditionDummy + open*conditionDummy + gender + max, data = Threshold3)

stargazer(m11, m12, type="html",
          dep.var.labels=c("Threshold"),
          covariate.labels=c("open discovery (open=1)","Condition (patent=1)"),
          out="models4.doc")
PNPIG <- PNP[,.(search=mean(search)),by=.(ID,ConditionDummy,open,gender,age,C,G)]

m13 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy + G, data = PNPIG)
m14 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy + G + gender + age, data = PNPIG)

summary(m13)
stargazer(m13, m14, type="html",
          dep.var.labels=c("search rate"),
          out="models7.doc")

#exploration rate without far search
PNPIC <- PNPC[,.(search=mean(search)),by=.(ID,ConditionDummy,open,gender,age,C)]

m3 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy, data = PNPIC)
summary(m3)
m1 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy + factor(ID), data = PNPC)
summary(m1)
m2 <- lm(search ~ C + open + ConditionDummy + open*ConditionDummy, data = PNPC)
summary(m2)

stargazer(m3,m2,m1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate no far search"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models8.doc")

table(PNP$farsearch)

#finding the best specification:

l1 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + factor(ID), data = PNP)
l2 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + factor(ID), data = PNP)
l3 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + C*ConditionDummy + factor(ID), data = PNP)
l4 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*ConditionDummy + factor(ID), data = PNP)

stargazer(l4,l3,l2,l1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models10.doc")

PNP4 <- subset(PNPS4,Condition != "Singleton")
PNP4$ConditionDummy <- ifelse(PNP4$Condition == "Patent",1,0)

n1 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + factor(ID), data = PNP4)
n2 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + factor(ID), data = PNP4)
n3 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + C*ConditionDummy + factor(ID), data = PNP4)
n4 <- lm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*ConditionDummy + factor(ID), data = PNP4)

stargazer(n4,n3,n2,n1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models11.doc")

l1 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + factor(ID), data = PNP,family = "binomial")
l2 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + factor(ID), data = PNP,family = "binomial")
l3 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + C*ConditionDummy + factor(ID), data = PNP,family = "binomial")
l4 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*ConditionDummy + factor(ID), data = PNP,family = "binomial")

stargazer(l4,l3,l2,l1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models12.doc")

PNP4 <- subset(PNPS4,Condition != "Singleton")
PNP4$ConditionDummy <- ifelse(PNP4$Condition == "Patent",1,0)

#try to see if clustering the ID changes something - it's not!
install.packages(multiwayvcov)
library(multiwayvcov)
h1 <- lm(search ~  C  + ConditionDummy , data = PNP4)
h2 <- miceadds::lm.cluster( data=PNP4, formula= search ~  C  + ConditionDummy,
                                    cluster="ID" )
summary(h1)
library(plm)
library(lmtest)
pm1 <- plm(search ~  C  + ConditionDummy, data = PNP4, model = "pooling")
# compute Stata like df-adjustment
G <- length(unique(PNP4$ID))
N <- length(PNP4$ID)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
firm_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = firm_c_vcov)

coef(h2)
vcov(h2)
summary(mod1)

n3 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*open + C*ConditionDummy + factor(ID), data = PNP4,family = "binomial")
n4 <- glm(search ~  C + open + ConditionDummy + open*ConditionDummy + C*ConditionDummy + factor(ID), data = PNP4,family = "binomial")

stargazer(n4,n3,n2,n1,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("search rate"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models13.doc")
library(stargazer)
n1 <- glm(search ~  C + open + ConditionDummy  , data = PNP4, family = "binomial")
summary(n1)
m6 <- lm(Payoff ~ C + open + ConditionDummy + open*ConditionDummy + factor(ID), data = PNP4)
summary(m6)
m7 <- lm(Payoff ~ C + open + ConditionDummy + open*ConditionDummy + C*open + factor(ID), data = PNP4)
summary(m7)
m8 <- lm(Payoff ~  C + open + ConditionDummy + open*ConditionDummy + C*open + C*ConditionDummy + factor(ID) , data = PNP4)
m9 <- lm(Payoff ~ C + open + ConditionDummy + open*ConditionDummy + C*ConditionDummy + factor(ID), data = PNP4)
stargazer(m9,m8,m7,m6,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("Payoff"),
          covariate.labels=c("searching cost","open discovery (open=1)","Condition (patent=1)"),out="models1.doc")

Threshold2 <- subset(Threshold1,Condition != "Singletone")
Threshold2$ConditionDummy <- ifelse(Threshold2$Condition =="Patent",1,0)

l6 <- lm(Threshold ~ open + ConditionDummy + open*ConditionDummy, data = Threshold2)
summary(l6)

stargazer(l6,type="html",
          omit = c("ID"),
          omit.labels = c("ID FE?"),
          dep.var.labels=c("Threshold- no weak threshold holders"),
          out="models2.doc")

#split the data to open and closed cases, and the cost to high and low.

PNP4$CostDummy <- ifelse(PNP4$C>=20,1,0)
PNP4open <- subset(PNP4,open == 1)
PNP4closed <- subset(PNP4, open == 0)
lm1 <- lm(search ~ CostDummy + ConditionDummy + CostDummy*ConditionDummy, data = PNP4closed)
summary(lm1)
lm2 <- lm(search ~ CostDummy + ConditionDummy + CostDummy*ConditionDummy, data = PNP4open)
summary(lm2)
stargazer(lm1,lm2,type="html",
          
          dep.var.labels=c("search rate- no weak threshold holders"),
          covariate.labels=c("Cost (high=1)","Condition (patent=1)","Cost*Condition"),
          out="models4.doc")
pm1 <- plm(search ~ CostDummy + ConditionDummy + CostDummy*ConditionDummy, data = PNP4closed)
# compute Stata like df-adjustment
G <- length(unique(PNP4closed$ID))
N <- length(PNP4closed$ID)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
firm_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = firm_c_vcov)

coef(h2)

#FE by round & game
PNP4$GR <- 50*(PNP4$G-1)+PNP4$R
n1 <- lm(search ~  C + ConditionDummy + factor(GR), data = PNP4)
n2 <- lm(search ~  C + ConditionDummy + C*ConditionDummy + factor(GR), data = PNP4)

stargazer(model,type="html",
         dep.var.labels=c("search rate"),
          out="model1.doc")

#try random effect
open <- subset(PNP4C,open == 1)
closed <- subset(PNP4C,open == 0)
HighCost <- subset(PNP4C, highCost == 1)
open.highcost <- subset(PNP4C, highCost == 1& open == 1)
open.lowcost <- subset(PNP4C, highCost == 0& open == 1)

PNP4C$highCost <- ifelse(PNP4C$C >= 20,1,0)
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
install.packages("lmerTest")
model = lmer(search ~  highCost+ ConditionDummy+ open + ConditionDummy*open + highCost*ConditionDummy + highCost*open +(1| ID)+(1|Ngroup) ,data=PNP4C)
summary(model)
model = lmer(search ~  highCost+ ConditionDummy + highCost*ConditionDummy  +(1| ID) ,data=open)
summary(model)
model = glmer(search ~  ConditionDummy+(1|ID)  ,data=open.highcost,family = "binomial")
summary(model)
model = lmer(search ~  ConditionDummy+(1|ID)  ,data=open.lowcost)
summary(model)
model = lmer(search ~ C + ConditionDummy + (1 | ID)+(1|Ngroup),data=closed)
summary(model)
anova(lmer(search ~  ConditionDummy  + (1|ID) +(1|Ngroup),data=closed ,
      nAGQ = 1))

model1 = lmer(search ~ C+ ConditionDummy +  (1 | Ngroup) + (1 | Ngroup:ID),data=PNP4C ,
              nAGQ = 1)
model1 = lmer(search ~ C+ ConditionDummy  + (1|ID) +(1|Ngroup),
             data=closed)
summary(model1)
anova(model1,model)
model2 = lme(search ~ open + C+ open*ConditionDummy + (Ngroup|ID) +(1|Ngroup),
              data=PNP4C)

anova(model2,model)

summary(model)
anova(model)

patent.model = lmer(search ~ ConditionDummy +
                          C + open + (1+ConditionDummy|ID) +
                        data=PNP4C,
                        REML=TRUE)
model = lmer(search ~ open + C+ ConditionDummy + open*ConditionDummy + (1|ID),
             data=PNP4C,
             REML=FALSE)
summary(model)
library(data.table)
library(dplyr)
PNPS <- as.data.table(PNPS)
#try to cluster
#group index
PNP <- subset(PNPS, Condition != "Singleton")
groups <- PNP[,.N,by=.(ID,Ngroup)]
length(unique(groups$Ngroup))
length(unique(groups$ID))
length(groups$ID)
groups$GroupIndex <- rep(0)
index <- 1
for (i in 1:103) {
  if(groups[i*4-3,"GroupIndex"]==0){
    for(j in 1:length(groups$ID)) {
      groups[j,"GroupIndex"] <- ifelse(groups[j,"Ngroup"]==groups[i*4-3,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4-2,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4-1,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4,"Ngroup"],index,groups[j,"GroupIndex"])
      
      
    }
    index <- index+1
  }
}
table(groups$GroupIndex)
PNP <- plyr::join(PNP,unique(subset(groups,select = c(ID,GroupIndex))))



PNP4C <- as.data.table(PNP4C)
groups <- PNP4C[,.N,by=.(ID,Ngroup)]
length(unique(groups$Ngroup))
length(unique(groups$ID))
length(groups$ID)
groups$GroupIndex <- rep(0)
index <- 1
for (i in 1:103) {
  if(groups[i*4-3,"GroupIndex"]==0){
   for(j in 1:length(groups$ID)) {
      groups[j,"GroupIndex"] <- ifelse(groups[j,"Ngroup"]==groups[i*4-3,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4-2,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4-1,"Ngroup"]|groups[j,"Ngroup"]==groups[i*4,"Ngroup"],index,groups[j,"GroupIndex"])


  }
  index <- index+1
  }
}
table(groups$GroupIndex)
PNP4C1 <- join(PNP4C,unique(subset(groups,select = c(ID,GroupIndex))))
library(plyr)
library(Formula)
library(plm)
library(data.table)
library(lmtest)
plm1 <- plm(search ~ open + C+ ConditionDummy + open*ConditionDummy, model = "random",effect="individual",data = PNP4C1,index = c("GroupIndex") 
                    )
plm1
coeftest(plm1, vcov = pvcovHC(plm1,method="arellano",type = "HC3"))

#comparison between patent and no patent, over the cases of first self sequential discovery
plm1 <- plm(search ~ C+ConditionDummy , model = "random",effect="individual",data = selfsearch2,index = c("Ngroup") 
)
plm1
coeftest(plm1, vcov = pvcovHC(plm1,method="arellano",type = "HC3"))
model = glmer(search ~  C+ ConditionDummy +(1|ID)+(1|GroupIndex) ,data=selfsearch2,family = "binomial")
summary(model)

model = lmer(search ~  C+ ConditionDummy +(1|ID) +(1|GroupIndex),data=selfsearch2)
summary(model)

subset(PNPS4C[,.N,by=.(Condition,discovery)],discovery != 0)[,mean(N),by = Condition]
selfsearch1 <- selfsearch[,.(mean = mean(search),sd=sd(search),.N),by=.(Condition,ID,highCost)]
selfsearch1 <- selfsearch1[,.(mean = mean(mean),sd=sd(mean),.N),by=.(Condition,highCost)]

selfsearch1$se <- selfsearch1$sd / sqrt(selfsearch1$N)
selfsearch1$ic <- selfsearch1$se * qt((1-0.05)/2 + .5, selfsearch1$N-1)
library(ggplot2)

ggplot(subset(selfsearch1, highCost==1)) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  labs(x = "Cost", y = "exploration rate") +
  ggtitle("Exploration rate first self discovery")

table(selfsearch$search,selfsearch$Condition)
selfsearch2 <- subset(selfsearch,highCost==1)
model = glmer(search ~ ConditionDummy +(1+ConditionDummy|ID)+(1|GroupIndex) ,data=selfsearch2,family = "binomial")
summary(model)
#"closed" cases only
closed <- subset(PNP,open==0)
open <- subset(PNP,open==1)
plm2 <- plm(search ~ C+ConditionDummy , model = "random",effect="individual",data = closed,index = c("GroupIndex") 
)
plm2
coeftest(plm2, vcov = vcovHC(plm2,method="arellano",type = "HC3"))
model.plm <- plm(search ~ C+ConditionDummy , model = "random",effect="individual",data = closed,index = c("GroupIndex"))
coeftest(model.plm, vcov=vcovHC(model.plm,type="HC0",cluster="group"))
model.plm <- plm(search ~ C+ConditionDummy , model = "random",effect="individual",data = subset(PNP,selfsearch == 1),index = c("ID"))
coeftest(model.plm, vcov=vcovHC(model.plm,type="HC0",cluster="group"))

model = glmer(search ~ C+ ConditionDummy + (1 + ConditionDummy|GroupIndex) ,data=closed,family = "binomial")
summary(model)


closed[,mean(search),by=.(Condition)]

#checking for differences in the total discoveries
endGame3$ConditionDummy <- ifelse(endGame3$Condition == "Patent",1,0)
model = lmer(Nopen ~ ConditionDummy + (1+ConditionDummy|GroupIndex) ,data=endGame3)
summary(model)
model = lmer(Nopen ~ ConditionDummy + (1|GroupIndex) ,data=endGame3)
summary(model)
library(sjPlot) # table functions
library(sjmisc) # sample data
library(lme4)
library(lmerTest)
library(sandwich)
library(lmtest)
coeffs <- coef(summary(model)) # get estimates, etc...
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2 # add the much disputed p-values

coeffsp <- cbind(coeffs, "p value" = round(p,3)) # combine it into one object
write.csv(coeffsp, "coeffsp.csv") # export
#total exploration rate
discoveryLevel$ConditionDummy <- ifelse(discoveryLevel$Condition == "Patent",1,0)
model = lmer(search ~ C+ ConditionDummy + gender + age + (1|ID) + (1|GroupIndex) ,data=PNP)
summary(model)
endGame3[,mean(Nopen),by = Condition]
discoveryLevel1$ConditionDummy <- ifelse(discoveryLevel1$Condition == "Patent",1,0)
end
library(plm)
PNP1 <- subset(PNP,select = -c(lag11,lag12,lag21,lag22))
PNP <- PNP1
#main results so far
model = lmer(Nopen ~ ConditionDummy + (1|GroupIndex) ,data=endGame3)

summary(model)
library("texreg")
texreg(model)
library(foreign)
library(xtable)
library(stargazer)
#presenting the results: 
devtools::install_github("ewenharrison/finalfit")
library(sjPlot)
library(sjmisc) 
library(lme4)
library(dplyr)
tab_model(model,show.ngroups = TRUE)
endGame3 <- as.data.table(endGame3)
subset(endGame3,GroupIndex == shift(GroupIndex,n = 1,type = "lag"))[,mean(Nopen),by = Condition]#more discoveries under patents
t <- subset(discoveryLevel) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE) 
subset(discoveryLevel,Corner == 1) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE)

t <- subset(discoveryLevel) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE)
t <- subset(t, select = c("Condition", "Nplayers", "Nround", "IsParallel", "Failures", "Nsearch"))
print(t)
t1 <- subset(discoveryLevel1) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE)
t1 <- subset(t1, select = c("Condition", "Nplayers", "Nround", "IsParallel", "Failures", "Nsearch"))
print(t1)
newobject<-xtable(t1)
print.xtable(newobject, type="latex")
PNPS <- as.data.table(PNPS)
result3conditions <- merge(subset(PNPS,open == 0)[,.(firstsearch=mean(search)),by = Condition],
                       subset(PNPS,open == 1)[,.(seqsearch=mean(search)),by = Condition], by = "Condition")
result3conditions <- merge(result3conditions, subset(endGame1,select = c("Condition","mean")),by="Condition")
result3conditions <- merge(result3conditions, PNPS[,.(meanpayoff=mean(Payoff)),by="Condition"],by="Condition")
table1::label(result3conditions$firstsearch) <- "search rate of first discovery"
table1::label(result3conditions$seqsearch) <- "search rate of sequential discovery"
table1::label(result3conditions$mean) <- "number of discoveries"
table1::label(result3conditions$meanpayoff) <- "payoff"
three_conditions<-xtable(result3conditions)
print.xtable(three_conditions, type="latex")

subset(discoveryLevel,Corner == 1) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE)

model1 = lmer(Nplayers ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel)
#there is more exploration around discovery with no patent protection
model2 = lmer(Nround ~ ConditionDummy + (1|Ngroup) ,data=subset(discoveryLevel))
#discovery the whole mine takes less time under no patent protection
model3 = lmer(IsParallel ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel)
model4 = lmer(Nsearch ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel)
model5 = lmer(Failures ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel)
texreg(list(model1,model2,model3,model4,model5))

discoveryLevel1PS <- subset(discoveryLevel1,Condition != "No Patent")
model1 = lmer(Nplayers ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel1PS)
summary(model1)
#there is more exploration around discovery with no patent protection
model2 = lmer(Nround ~ ConditionDummy + (1|Ngroup) ,data=subset(discoveryLevel1PS))
summary(model2)
#discovery the whole mine takes less time under no patent protection
model3 = lmer(IsParallel ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel1PS)
summary(model3)
#more discoveries at the same time under patent protection (each of them is been explored with other player)

model4 = lmer(Nsearch ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel1PS)
summary(model4)
model5 = lmer(Failures ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel1PS)
summary(model5)
texreg(list(model1,model2,model3,model4))


m1 = lmer(search ~ C+ ConditionDummy +(1 |ID) + (1|GroupIndex) ,data=subset (PNP, open == 0))
summary(m) 
texreg(m)
#no significant affect of patent on searching the first discovery (underweighting of rare events)
m = lmer(search ~ C + ConditionDummy+(1|ID) + (1|GroupIndex)
         ,data=subset (PNPS, open == 0 & Condition != "No Patent"))
summary(m) 
m = lmer(Payoff ~ C + ConditionDummy+(1|ID) + (1|GroupIndex)
         ,data=subset (PNPS, open == 0 & Condition != "No Patent"))
summary(m)
texreg(m)
#learning effect
#significant decreases in the searching activity in no patent condition as the game progresses
#PNP$selfsearch <- ifelse(shift(PNP$Hive, 1L, type="lag") == 2 & 
#                          discoveryLevel[match(shift(PNP$Hexagon, 1L, type="lag"),discoveryLevel$ID),"Ngame"]
 #                        ==PNP$Ngroup,1,0)
plot(discoveryLevel$Nplayers,discoveryLevel$Nround)
table(PNP$selfsearch)
head(subset(PNP, selfsearch == 1))
for (i in 2:nrow(PNP)) {
  PNP[i,"selfsearch"] <- ifelse(PNP[i,"open"]==1 & PNP[i-1,"open"]==0 & PNP[i-1,"Hive"]==2,1,0)
}
sum(endGame3$Nopen)
PNP$HighCost <- ifelse(PNP$C>=20,1,0)
m2 = lmer(search ~ C+ ConditionDummy + (1|ID) + (1|GroupIndex),data=subset(PNP, selfsearch == 1 ))
summary(m2)

#players search more for self-sequential discovery under no patent protection (weakly significant).
#Is it good? They search more since they react to competitive condition. But they finally find less since they search less optimaly,
#they can't learn from others failure, and they earn much less from this competition.
m3 = lmer(search ~ C+ ConditionDummy  + (1+ConditionDummy|ID) ,data=subset(PNP, selfsearch == 1 &HighCost == 1))
summary(m3)#results are stronger when we consider the ceiling affect in low costs.

m4 = lmer(search ~ C+ G + (1|ID) + (1|GroupIndex) ,data=subset(PNP,Condition == "No Patent"))
summary(m4)#learning effect in no patent condition

m4 = lmer(search ~ C+ G + (1|ID) + (1|GroupIndex) ,data=subset(PNP,Condition == "Patent"))
summary(m4)#learning effect in the patent condition
texreg(m4)

model.plm <- plm(search ~ C+ConditionDummy , model = "random",effect="individual",
                 data = subset(PNP, selfsearch == 1 & HighCost == 1),index = c("ID"))
coeftest(model.plm, vcov=vcovHC(model.plm,type="HC0",cluster="group"))

model4 = lmer(Failures ~ ConditionDummy + (1|Ngroup) ,data=discoveryLevel)
summary(model4) #more failures under no patent condition, less efficient search.  
model = lmer(Payoff ~ C + ConditionDummy + (1|Ngroup) + (1|ID) ,data=PNP)
summary(model) #Higher average payoff with patent
texreg(model)

PNPS$ConditionDummy <- ifelse(PNPS$Condition == "Patent",1,0)
model1 = lmer(search ~ C+ ConditionDummy + (1|ID),
             data = subset(PNPS,(Condition == "Singleton" | Condition == "Patent")& open == 0))
summary(model)
sjPlot::tab_model(model1,model4,model2,model3)
sjPlot::tab_model(m,m1,m2)
sjPlot::tab_model(model)

#the effect of others' discoveries
PNP$other_discovery <- ifelse((shift(PNP$IDOpenDiscoveries1, n=1,fill= 0, type="lag") != PNP$IDOpenDiscoveries1 |
                                 shift(PNP$IDOpenDiscoveries2, n=1,fill= 0, type="lag") != PNP$IDOpenDiscoveries2 |
                                 shift(PNP$IDOpenDiscoveries3, n=1,fill= 0, type="lag") != PNP$IDOpenDiscoveries3 )&
                                 shift(PNP$Hive, n=1,fill= 0, type="lag") != 2& PNP$Condition == "Patent",1,0)
table(subset(PNP,Condition == "Patent")$other_disscovery,subset(PNP,Condition == "Patent")$open)
model2 = lmer(search ~ C+ other_discovery + (1|ID) + (1|GroupIndex),
             data = subset(PNP,Condition == "Patent" & open == 0))
summary(model)
texreg(list(model1,model2))

sjPlot::tab_model(model)
library(dplyr)
rename(PNP, other_disscovery = other_discovery)
#deviation from optimal strategy

model = lmer(deviation ~ open +ConditionDummy + (1|ID) + (1|GroupIndex),
             data = subset(PNP))
summary(model)

subset(discoveryLevel,farsearch ==0|Condition == "No Patent")[,mean(Nround,na.rm = TRUE),by = Condition]
discoveryLevel <- as.data.table(discoveryLevel)
open <- as.data.table(open)
open[,mean(search),by = Condition]

write.csv(endGame3, "endGame.csv")
write.csv(discoveryLevel, "discovery level.csv")
write.csv(PNP, "patent and no patent data.csv")
write.csv(PNPS, "patent no patent data and singleton.csv")

library(table1)

table1::label(discoveryLevel$Nplayers) <- "Number of players"
table1::label(discoveryLevel$Failures) <- "Number of failures"
table1::label(discoveryLevel$IsParallel) <- "Parallel mines"
table1::label(discoveryLevel$Nround) <- "Number of rounds to reveal the mine"

table1::table1(~Nplayers + Failures + IsParallel + Nround | Condition, data = discoveryLevel)
#presenting the ceiling effect
PNP<- as.data.table(PNP)
table(PNP$Condition,PNP$selfsearch)
PNP$selfsearch <- ifelse(is.na(PNP$selfsearch),0,PNP$selfsearch)
Search <- summarySE(PNP, measurevar="search", groupvars=c("C","open","selfsearch","Condition"))

#figure 3
ggplot(subset(Search,selfsearch == 1), aes(x=C, y=search, linetype=Condition)) + 
  geom_line() +
  geom_point() +
 xlab('Exlploration cost') +
  ylab('Exploration rate') + ggtitle("Exploration rate of subsequent treasures")+
  theme(text = element_text(size=15))
#figure 3 no title
ggplot(subset(Search,selfsearch == 1), aes(x=C, y=search, linetype=Condition)) + 
  geom_line() +
  geom_point() +
  xlab('Exlploration cost') +
  ylab('Exploration rate') +
  theme(text = element_text(size=15))
#figure 2
ggplot(subset(Search,open == 0), aes(x=C, y=search, linetype=Condition)) + 
  geom_line() +
  geom_point() +  xlab('Exploration cost') +
  ylab('Exploration rate') + ggtitle("Exploration rate of first treasures")+theme(text = element_text(size=15))
#figure 2 no title
ggplot(subset(Search,open == 0), aes(x=C, y=search, linetype=Condition)) + 
  geom_line() +
  geom_point() +  xlab('Exploration cost') +
  ylab('Exploration rate') +theme(text = element_text(size=15))


geom_pointrange( aes(x=C, ymin=search, ymax=search+ci+0.05)) +
ggplot(Search, aes(x=C, y=mean, col=Condition)) + geom_line() +
  xlab('search cost') +
  ylab('search rate') + ggtitle("Search rate of sequential discoveries")


Search <- subset(PNP, open == 0)[,.(mean=mean(search)),by=.(Condition,C)]
ggplot(Search, aes(x=C, y=mean, col=Condition)) + geom_line() +
  xlab('search cost') +
  ylab('search rate') + ggtitle("Search rate of first discoveries")


PNPS[,.N,by = G]



