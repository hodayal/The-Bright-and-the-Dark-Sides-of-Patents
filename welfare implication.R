library(ggplot2)
#overall treasures found
#figure 6

endGame3 <- as.data.table(endGame3)
endGame <- subset(PNP,R==50)
endGame$Nopen <- endGame$NOpenDiscoveries1+endGame$NOpenDiscoveries2*2+endGame$NOpenDiscoveries3*3
endGame2 <- subset(endGame, select = c("Condition","Nopen","Ngroup","GroupIndex"))
endGame3 <- unique(endGame2)
endGame1 <- endGame3[,. (mean =mean(Nopen),sd = sd(Nopen),.N),by = Condition]
hist(subset(endGame3,Condition == "Patent")$Nopen)
hist(subset(endGame3,Condition == "No Patent")$Nopen)
ggplot(endGame3, aes(Nopen, fill = Condition)) + geom_density(alpha = 0.2) + ggtitle("Total discoveries")

endGame1$N <- endGame1$N*4
endGame1$se <- endGame1$mean / sqrt(endGame1$N)
endGame1$ic <- endGame1$se * qt((1-0.05)/2 + .5, endGame1$N-1)
limits <- aes(ymax = endGame1$mean + endGame1$ic-2, ymin=endGame1$mean - endGame1$ic+2)

ggplot(subset(endGame1,Condition != "Singleton")) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("No. treasures at the group level")+ylab("No. treasures") + theme_grey(base_size = 15)

endGame <- data.table(Condition = .("Patent", "No Patent"),mean = .(3.67,3.8))
endGame$Condition <- as.character(endGame$Condition)
endGame$mean <- as.numeric(endGame$mean)
ggplot(endGame) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  
  ggtitle("Individual discoveries")+ylab("No. discoveries") + theme_grey(base_size = 15)
#t-test for N founded discoveries
install.packages("ggpubr")
library("ggpubr")
ggboxplot(subset(endGame1,Condition != "Singleton"), x = "Condition", y = "mean", 
          color = "Condition",  palette = c("#00AFBB", "#E7B800"),
          ylab = "Number of discoveries", xlab = "Condition")

g<-ggplot(endGame1,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",fill="forestgreen", position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))+ggtitle("overall founded discoveries")

g



endGame1 <- endGame2[,.(mean = mean(Nopen),sd = sd(Nopen),.N),by = Condition] %>%
endGame1$se <- endGame1$mean / sqrt(endGame1$N / 4)
endGame1$ic <- se * qt((1-0.05)/2 + .5, n-1)


#overall exploration rate
all_data$search <- ifelse(all_data$Hive > 0 ,1,0)
table(all_data$search)
all_data <- as.data.table(subset(all_data,ID > 0))
searchrate <- PNPS[,.(searchrate=mean(search),sd = sd(search),.N),by = .(ID,Condition)]
searchrate <- searchrate[,.(searchrate=mean(searchrate),sd = sd(searchrate),.N),by = .(Condition)]
searchrate$se <- searchrate$searchrate / sqrt(searchrate$N)
searchrate$ic <- searchrate$se * qt((1-0.05)/2 + .5, searchrate$N-1)

ggplot(searchrate) +
  geom_bar( aes(x=Condition, y=searchrate), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=searchrate-ic, ymax=searchrate+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("overall search rate")

PNPS <- as.data.table(PNPS)
findrate <- subset(PNPS,Hive != 0)
table(findrate$Hive-1,factor(findrate$Condition))
findrate$Hive <- findrate$Hive-1
findrate <- findrate[,.(mean=mean(Hive)),by = .(Condition,ID)]
findrate <- findrate[,.(findrate=mean(mean),sd = sd(mean),.N),by = .(Condition)]
findrate$se <- findrate$findrate / sqrt(findrate$N)
findrate$ic <- findrate$se * qt((1-0.05)/2 + .5, findrate$N-1)

ggplot(findrate) +
  geom_bar( aes(x=Condition, y=findrate), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=findrate-ic, ymax=findrate+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("overall searching rate")

#find the overall exploration rate by round
search <- PNP4[,.(mean=mean(search),sd=sd(search),N=.N),by=.(Condition,ID)]
search <- search[,.(mean=mean(mean),sd=sd(mean),N=.N),by=.(Condition)]
search$se <- search$sd / sqrt(search$N)
search$ic <- search$se * qt((1-0.05)/2 + .5, search$N-1) 
limits <- aes(ymax = search$mean + search$ic, ymin=search$mean - search$ic)

Plot <- ggplot(data = search, aes(x = Condition, y = mean))
Plot <- Plot + geom_bar(stat = "identity",
                        position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25)
Plot+ggtitle("Overall exploration rate")

table(PNPS$C,PNPS$Condition)

#the sum of players earnings in each group.

endGame <- subset(endGame,ID>0)
endGame$end <- rep(0)
for (i in 1:length(endGame$end)) {if ((i%%50==0))
  endGame[i,"end"]<-1
else(endGame[i,"end"]<-endGame[i,"end"])
}
sumPay <- subset(endGame, Condition != "Singleton")

#group index- the same picture at the end of the game
length(PNPS$Ngroup)
PNPS$endGame <- ifelse(PNPS$R%%50 == 0,1,0)
PNPS$Ngroup <- rep(0)
Ngroup <- 1
for (i in 24001:length(PNPS$Ngroup)
) {
   
  if(PNPS[i,"endGame"] == 1 & PNPS[i,"Ngroup"] == 0){
    PNPS[i,"Ngroup"] <- Ngroup
    for (j in 1:length(PNPS$Ngroup)) {
      if (PNPS[j,"endGame"] == 1 & identical(PNPS[i,"IDOpenDiscoveries1"],PNPS[j,"IDOpenDiscoveries1"])
                    & identical (PNPS[i,"IDOpenDiscoveries2"],PNPS[j,"IDOpenDiscoveries2"])
                    & identical (PNPS[i,"IDOpenDiscoveries3"],PNPS[j,"IDOpenDiscoveries3"])
                    & identical (PNPS[i,"MAP"],PNPS[j,"MAP"])
                    & identical (PNPS[i,"Condition"],PNPS[j,"Condition"])
          )
      {PNPS[j,"Ngroup"] <- Ngroup}
    }
    Ngroup <- Ngroup + 1
  }
  
}
table(PNPS$Ngroup)


for (i in length(PNPS$Ngroup):1){
  PNPS[i,"Ngroup"] <- ifelse( PNPS[i,"Ngroup"] == 0,PNPS[i+1,"Ngroup"],PNPS[i,"Ngroup"])
}

check <- subset(PNPS,Ngroup == 179&endGame == 1)
sumPay1 <- sumPay[,.(mean = mean(Payoff),sd=sd(Payoff),.N),by=.(Ngroup,Condition,G)]
sumPay1 <- sumPay1[,.(mean = mean(mean),sd=sd(mean),.N),by=.(Condition)]
sumPay1$se <- sumPay1$mean / sqrt(sumPay1$N)
sumPay1$ic <- sumPay1$se * qt((1-0.05)/2 + .5, sumPay1$N-1)

ggplot(sumPay1) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("mean over all payoff per group of players")

#N of open or closed cases

table(PNPS$open,PNPS$Condition)
table(PNPS$gender,PNPS$Condition)

#reward

PNPS$reward <- ifelse(PNPS$search == 1, PNPS$Payoff + PNPS$C,0)
table(PNPS$reward)
table(PNPS$Hive,PNPS$reward,PNPS$Condition)
subset(PNPS,reward == 160)
subset(PNPS, discovery == 129|discovery == 130|discovery == 242|discovery == 291)

#finding prbability
table(PNPS$Hive,PNPS$Condition)
discoveryLevel <- as.data.table(discoveryLevel)
#The number of discovery per player-first discovery
#figure 5
endGame <- subset(PNPS,R==50)
endGame$Ndis <- str_count(endGame$IDSelfOpenDiscoveries,"\\-")
endGame$Ndis <- str_count(endGame$IDSelfOpenDiscoveries,"\\-")
x<-discoveryLevel1[,.N,by=.(Condition,Finder,order)]
y<-subset(PNPS,select = c("Condition","ID","G"))
names(y)[names(y) == "ID"] <- "Finder"
names(y)[names(y) == "G"] <- "order"
y <- unique(y)
mergedData <- merge(x, y,all=TRUE)
mergedData$N <- ifelse(is.na(mergedData$N),0,mergedData$N)
mergedData<-mergedData[!(mergedData$order==5),]
x<- subset(mergedData,Condition == "Patent")
y<-subset(mergedData,Condition == "No Patent")
t.test(x$N,y$N)
NfirstDs <- mergedData[,mean(N),by=.(Condition)]
#The number of discovery per player-sequential discovery

endGame2 <- subset(endGame, select = c("Condition","ID","Ndis","G"))
endGame3 <- unique(endGame2)
endGame3<-endGame3[!(endGame3$G==5),]
endGame3 <- as.data.table(endGame3)
endGame1 <- endGame3[,. (mean =mean(Ndis),sd = sd(Ndis),N=.N),by = .(Condition)]
endGame1$se <- endGame1$mean / sqrt(endGame1$N)
endGame1$ic <- endGame1$se * qt((1-0.05)/2 + .5, endGame1$N-1)

ggplot(subset(endGame1,Condition != "Singleton")) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("No. treasures at the individual level")+ylab("No. treasures")+ylim(0,10)+ theme_grey(base_size = 15)

Ndis <- join(endGame1,mergedData[,mean(N),by=.(Condition)]
)
names(Ndis)[names(Ndis) == "mean"] <- "total number of discoveries"
names(Ndis)[names(Ndis) == "V1"] <- "first discoveries"
xtable(Ndis, type = "latex")

overall <- PNPS[,.(overall = mean(search)),by = Condition]
first <- subset(PNPS,open==0)[,.(first_discovery = mean(search)), by = Condition]
sequential <-  subset(PNPS,open==1)[,.(sequential_discovery = mean(search)), by = Condition]
shareseq <- PNPS[,.(share_sequential = mean(open)),by = Condition]
search_rate <- join(overall,first)
search_rate <- join(search_rate,sequential)
search_rate <- join(search_rate,shareseq)
xtable(subset(search_rate,Condition != "Singleton"),
       "Searching rate overall the game (column 1), only for first discoveries (column 2), only for sequential discovery (column 3),
       and the share of rounds in which a player can search for sequential discoveries")

PNP$pay <- PNP$Payoff+PNP$C

PNP <- as.data.table(PNP)
PNP$find <- ifelse(PNP$Hive == 2,1,0)
disID2 <- PNP[,.N,by=.(Condition,Ngroup,find,ID)]
subset(disID2)[,.(Ndiscoveries = mean(N),sd = sd(N)),by = .(Condition,find,Ngroup,ID)]

#finding the final rate of discoveries

PNPS <- as.data.table(PNPS)
disrate1 <- PNPS[,.N,by=.(ID,Hive,G)]
disrate <- join(subset(disrate1,Hive == 1,select = -Hive),endGame)
for (i in 1:nrow(disrate)) {
  disrate[i,'failed'] <- str_count(disrate[i,'IDOpenDiscoveries1'],"-")*4+
    str_count(disrate[i,'IDOpenDiscoveries2'],"-")*7/2 + 
    str_count(disrate[i,'IDOpenDiscoveries3'],"-")*9/3 +
    disrate[i,'N']
}
for (i in 1:nrow(disrate)) {
  disrate[i,'dis'] <- str_count(disrate[i,'IDOpenDiscoveries1'],"-")*3+
    str_count(disrate[i,'IDOpenDiscoveries2'],"-")*3/2 +
    str_count(disrate[i,'IDOpenDiscoveries3'],"-")*3/3
}
disrate$rate <- (105-disrate$dis)/(2100-disrate$dis-disrate$failed)
mean(disrate$rate)
max(disrate$rate)
hist(disrate$rate)
disrate[,mean(rate),by=Condition]

#frac of failed searched and secsess searches
N <- subset(PNP, select = c('Condition','Hive','ID','discovery','open'))
S <- N[,.N,by = .(ID,discovery)]
S <- subset(S,discovery != 0)
S <- S[,.N,by = ID]
N <- join(N,S)
N <- subset(N,open == 1&Hive !=0)[,.(NoSearch = .N),by = .(Condition,Hive,ID,N)]
N$Nmines <- N$N/3
wide = N %>% 
  spread(Hive, NoSearch)
wide$SecFrac <- wide$`2`/(wide$`1`+wide$`2`)
wide$SecFrac <- ifelse(is.na(wide$SecFrac), 0, wide$SecFrac)
mean(wide$SecFrac)
N <- wide[,.(mean = mean(SecFrac),sd = sd(SecFrac),.N),by = Condition]
N$se <- N$mean / sqrt(N$N)
N$ic <- N$se * qt((1-0.05)/2 + .5, N$N-1)
#figure 8
ggplot(N) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("Rate of successful searches") + ylab("Rate")+theme(text = element_text(size=15))
