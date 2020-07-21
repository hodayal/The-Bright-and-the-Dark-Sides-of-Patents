is_linked <- function(left1,right1,left2,right2) {
  
  if (right1 == right2 & abs (left1-left2) == 1) {
    result <- TRUE
  }
  else if ((right1 %% 2) == 1 & ((left1==left2 & abs(right1-right2)==1)|(left2==left1+1 & abs(right1-right2)==1))) {
    result <- TRUE
  }
  else if ((right2 %% 2) == 1 & ((left1==left2 & abs(right1-right2)==1)|(left1==left2+1 & abs(right1-right2)==1))){
    result <- TRUE
  }
  else {
    result <- FALSE
  }
  
  return(result)
}
is_linked(26,58,25,59)

searchrate02 <- subset(nopatent,select = c("C","Hive","search","ID"),open == 0,farsearch==0)

searchrateOpen02 <- subset(nopatent,select = c("C","Hive","search","ID"),open == 1,farsearch==0)
searchrate102 <- searchrate02[,.(rate=mean(search)),by=.(C,ID)]
searchrateOpen102 <- searchrateOpen02[,.(rate=mean(search)),by=.(C,ID)]

library(ggplot2)
myData02 <- searchrate102[,.(mean=mean(rate),sd=sd(rate),n=.N),by=.(C)]
myData02$se <- myData02$sd / sqrt(myData02$n)
myDataO02 <- searchrateOpen102[,.(mean=mean(rate),sd=sd(rate),n=.N),by=.(C)]
myDataO02$se <- myDataO02$sd / sqrt(myDataO02$n)
myData02$open <- rep(0)
myDataO02$open <- rep(1)
myData02 <- rbind(myData02,myDataO02)

limits02 <- aes(ymax = myData02$mean + myData02$se,
                ymin = myData02$mean - myData02$se)

nopatentPlot <- ggplot(data = myData02, aes(x = factor(C), y = mean,
                                            fill = factor(open)))

nopatentPlot <- nopatentPlot + geom_bar(stat = "identity",
                                        position = position_dodge(0.9)) +
  geom_errorbar(limits02, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Cost", y = "exploration rate") +
  ggtitle("Exploration rate no patent") +
  scale_fill_discrete(name = "Is Open?")
nopatentPlot

#patent
patent <- subset(all_data,Condition=="Patent")
patent <- as.data.table(patent)
patent$open <- ifelse((str_count(patent$IDSelfOpenDiscoveries,"\\("))%%3>0,1,0)

patent$corner <- rep(0)
value1 <- "\\(0-"
value2 <- "\\(29-"
value3 <- "-0\\)"
value4 <- "-69\\)"
for (i in 1:length(patent$open)) {
  chars1 <- patent[i,"IDSelfOpenDiscoveries"]
  
  patent[i,"corner"] <- ifelse(grepl(value1, chars1)|grepl(value2, chars1)|grepl(value3, chars1)|grepl(value4, chars1),1,0)
}
table(patent$corner)


patent$lag11 <- shift(patent$NOpenDiscoveries1, n=1,fill= 0, type="lag")
patent$lag12 <- shift(patent$NOpenDiscoveries1, n=2,fill= 0, type="lag")
patent$lag21 <- shift(patent$NOpenDiscoveries2, n=1,fill= 0, type="lag")
patent$lag22 <- shift(patent$NOpenDiscoveries2, n=2,fill= 0, type="lag")
patent$open <- ifelse((patent$corner==1 & patent$NOpenDiscoveries1==patent$lag11 & patent$NOpenDiscoveries1==patent$lag12 & patent$NOpenDiscoveries2==patent$lag21 & patent$NOpenDiscoveries2==patent$lag22),0,patent$open)

table(patent$open)


patent$search <- ifelse(patent$Hive>0,1,0)

patent <- as.data.table(patent)
patent$isLinked <- rep(0)
for (i in 1:length(patent$isLinked)) {
  x <- str_extract_all(patent[i,"Hexagon"],"\\(?[0-9,.]+\\)?")[[1]]
  x <- gsub("\\(","",x)
  x <- gsub("\\)","",x)
  x <- as.numeric(x)
  y <- str_extract_all(patent[i,"IDSelfOpenDiscoveries"],"\\(?[0-9,.]+\\)?")[[1]]
  y <- gsub("\\(","",y)
  y <- gsub("\\)","",y)
  y <- as.numeric(y)
  ly <- as.numeric(length(y))
  lx <- as.numeric(length(x))
  isLinked <- FALSE
  if (lx>1){
    if (ly>1){for (j in 1:(ly/2)) {
      if (!isLinked){isLinked <- is_linked(x[1],x[2],y[2*j-1],y[2*j])}
      
    } 
    }
  }
  patent[i,"isLinked"] <- as.numeric(isLinked)
}
library(stringr)
patent$isLinked <- rep(0)
for (i in 218) {
  x <- str_extract_all(PNP[i,"Hexagon"],"\\(?[0-9,.]+\\)?")[[1]]
  x <- gsub("\\(","",x)
  x <- gsub("\\)","",x)
  x <- as.numeric(x)
  y <- str_extract_all(PNP[i,"IDSelfOpenDiscoveries"],"\\(?[0-9,.]+\\)?")[[1]]
  y <- gsub("\\(","",y)
  y <- gsub("\\)","",y)
  y <- as.numeric(y)
  ly <- as.numeric(length(y))
  lx <- as.numeric(length(x))
  isLinked <- FALSE
  if (lx>1){
    if (ly>1){for (j in 1:(ly/2)) {
      if (!isLinked){isLinked <- is_linked(x[1],x[2],y[2*j-1],y[2*j])}
      
    } 
    }
  }
   as.numeric(isLinked)
}
as.numeric(isLinked)
subset(patent,isLinked==1)
patent$farsearch <- ifelse(patent$isLinked==0&patent$search==1&patent$open==1,1,0)
farsearch <- subset(patent,patent$farsearch==1)

table(patent$farsearch)

searchrate01 <- subset(patent,select = c("C","Hive","search","ID"),open == 0,farsearch==0)

searchrateOpen01 <- subset(patent,select = c("C","Hive","search","ID"),open == 1,farsearch==0)
searchrate101 <- searchrate01[,.(rate=mean(search)),by=.(C,ID)]
searchrateOpen101 <- searchrateOpen01[,.(rate=mean(search)),by=.(C,ID)]

library(ggplot2)
myData01 <- searchrate101[,.(mean=mean(rate),sd=sd(rate),n=.N),by=.(C)]
myData01$se <- myData01$sd / sqrt(myData01$n)
myDataO01 <- searchrateOpen101[,.(mean=mean(rate),sd=sd(rate),n=.N),by=.(C)]
myDataO01$se <- myDataO01$sd / sqrt(myDataO01$n)
myData01$open <- rep(0)
myDataO01$open <- rep(1)
myData01 <- rbind(myData01,myDataO01)

limits01 <- aes(ymax = myData01$mean + myData01$se,
                ymin = myData01$mean - myData01$se)

patentPlot <- ggplot(data = myData01, aes(x = factor(C), y = mean,
                                          fill = factor(open)))

patentPlot <- patentPlot + geom_bar(stat = "identity",
                                    position = position_dodge(0.9)) +
  geom_errorbar(limits01, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Cost", y = "exploration rate") +
  ggtitle("Exploration rate patent") +
  scale_fill_discrete(name = "Is Open?")
patentPlot
#find the overall exploration rate by open/closed
PNPSC <- subset(PNPS,farsearch == 0)
search <- PNPSC[,.(mean=mean(search),sd=sd(search),N=.N),by=.(Condition,open)]
search$se <- search$sd / sqrt(search$N)
search$ic <- search$se * qt((1-0.05)/2 + .5, search$N-1) 
limits <- aes(ymax = search$mean + search$ic, ymin=search$mean - search$ic)

  Plot <- ggplot(data = search, aes(x = factor(open), y = mean,fill = factor(Condition)))
Plot <- Plot + geom_bar(stat = "identity",
                        position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25)+
  labs(x = "Cost", y = "exploration rate") +
  ggtitle("search rate") +
  scale_fill_discrete(name = "Is Open?")
Plot
#exploration per individual
library(data.table)
library(ggplot2)
PNP <- as.data.table(PNP)
Search <- subset(PNP,selfsearch == 1)[,.(mean=mean(search)),by=.(Condition,C)]
Search$se <- Search$sd / sqrt(Search$N)
Search$ic <- Search$se * qt((1-0.05)/2 + .5, Search$N-1)

limits <- aes(ymax = Search$mean + Search$ic,
              ymin = Search$mean - Search$ic)

searchPlot <- ggplot(data = Search, aes(x = factor(C), y = mean,
                                        fill = factor(Condition)))

searchPlot <- searchPlot + geom_bar(stat = "identity",
                                    position = position_dodge(0.9)) +
  
  labs(x = "cost", y = "search rate") +
  ggtitle("Search rate of sequential discovery") +
  scale_fill_discrete(name = "Condition")
searchPlot <-  searchPlot + scale_fill_grey(start = .3, end = .9)+ theme_bw()

searchPlot
skipSearch
#try to plot the negative effect of patent in the data
length(unique(PNPS$ID))
length(unique(PNPS4$ID))
length(unique(skipSearch))

PNPS4 <- subset(PNPS,!(ID %in% skipSearch))
PNPS4C <- subset(PNPS4, farsearch == 0)
PNP4 <- subset(PNPS4, Condition != "Singleton")

PNP4C <- subset(PNPS4,Condition != "Singleton"&farsearch == 0)
PNP4C$categories <- ifelse (PNP4C$Condition == "Patent" & PNP4C$open==0,"patent closed",
        ifelse(PNP4C$Condition == "Patent" &PNP4C$open ==1, "patent open",
               ifelse(PNP4C$Condition == "No Patent" &PNP4C$open ==0, "no patent closed",
                     "no patent open")))
table(PNP4C$categories)
#PNP5 <- PNP4C[,.(search = mean(search)),by = .(C,categories,ID)]
PNP5 <- PNP4C[,.(mean=mean(search),sd=sd(search),.N),by=.(C,categories)]
PNP5$se <- PNP5$mean / sqrt(PNP5$N)
PNP5$ic <- PNP5$se * qt((1-0.05)/2 + .5, PNP5$N-1)
limits <- aes(ymax = PNP5$mean + PNP5$ic, ymin=PNP5$mean - PNP5$ic)

write.csv(PNP4C1,"data without weak threshold and farsearch.csv")
write.csv(PNPS,"all data processed1.csv")

ggplot(data=PNP5, aes(x=C, y=mean, group=categories, shape=categories, color=categories)) +
  geom_line() +
  geom_point() + labs(x = "searching cost", y = "search rate") + 
  geom_errorbar(limits, width=.2,position=position_dodge(0.05)) +
  ggtitle("search rate - without weak threshold holders")
#classifing farsearch as closed case.
table(PNP4$farsearch,PNP4$open)
PNP6 <- PNP4
PNP6$open <- ifelse(PNP6$farsearch == 1,0,PNP6$open)
PNP6$categories <- ifelse (PNP6$Condition == "Patent" & PNP6$open==0,"patent closed",
                            ifelse(PNP6$Condition == "Patent" &PNP6$open ==1, "patent open",
                                   ifelse(PNP6$Condition == "No Patent" &PNP6$open ==0, "no patent closed",
                                          "no patent open")))
PNP5 <- PNP6[,.(mean=mean(search),sd=sd(search),.N),by=.(C,categories)]
ggplot(data=PNP5, aes(x=C, y=mean, group=categories, shape=categories, color=categories)) +
  geom_line() +
  geom_point() + labs(x = "searching cost", y = "search rate") + 
  ggtitle("search rate - without weak threshold holders")







openrate <- PNP[,.(openrate = mean(open),sd = sd(open), N=.N),by = Condition]
openrate$se <- openrate$sd / sqrt(openrate$N)
openrate$ic <- openrate$se * qt((1-0.05)/2 + .5, openrate$N-1)
limits <- aes(ymax = openrate$mean + openrate$ic,
              ymin = openrate$mean - openrate$ic)

ggplot(openrate) +
  geom_bar( aes(x=Condition, y=openrate), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=openrate-ic, ymax=openrate+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("The rate of open discovery cases")

#adding the singletone condition


PNPS4$categories  <- ifelse (PNPS4$Condition == "Patent" & PNPS4$open==0,"patent closed",
                                ifelse(PNPS4$Condition == "Patent" &PNPS4$open ==1, "patent open",
                                       ifelse(PNPS4$Condition == "No Patent" &PNPS4$open ==0, "no patent closed",
                                              ifelse(PNPS4$Condition == "No Patent" &PNPS4$open ==1, "no patent open",
                                                     ifelse(PNPS4$Condition == "Singleton" &PNPS4$open ==1, "singleton open","singleton closed")))))
table(PNPS4$categories)
PNPS5 <- PNPS4[,.(searchrate = mean(search)),by = .(C,categories)]
ggplot(data=PNPS5, aes(x=C, y=searchrate, group=categories, shape=categories, color=categories)) +
  geom_line() +
  geom_point() + labs(x = "searching cost", y = "search rate") +
  ggtitle("search rate - without weak threshold holders")
PNP4 <- subset(PNPS4,Condition != "Singleton")

PNP4$categories  <- ifelse (PNP4$Condition == "Patent" & PNP4$open==0,"patent first discovery",
                             ifelse(PNP4$Condition == "Patent" &PNP4$open ==1, "patent sequential discovery",
                                    ifelse(PNP4$Condition == "No Patent" &PNP4$open ==0, "no patent first discovery", "no patent sequential discovery")))
table(PNP4$categories)
PNP5 <- PNP4[,.(searchrate = mean(search)),by = .(C,categories)]
ggplot(data=PNP5, aes(x=C, y=searchrate, group=categories, shape=categories, color=categories)) +
  geom_line() +
  geom_point() + labs(x = "searching cost", y = "search rate") +
  ggtitle("Searching rate")

#singleton vs patent

PNPS4$categories  <- ifelse (PNPS4$Condition == "Patent" & PNPS4$open==0,"patent first discovery",
                             ifelse(PNPS4$Condition == "Patent" &PNPS4$open ==1, "patent  sequential discovery",
                                    ifelse(PNPS4$Condition == "No Patent" &PNPS4$open ==0, "no patent first discovery",
                                           ifelse(PNPS4$Condition == "No Patent" &PNPS4$open ==1, "no patent  sequential discovery",
                                                  ifelse(PNPS4$Condition == "Singleton" &PNPS4$open ==1, "singleton  sequential discovery","singleton first discovery")))))
table(PNPS4$farsearch)
PS <- subset(PNPS4,Condition != "No Patent"&farsearch==0)
PS <- PS[,.(searchrate = mean(search)),by = .(C,categories)]
ggplot(data=PS, aes(x=C, y=searchrate, group=categories, shape=categories, color=categories)) +
  geom_line() +
  geom_point() + labs(x = "searching cost", y = "search rate") +
  ggtitle("search rate - Patent sv Singleton")

#no patent - the effect of the first finder
PNPS <- as.data.table(PNPS)
NPC <- subset(PNP4C1,Condition == "No Patent")
NPC$selfDiscovery <- rep(0)
for (i in 2:length(NPC$selfDiscovery)) {
  NPC[i,"selfDiscovery"] <- ifelse(NPC[i,"open"]==1 & NPC[i-1,"open"]==0 & NPC[i-1,"Hive"]==2,1,0)
  #NPC[i,"selfDiscovery"] <- ifelse(NPC[i-1,"selfDiscovery"] == 1 & NPC[i,"open"] == 1,1,NPC[i,"selfDiscovery"])
}
table(NPC$selfDiscovery,NPC$search)
P <-  subset(PNP4C1,Condition == "Patent")
P$selfDiscovery <- rep(0)
for (i in 2:length(P$selfDiscovery)) {
  P[i,"selfDiscovery"] <- ifelse(P[i,"open"]==1 & P[i-1,"open"]==0 & P[i-1,"Hive"]==2,1,0)
  #NPC[i,"selfDiscovery"] <- ifelse(NPC[i-1,"selfDiscovery"] == 1 & NPC[i,"open"] == 1,1,NPC[i,"selfDiscovery"])
}
table(P$selfDiscovery,P$search)
selfsearch <- rbind(subset(P,selfDiscovery==1),subset(NPC, selfDiscovery==1))

selfsearch <- NPC[,.(mean = mean(search),sd = sd(search),.N),by=.(open,selfDiscovery,ID)]
selfsearch <- selfsearch[,.(mean = mean(mean),sd = sd(mean),.N),by=.(open,selfDiscovery)]

selfsearch$se <- selfsearch$sd / sqrt(selfsearch$N)
selfsearch$ic <- selfsearch$se * qt((1-0.05)/2 + .5, selfsearch$N-1)
selfsearch$Case <- ifelse(selfsearch$open == 1,ifelse(selfsearch$selfDiscovery==1,"seq. self discovery","seq. other discovery"),"first discovery")

limits <- aes(ymax = selfsearch$mean + selfsearch$ic,
              ymin = selfsearch$mean - selfsearch$ic)

ggplot(selfsearch) +
  geom_bar( aes(x=Case, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Case, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("The search rate self/others discovery")
write.csv(PNPS,"all data processed1.csv")

#high costs only
PNP4C$categories  <- ifelse (PNP4C$Condition == "Patent" & PNP4C$open==0,"patent first discovery",
                             ifelse(PNP4C$Condition == "Patent" &PNP4C$open ==1, "patent  sequential discovery",
                                    ifelse(PNP4C$Condition == "No Patent" &PNP4C$open ==0, "no patent first discovery",
                                           ifelse(PNP4C$Condition == "No Patent" &PNP4C$open ==1, "no patent  sequential discovery",
                                                  ifelse(PNP4C$Condition == "Singleton" &PNP4C$open ==1, "singleton  sequential discovery","singleton first discovery")))))
PNP4CH <- subset(PNP4C)
#PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories,ID)]
PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories)]

PNP4CH$se <- PNP4CH$sd / sqrt(PNP4CH$N)
PNP4CH$ic <- PNP4CH$se * qt((1-0.05)/2 + .5, PNP4CH$N-1)
limits <- aes(ymax = PNP4CH$mean + PNP4CH$ic,
              ymin = PNP4CH$mean - PNP4CH$ic)

ggplot(PNP4CH) +
  geom_bar( aes(x=categories, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=categories, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) 
+ theme(axis.text.x = element_text(angle = 60, hjust = 1)+ 
  ggtitle("Search Rate By the Game state "))

#low costs only
PNP4CH <- subset(PNP4C,C>20)
#PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories,ID)]
PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories)]

PNP4CH$se <- PNP4CH$sd / sqrt(PNP4CH$N)
PNP4CH$ic <- PNP4CH$se * qt((1-0.05)/2 + .5, PNP4CH$N-1)
limits <- aes(ymax = PNP4CH$mean + PNP4CH$ic,
              ymin = PNP4CH$mean - PNP4CH$ic)

ggplot(PNP4CH) +
  geom_bar( aes(x=categories, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=categories, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("search rate in low costs")

#All costs only
PNP4CH <- PNP4C
#PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories,ID)]
PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories)]

PNP4CH$se <- PNP4CH$sd / sqrt(PNP4CH$N)
PNP4CH$ic <- PNP4CH$se * qt((1-0.05)/2 + .5, PNP4CH$N-1)
limits <- aes(ymax = PNP4CH$mean + PNP4CH$ic,
              ymin = PNP4CH$mean - PNP4CH$ic)

ggplot(PNP4CH) +
  geom_bar( aes(x=categories, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=categories, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("search rate by cases")

#high costs only
PNP4CH <- subset(PNP4C,C>=35)
#PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(categories,ID)]
PNP4CH <- PNP4CH[,.(mean=mean(search),sd=sd(search),.N),by = .(Condition)]

PNP4CH$se <- PNP4CH$sd / sqrt(PNP4CH$N)
PNP4CH$ic <- PNP4CH$se * qt((1-0.05)/2 + .5, PNP4CH$N-1)
limits <- aes(ymax = PNP4CH$mean + PNP4CH$ic,
              ymin = PNP4CH$mean - PNP4CH$ic)

ggplot(PNP4CH) +
  geom_bar( aes(x=Condition, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("search rate when the cost is over 35")
summary(Thresholds)
hist(Thresholds$Threshold)

#learning processes
PNP <- as.data.table(PNP)
learn <- PNP[,.(mean=mean(search),sd=sd(search),.N),by=.(Condition,G)]
learn <- subset(learn,Condition == 'No Patent')
learn$se <- learn$sd / sqrt(learn$N)
learn$ic <- learn$se * qt((1-0.05)/2 + .5, learn$N-1)
library(scales)
ggplot(learn,aes(x=factor(G), y=mean, group=1)) +
  geom_bar( aes(x=factor(G), y=mean), stat="identity", fill="forestgreen", alpha=0.5)  +
  geom_errorbar( aes(x=factor(G), ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Exploration rate over time") + scale_y_continuous(limits=c(0.5,0.65),oob = rescale_none) + 
  geom_smooth(method = "nls", formula = y ~ a * x + b, se = F) + xlab("Game") + ylab("Exploration rate")

#g <- g + geom_bar(stat = "identity",
 #                             position = position_dodge(0.9)) +
  #geom_errorbar(limits, position = position_dodge(0.9),
    #            width = 0.25) +
  labs(x = "Number of game", y = "Exploration rate") +
  ggtitle("Learning process") +
  scale_fill_discrete(name = "Condition")
g
#summery statstics of the game measurments
#the average time took to reveal a mine
subset(PNP[,.N,by = .(discovery,Condition)],discovery !=0)[,.(mean=mean(N),sd=sd(N)),by=Condition]
#how many players search at the same mine in no patent condition?
subset(PNP,Condition == "No Patent" & discovery != 0)[,.N,by = .(ID)]
#how many times there are more than one open mine?


PNP <- as.data.table(PNP)
searchrate <- subset(PNP,open == 1)[,.(searchrate=mean(search)),by = .(Condition,C)]
searchrate <- searchrate[,.(searchrate=mean(searchrate),sd = sd(searchrate),.N),by = .(Condition,c)]
searchrate$se <- searchrate$searchrate / sqrt(searchrate$N)
searchrate$ic <- searchrate$se * qt((1-0.05)/2 + .5, searchrate$N-1)

ggplot(searchrate) +
  geom_bar( aes(x=Condition, y=searchrate), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=searchrate-ic, ymax=searchrate+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Overall search rate for sequential discoveries")
table(PNP$Condition,PNP$selfsearch)
