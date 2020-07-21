library(data.table)
library(ggplot2)

specification <- function(threshold,cost,search){
  result <- ifelse((cost>threshold&search==0)|(cost<threshold&search==1),1,0)
  return(result)
}
table(PNPS$farsearch)
#Threshold and search by game 

THV <- subset(PNPS,select = c("ID","C","search","open","Condition","G"),farsearch == 0)
for(i in c(3,7,13,17,23,27,33,37)){
  THV$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THV$possibleTH,THV$C,THV$search)) 
}
THV <- cbind(THV,A3,A7,A13,A17,A23,A27,A33,A37)
THV <- as.data.table(THV)
THV1 <- THV[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37)),by=.(open,ID,Condition,G)]
THV2 <- subset(THV1,select = -c(ID,open,Condition,G))
THV1$Threshold = colnames(THV2)[max.col(THV2,ties.method="last")]
THV1$Threshold <- gsub("B","",THV1$Threshold)

THV1$max<-apply(X=THV2, MARGIN=1, FUN=max)

Threshold <- subset(THV1,select = c(open,ID,Threshold,max,Condition,G))

Threshold$Threshold <- as.numeric(Threshold$Threshold)
PNPS <- as.data.table(PNPS)
search <- PNPS[,.(search = mean(search)), by = .(open,Condition,ID,G)]
learn <- merge(Threshold,search)
learn <- learn[,.(threshold = mean(Threshold),sd_th = sd(Threshold),search_rate = mean(search),sd_sr = sd(search),N=.N),by= .(Condition,open,G)]
learn$se_th <- learn$sd_th / sqrt(learn$N)
learn$se_sr <- learn$sd_sr / sqrt(learn$N)
try <- gather(learn, key="measure", value="value", c("threshold", "search_rate"))
devtools::install_github("zeehio/facetscales")
library(g)
library(facetscales)

scales_y <- list(
  `search_rate` = scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, .1)),
  `threshold_rate` = scale_y_continuous(limits = c(15, 35), breaks = seq(15, 35, 5))
)
#ggplot(mpg, aes(displ, cty)) + 
 # geom_point() + 
  #facet_grid_sc(rows = vars(drv), scales = list(y = scales_y))
#my_range <- function(x) { if (max(x) < 1) {seq(0.4, 1, 0.1)} else seq(15, 35, 5) }
ggplot(try, aes(x=G, y=value, fill=factor(open)))+
  geom_bar(stat='identity',position = "dodge")+
  facet_grid(measure~Condition, scales="free")+scale_fill_grey(start=0.5, end=0.1) +
  labs(fill = "sequential discovery")+
  xlab("Game number") + ggtitle("Learning effect")

write.csv(Threshold,"Thresholds.csv")
Threshold1 <- Threshold[,mean(Threshold),by=.(open,condition)]



Thresholds$categories <- ifelse(0.7<=Thresholds$max&Thresholds$max<=0.8,"70%-80%",ifelse(0.8<Thresholds$max&Thresholds$max <=0.9,"80%-90%",ifelse(0.9<Thresholds$max&Thresholds$max <=1,"90%-100%","0-70%"))) 
Threshold1 <- Threshold[,.(mean=mean(Threshold),median=median(Threshold)),by=.(open,Condition,categories)]
Threshold1$condition <- ifelse (Threshold1$Condition == "Patent" & Threshold1$open==0,"patent closed",
                                ifelse(Threshold1$Condition == "Patent" &Threshold1$open ==1, "patent open",
                                       ifelse(Threshold1$Condition == "No Patent" &Threshold1$open ==0, "no patent closed",
                                              ifelse(Threshold1$Condition == "No Patent" &Threshold1$open ==1, "no patent open",
                                                     ifelse(Threshold1$Condition == "Singleton" &Threshold1$open ==1, "singleton open","singleton closed")))))


THmean <- ggplot(Threshold1, aes(fill=condition, y=mean, x=categories)) + 
  geom_bar(position="dodge", stat="identity")
THmean
THmedian <- ggplot(Threshold1, aes(fill=condition, y=median, x=categories)) + 
  geom_bar(position="dodge", stat="identity")
THmedian

table(Threshold$categories)

THVG <- subset(PNPS,select = c("ID","C","search","open","Condition","gender","age","G"),farsearch==0)
for(i in c(3,7,13,17,23,27,33,37)){
  THVG$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THVG$possibleTH,THVG$C,THVG$search)) 
}
THVG <- cbind(THVG,A3,A7,A13,A17,A23,A27,A33,A37)
THVG <- as.data.table(THVG)
THV1G <- THVG[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37)),by=.(open,ID,Condition,age,gender,G)]
THV2G <- subset(THV1G,select = -c(ID,open,Condition,age,gender,G))
THV1G$Threshold = colnames(THV2G)[max.col(THV2G,ties.method="last")]
THV1G$Threshold <- gsub("B","",THV1G$Threshold)

THV1G$max<-apply(X=THV2G, MARGIN=1, FUN=max)

ThresholdG <- subset(THV1G,select = c(open,ID,Threshold,max,Condition,age,gender,G))
ThresholdG$conditionDummy <- ifelse(ThresholdG$Condition=="Patent",1,0)

que8 <- lm(Threshold ~ open + conditionDummy + max + G, data = ThresholdG)
summary(que8)


THV <- subset(PNPS,select = c("ID","C","search","Condition","open"))
for(i in c(3,7,13,17,23,27,33,37)){
  THV$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THV$possibleTH,THV$C,THV$search)) 
}
THV <- cbind(THV,A3,A7,A13,A17,A23,A27,A33,A37)
THV <- as.data.table(THV)
THV1 <- THV[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37)),by=.(ID,Condition,open)]
THV2 <- subset(THV1,select = -c(ID,Condition,open))
THV1$Threshold = colnames(THV2)[max.col(THV2,ties.method="last")]
THV1$Threshold <- gsub("B","",THV1$Threshold)

THV1$max<-apply(X=THV2, MARGIN=1, FUN=max)

Threshold <- subset(THV1,select = c(ID,Threshold,Condition,open))
Threshold <- as.data.table(Threshold)
Threshold$Threshold <- as.numeric(Threshold$Threshold) 
Threshold1 <- Threshold[,.(mean = mean(Threshold),sd = sd(Threshold),N=.N),by= .(Condition,open)]
Threshold1$se <- Threshold1$sd / sqrt(Threshold1$N)
Threshold1$ic <- Threshold1$se * qt((1-0.05)/2 + .5, Threshold1$N-1)

limits <- aes(ymax = Threshold1$mean + Threshold1$ic, ymin=Threshold1$mean - Threshold1$ic)
g <- ggplot(Threshold1, aes(x = factor(open), y = mean,
                                  fill = factor(Condition)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(aes(ymax = Threshold1$mean + Threshold1$ic, ymin=Threshold1$mean - Threshold1$ic), position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Is Open?", y = "mean threshold") +
  ggtitle("Threshold by condition") +
  scale_fill_discrete(name = "Condition")

g


Threshold$conditionDummy <- ifelse(Threshold$Condition=="Patent",1,0)
#deviation from optimal strategy
IDopen <- PNPC[,.N,by = .(ID,open)]

PNPS <- rbind(patent,nopatent,singleton)
THV <- subset(PNPS,select = c("ID","C","search","open","Condition"),farsearch==0)
for(i in c(3,7,13,17,23,27,33,37)){
  THV$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THV$possibleTH,THV$C,THV$search)) 
}
THV <- cbind(THV,A3,A7,A13,A17,A23,A27,A33,A37)
THV <- as.data.table(THV)
THV1 <- THV[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37),N=.N),by=.(open,ID,Condition)]
THV2 <- subset(THV1,select = -c(ID,open,Condition,N))
THV1$Threshold = colnames(THV2)[max.col(THV2,ties.method="last")]
THV1$Threshold <- gsub("B","",THV1$Threshold)

THV1$max<-apply(X=THV2, MARGIN=1, FUN=max)

Threshold <- subset(THV1,select = c(open,ID,Threshold,max,Condition,N))
Threshold$optimal_threshold <- ifelse(Threshold$Condition == "Patent" |Threshold$Condition == "Singleton", 23, ifelse(Threshold$Condition == "No Patent"& Threshold$open == 1,23,17))
Threshold$optimal_threshold <- as.numeric(Threshold$optimal_threshold)
Threshold$Threshold <- as.numeric(Threshold$Threshold)
Threshold$deviation <- Threshold$Threshold-Threshold$optimal_threshold
IDopen <- PNPC[,.N,by = .(ID,open)]
Threshold <- merge(Threshold,IDopen,c("open","ID"))


Threshold2 <- Threshold[,.(mean=weighted.mean(deviation,N),sd=weighted.sd(deviation,N),N=.N),by=.(Condition,ID)]

Threshold2$se <- Threshold2$sd / sqrt(Threshold2$N)

limits <- aes(ymax = Threshold2$mean + Threshold2$se, ymin=Threshold2$mean - Threshold2$se)
g<-ggplot(Threshold2,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))

g

Threshold$deviation_abs <- abs(Threshold$Threshold-Threshold$optimal_threshold)

Threshold2 <- Threshold[,.(mean=weighted.mean(deviation_abs,N),sd=weighted.sd(deviation_abs,N),N=.N),by=Condition]

Threshold2$se <- Threshold2$sd / sqrt(Threshold2$N)
Threshold2$ic <- Threshold2$se * qt((1-0.05)/2 + .5, Threshold2$N-1)


limits <- aes(ymax = Threshold2$mean + Threshold2$ic, ymin=Threshold2$mean - Threshold2$ic)
g<-ggplot(Threshold2,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))

g
Threshold <- as.data.table(Thresholds)


Threshold$Threshold <- as.numeric(Threshold$Threshold)
Threshold2 <- subset(Threshold,!(ID %in% skipSearch))



library(data.table)
library(ggplot2)
library(radiant.data)
#figure with dots #figure 4
Threshold1$se <- Threshold1$sd / sqrt(Threshold1$N)
Threshold1$ic <- Threshold1$se * qt((1-0.05)/2 + .5, Threshold1$N-1)
Threshold1$state <- ifelse(Threshold1$open == 0, "First treasure","Subsequent treasure")
limits <- aes(ymax = Threshold1$mean + Threshold1$ic,
              ymin = Threshold1$mean - Threshold1$ic)
optimal <- data.frame(state = c("Subsequent treasure","Subsequent treasure","Subsequent treasure","Subsequent treasure",
                               "First treasure","First treasure","First treasure","First treasure"),
                      Condition = c("No Patent","No Patent","Patent","Singleton","No Patent","No Patent","Patent","Singleton"),
                      optimal = c(21.28,25.1,20.4,20.4,16,16.5,20.4,20.4))

ThrPlot <- ggplot(data = Threshold1, aes(x = factor(state), y = mean,
                                         fill = factor(Condition)))

ThrPlot <- ThrPlot + geom_bar(stat = "identity",
                              position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "State of the game", y = "Mean threshold") +labs(fill = "Condition")+
  ggtitle("Actual and optimal thresholds") +
  scale_fill_grey(name = "Condition",start=0.4, end=0.8)+ theme_bw()+
  geom_point(data = optimal ,aes(y=optimal),
             stat="identity",
             position=position_dodge(width = .9),
             alpha=.8,
             size=3)+ theme_bw(base_size = 15)
ThrPlot
#figure with dots, no singleton
Threshold2 <- subset(Threshold1,Condition != "Singleton")
Threshold2$se <- Threshold2$sd / sqrt(Threshold2$N)
Threshold2$ic <- Threshold2$se * qt((1-0.05)/2 + .5, Threshold2$N-1)
Threshold2$state <- ifelse(Threshold2$open == 0, "first discovery","sequential discovery")
limits <- aes(ymax = Threshold2$mean + Threshold2$ic,
              ymin = Threshold2$mean - Threshold2$ic)
optimal <- data.frame(state = c("sequential discovery","sequential discovery",
                                "first discovery","first discovery"),
                      Condition = c("No Patent","Patent","No Patent","Patent"),
                      optimal = c(23,20.4,16.3,20.4))

ThrPlot <- ggplot(data = Threshold2, aes(x = factor(state), y = mean,
                                         fill = factor(Condition)))

ThrPlot <- ThrPlot + geom_bar(stat = "identity",
                              position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "state of the game", y = "mean threshold") +labs(fill = "Condition")+
  ggtitle("Actual and optimal threshold") +
  scale_fill_discrete(name = "Condition")+ theme_bw()+
  geom_point(data = optimal ,aes(y=optimal,group=Condition),
             stat="identity",
             position=position_dodge(width = .9),
             alpha=.8,
             size=3)+ theme_bw(base_size = 15)  

ThrPlot
hist(Threshold$max)
hist(Threshold$max, xlab="rate of consistency", main="Histogram of threshold quality", col="lightgreen")
cdf <- ggplot(Threshold, aes(x=max)) + stat_ecdf(aes(x=max))+
  xlab("rate of consistency") + ggtitle("cdf of threshold consistency")+theme_grey(base_size = 15)
cdf
ThresholdPNP <- subset(Threshold,Condition != "Singleton")
uniqeCount <- unique(PNP$ID)
length(uniqeCount)
length(unique(skipSearch))
ThresholdPNP$ConditionDummy <- ifelse(ThresholdPNP$Condition == "Patent",1,0)


Search <- PNPS[,.(mean=mean(search)),by=.(Condition,open,ID)]
Search <- Search[,.(mean=mean(mean),sd=sd(mean),N=.N),by=.(Condition,open)]
Search$se <- Search$sd / sqrt(Search$N)
Search$ic <- Search$se * qt((1-0.05)/2 + .5, Search$N-1)

limits <- aes(ymax = Search$mean + Search$ic,
              ymin = Search$mean - Search$ic)

searchPlot <- ggplot(data = Search, aes(x = factor(open), y = mean,
                                        fill = factor(Condition)))

searchPlot <- searchPlot + geom_bar(stat = "identity",
                                    position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "open discovery", y = "search rate") +
  ggtitle("search rate") +
  scale_fill_discrete(name = "Condition")

searchPlot
#optimal threshold by round
PNP$optimal_threshold <- ifelse(PNP$Condition == "Patent" , 20.42, ifelse(PNP$Condition == "No Patent"& PNP$open == 1,25.1,16.5))
PNP$deviation <- ifelse((PNP$C>PNP$optimal_threshold & PNP$search == 1)|(PNP$C<PNP$optimal_threshold & PNP$search == 0),PNP$C - PNP$optimal_threshold,0)

deviation <- PNP[,.(mean = mean(deviation),sd=sd(deviation),N=.N),by=Condition]
deviation$se <- deviation$sd / sqrt(deviation$N)
deviation$ic <- deviation$se * qt((1-0.05)/2 + .5, deviation$N-1)


limits <- aes(ymax = deviation$mean + deviation$ic, ymin=deviation$mean - deviation$ic)
g<-ggplot(deviation,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))+ggtitle("Deviation from optimal threshold")

g

PNPS$deviationAbs <- ifelse((PNPS$C>PNPS$optimal_threshold & PNPS$search == 1)|(PNPS$C<PNPS$optimal_threshold & PNPS$search == 0),  abs(PNPS$C - PNPS$optimal_threshold),0)
deviationAbs <- subset(PNPS,deviationAbs != 0)[,.(mean = mean(deviationAbs),sd=sd(deviationAbs),N=.N),by=Condition]
deviationAbs$se <- deviationAbs$sd / sqrt(deviationAbs$N)
deviationAbs$ic <- deviationAbs$se * qt((1-0.05)/2 + .5, deviationAbs$N-1)


limits <- aes(ymax = deviationAbs$mean + deviationAbs$ic, ymin=deviationAbs$mean - deviationAbs$ic)
g<-ggplot(deviationAbs,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))

g

deviation1 <- subset(PNPS,deviationAbs != 0)[,.(mean = mean(deviation),sd=sd(deviation),N=.N),by=.(Condition,open)]
deviation1$se <- deviation1$sd / sqrt(deviation1$N)
deviation1$ic <- deviation1$se * qt((1-0.05)/2 + .5, deviation1$N-1)


limits <- aes(ymax = deviation1$mean + deviation1$ic, ymin=deviation1$mean - deviation$ic)
g <- ggplot(data = deviation1, aes(x = factor(Condition), y = mean,
                                   fill = factor(open)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Condition", y = "mean deviation") +
  ggtitle("Deviation by condition - per round measurement") +
  scale_fill_discrete(name = "Is Open?")

g

PNPS$deviationAbs <- ifelse((PNPS$C>PNPS$optimal_threshold & PNPS$search == 1)|(PNPS$C<PNPS$optimal_threshold & PNPS$search == 0),  abs(PNPS$C - PNPS$optimal_threshold),0)
deviationAbs <- subset(PNPS,deviationAbs != 0)[,.(mean = mean(deviationAbs),sd=sd(deviationAbs),N=.N),by=.(Condition,open)]
deviationAbs$se <- deviationAbs$sd / sqrt(deviationAbs$N)
deviationAbs$ic <- deviationAbs$se * qt((1-0.05)/2 + .5, deviationAbs$N-1)


limits <- aes(ymax = deviationAbs$mean + deviationAbs$ic, ymin=deviationAbs$mean - deviationAbs$ic)
g <- ggplot(data = deviationAbs, aes(x = factor(Condition), y = mean,
                                     fill = factor(open)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Condition", y = "mean deviation") +
  ggtitle("Absolute value of deviation by condition - per round") +
  scale_fill_discrete(name = "Is Open?")

g

#always skip & always search players
PNPS <- as.data.table(all_data_processed1)
Threshold4 <- subset(Thresholds,max<0.75)
Threshold5 <- PNPS[,.(search=mean(search)),by=ID]
Threshold6 <- subset(Threshold5,search == 0 | search == 1)
Thresholds$Threshold <- as.numeric(Thresholds$Threshold)

skipSearch <- c(as.vector(Threshold4$ID),as.vector(Threshold6$ID))
length(skipSearch)
data.frame(table(skipSearch))
Threshold6$ID
Threshold4 <- subset(Thresholds,!(ID %in% skipSearch))
PNPS4 <- subset(PNPS,!(ID %in% skipSearch))
PNPS4C <- subset(PNPS4, farsearch == 0)
PNP4C <- subset(PNPS4C, Condition != "Singleton")
skipSearch
length(unique(skipSearch))
write.csv(PNPS,"all data processed.csv")
#threshold
Threshold4$Threshold <- as.numeric(Threshold4$Threshold)
Threshold4 <- subset(Threshold4,Condition != "Singleton")
Threshold4 <- Threshold4[,.(mean = mean(Threshold),sd = sd(Threshold),N=.N),by= .(Condition,open)]
Threshold4$se <- Threshold4$sd / sqrt(Threshold4$N)
Threshold4$ic <- Threshold4$se * qt((1-0.05)/2 + .5, Threshold4$N-1)
#optimal <- data.frame(c("Simulation Patent","Simulation Patent","Simulation No Patent","Simulation No Patent"),c(0,1,0,1),c(20.42,20.42,16.5,23))
#names(optimal)<-c("Condition","open","mean")
#Threshold4 <- rbind(Threshold4,optimal,fill=TRUE)

limits <- aes(ymax = Threshold4$mean + Threshold4$ic, ymin=Threshold4$mean - Threshold4$ic)
g <- ggplot(data = Threshold4, aes(x = factor(Condition), y = mean,
                                  fill = factor(open)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Condition", y = "mean threshold") +
  ggtitle("Threshold by condition") +
  scale_fill_discrete(name = "is sequential?")

g

#payoff

Payoff <- PNPS4C[,.(meanPayoff=mean(Payoff),sd=sd(Payoff),N=.N),by=Condition]
Payoff$se <- Payoff$sd / sqrt(Payoff$N)
Payoff$ic <- Payoff$se * qt((1-0.05)/2 + .5, Payoff$N-1)
Payoff <- subset(Payoff,Condition != "Singleton")
optimal <- data.frame("Simulation",4.748)
names(optimal)<-c("Condition","meanPayoff")
Payoff <- rbind(Payoff,optimal,fill=TRUE)
limits <- aes(ymax = Payoff$meanPayoff + Payoff$ic, ymin=Payoff$meanPayoff - Payoff$ic)
g<-ggplot(Payoff,aes(Condition,meanPayoff))
g<-g+geom_bar(stat = "identity",position = position_dodge())
g<-g+geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))+ggtitle("Mean payoff")
g

#overall search rate
searchrate <- PNPS4[,.(searchrate=mean(search)),by = .(Condition,ID)]
searchrate <- searchrate[,.(searchrate=mean(searchrate),sd = sd(searchrate),.N),by = .(Condition)]
searchrate$se <- searchrate$searchrate / sqrt(searchrate$N)
searchrate$ic <- searchrate$se * qt((1-0.05)/2 + .5, searchrate$N-1)

ggplot(searchrate) +
  geom_bar( aes(x=Condition, y=searchrate), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Condition, ymin=searchrate-ic, ymax=searchrate+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("overall search rate")

table(PNPS4$open,PNPS4$Condition)


#deviation from optimal threshold
Threshold1 <- subset(Threshold,!(ID %in% skipSearch))


Threshold1$optimal_threshold <- ifelse(Threshold1$Condition == "Patent" |Threshold1$Condition == "Singleton", 23, ifelse(Threshold1$Condition == "No Patent"& Threshold1$open == 1,23,17))
Threshold1$optimal_threshold <- as.numeric(Threshold1$optimal_threshold)
Threshold1$Threshold <- as.numeric(Threshold1$Threshold)
Threshold1$deviation <- Threshold1$Threshold-Threshold1$optimal_threshold
#Threshold2 <- Threshold4[,.(deviation=mean(deviation),sd=sd(deviation),N=.N),by=.(Condition,open)]
library(radiant.data)
Threshold2 <- Threshold1[,.(mean=weighted.mean(deviation,N),sd=weighted.sd(deviation,N),N=.N),by=.(Condition,ID)]
Threshold3 <- Threshold2[, .(mean = mean(mean),sd = sd(sd), N =.N), by = Condition]
Threshold3$se <- Threshold3$mean / sqrt(Threshold3$N)
Threshold3$ic <- Threshold3$se * qt((1-0.05)/2 + .5, Threshold3$N-1)

g<-ggplot(Threshold3,aes(Condition,mean))
g<-g+geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar( aes(x=Condition, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("Deviation from optimal threshold \n weighted by the frequency of the discovery types")

g
#learning proccess
First_game <- subset(PNPS,G == 3|G == 4 | G == 5)
THV <- subset(First_game,select = c("ID","C","search","open","Condition","gender","age"),farsearch == 0)
for(i in c(3,7,13,17,23,27,33,37)){
  THV$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THV$possibleTH,THV$C,THV$search)) 
}
THV <- cbind(THV,A3,A7,A13,A17,A23,A27,A33,A37)
THV <- as.data.table(THV)
THV1 <- THV[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37)),by=.(open,ID,Condition,age,gender)]
THV2 <- subset(THV1,select = -c(ID,open,Condition,age,gender))
THV1$Threshold = colnames(THV2)[max.col(THV2,ties.method="last")]
THV1$Threshold <- gsub("B","",THV1$Threshold)

THV1$max<-apply(X=THV2, MARGIN=1, FUN=max)

Threshold <- subset(THV1,select = c(open,ID,Threshold,max,Condition,age,gender))

Threshold$Threshold <- as.numeric(Threshold$Threshold)

Threshold <- Threshold[,.(mean = mean(Threshold),sd = sd(Threshold),N=.N),by= .(Condition,open)]
Threshold$se <- Threshold$sd / sqrt(Threshold$N)
Threshold$ic <- Threshold$se * qt((1-0.05)/2 + .5, Threshold$N-1)

limits <- aes(ymax = Threshold$mean + Threshold$ic, ymin=Threshold$mean - Threshold$ic)
g <- ggplot(data = Threshold, aes(x = factor(open), y = mean,
                                  fill = factor(Condition)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Is Open?", y = "mean threshold") +
  ggtitle("Threshold by condition-last 2 games") +
  scale_fill_discrete(name = "Condition")

g
#deviation by condition, without always skipers/searchers
PNPS4$deviation <- ifelse((PNPS4$C>PNPS4$optimal_threshold & PNPS4$search == 1)|(PNPS4$C<PNPS4$optimal_threshold & PNPS4$search == 0),  PNPS$C - PNPS$optimal_threshold,0)
deviation <- subset(PNPS4,deviation != 0)[,.(mean = mean(deviation),sd=sd(deviation),N=.N),by=.(Condition,open)]
deviation$se <- deviation$sd / sqrt(deviation$N)
deviation$ic <- deviation$se * qt((1-0.05)/2 + .5, deviation$N-1)


limits <- aes(ymax = deviation$mean + deviation$ic, ymin=deviation$mean - deviation$ic)
g <- ggplot(data = deviation, aes(x = factor(Condition), y = mean,
                                     fill = factor(open)))

g <- g + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Condition", y = "mean deviation") +
  ggtitle("Deviation by condition - per round") +
  scale_fill_discrete(name = "Is Open?")

g

#threshold histogram #figure 4
ggplot(data = subset(Threshold,open == 0 & max >=0.7),aes(x = Threshold)) + 
   geom_density(aes(linetype= Condition, color = Condition), size =1) +  ylab("Density") + 
  theme(text = element_text(size=15)) + ggtitle('Cost thresholds in first treasures') + 
  geom_vline(xintercept=20.3, color = 'forest green', linetype= "longdash") + geom_vline(xintercept=16, color = 'red') +
  geom_vline(xintercept=16.5, color = 'red')
ggplot(data = subset(Threshold,open == 1 & max >=0.7),aes(x = Threshold)) + 
  geom_density(aes(linetype= Condition, color = Condition), size =1) +  ylab("Density") + 
  theme(text = element_text(size=15)) + ggtitle('Cost thresholds in subsequent treasures')+
  geom_vline(xintercept=20.3, color = 'forest green', linetype= "longdash") + geom_vline(xintercept=21.28, color = 'red') +
  geom_vline(xintercept=25.1, color = 'red')

#median of threshold
Threshold3 <- Threshold[,.(median = median(Threshold),sd = sd(Threshold),N=.N),by= .(Condition,open)]
Threshold3$state <- ifelse(Threshold3$open == 0, "First treasure","Subsequent treasure")
optimal <- data.frame(state = c("Subsequent treasure","Subsequent treasure","Subsequent treasure","Subsequent treasure","Subsequent treasure",
                                "Subsequent treasure","Subsequent treasure","Subsequent treasure",
                                "First treasure","First treasure","First treasure","First treasure"),
                      Condition = c("No Patent","No Patent","No Patent","No Patent","No Patent","No Patent","Patent","Singleton","No Patent","No Patent","Patent","Singleton"),
                      optimal = c(22,22.76,23.54,24.32,21.28,25.1,20.4,20.4,16,16.5,20.4,20.4))
ThrPlot <- ggplot(data = Threshold3, aes(x = factor(state), y = median,
                                         fill = factor(Condition)))

ThrPlot <- ThrPlot + geom_bar(stat = "identity",
                              position = position_dodge(0.9)) +
    labs(x = "State of the game", y = "Median threshold") +labs(fill = "Condition")+
  ggtitle("Observed and Optimal Thresholds") +
  scale_fill_grey(name = "Condition",start=0.4, end=0.8)+ theme_bw()+
  geom_point(data = optimal ,aes(y=optimal),
             stat="identity",
             position=position_dodge(width = .9),
             alpha=.8,
             size=3)+ theme_bw(base_size = 15)
ThrPlot

#remove the last 12 rounds
THV <- subset(PNPS,select = c("ID","C","search","open","Condition"),farsearch == 0 & R < 39)
for(i in c(3,7,13,17,23,27,33,37)){
  THV$possibleTH <- rep(i)
  nam <- paste("A", i, sep = "")
  assign(nam, specification(THV$possibleTH,THV$C,THV$search)) 
}
THV <- cbind(THV,A3,A7,A13,A17,A23,A27,A33,A37)
THV <- as.data.table(THV)
THV1 <- THV[,.(B3=mean(A3),B7=mean(A7),B13=mean(A13),B17=mean(A17),B23=mean(A23),B27=mean(A27),B33=mean(A33),B37=mean(A37)),by=.(open,ID,Condition)]
THV2 <- subset(THV1,select = -c(ID,open,Condition))
THV1$Threshold = colnames(THV2)[max.col(THV2,ties.method="last")]
THV1$Threshold <- gsub("B","",THV1$Threshold)

THV1$max<-apply(X=THV2, MARGIN=1, FUN=max)

Threshold <- subset(THV1,select = c(open,ID,Threshold,max,Condition,G))

Threshold$Threshold <- as.numeric(Threshold$Threshold)

table(Threshold$Threshold)
table(Thresholds$Threshold)
hist(Threshold$Threshold)
hist(Thresholds$Threshold)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
wilcox.test(subset(Threshold, open == 0 & Condition == 'Patent')$Threshold, mu = 20.42, alternative = "two.sided")
wilcox.test(subset(Threshold, open == 1 & Condition == 'Patent')$Threshold, mu = 20.42, alternative = "two.sided")
wilcox.test(subset(Threshold, open == 0 & Condition == 'No Patent')$Threshold, mu = 16.25, alternative = "two.sided")
wilcox.test(subset(Threshold, open == 1 & Condition == 'No Patent')$Threshold, mu = 23, alternative = "two.sided")
wilcox.test(subset(Threshold, open == 0 & Condition == 'Singleton')$Threshold, mu = 20.42, alternative = "two.sided")
wilcox.test(subset(Threshold, open == 1 & Condition == 'Singleton')$Threshold, mu = 20.42, alternative = "two.sided")
wilcox.test(subset(Thresholds, open == 0 & Condition == 'Patent')$Threshold,
            subset(Thresholds, open == 0 & Condition == 'No Patent')$Threshold,
           alternative = "two.sided")
wilcox.test(subset(Thresholds, open == 1 & Condition == 'Patent')$Threshold,
            subset(Thresholds, open == 1 & Condition == 'No Patent')$Threshold,
            alternative = "two.sided")
wilcox.test(subset(Thresholds, open == 0 & Condition == 'Patent')$Threshold,
            subset(Thresholds, open == 0 & Condition == 'Singleton')$Threshold,
            alternative = "two.sided")
wilcox.test(subset(Thresholds, open == 1 & Condition == 'Patent')$Threshold,
            subset(Thresholds, open == 1 & Condition == 'Singleton')$Threshold,
            alternative = "two.sided")


ThrPlot <- ggplot(data = subset(Threshold3,Condition != 'Singleton'), aes(x = factor(state), y = median,
                                         fill = factor(Condition)))

ThrPlot <- ThrPlot + geom_bar(stat = "identity",
                              position = position_dodge(0.9)) +
  labs(x = "State of the game", y = "Median threshold") +labs(fill = "Condition")+
  ggtitle("Observed and Optimal Thresholds") +
  scale_fill_grey(name = "Condition",start=0.4, end=0.8)+ theme_bw()+
  geom_point(data = subset(optimal,Condition != 'Singleton') ,aes(y=optimal),
             stat="identity",
             position=position_dodge(width = .9),
             alpha=.8,
             size=3)+ theme_bw(base_size = 15)
ThrPlot
Threshold$state <- ifelse(Threshold$open == 0, "First treasure","Subsequent treasure")
p<-ggplot(Threshold, aes(x=factor(state), y = Threshold, fill=factor(Condition))) +
    geom_boxplot() + theme_minimal() + xlab("State of the game")+labs(fill = "Condition")+ 
  theme_bw(base_size = 15)+ scale_fill_grey(name = "Condition",start=0.4, end=0.9) +
  geom_point(data = optimal ,aes(y=optimal),
                                       stat="identity",
                                       position=position_dodge(width = .9),
                                       alpha=.8,
                                       size=3, shape=21,color="black") + 
  ggtitle("Optimal and observed thresholds")
 
p
p<-ggplot(Threshold, aes(x=factor(state), y = Threshold, fill=factor(Condition))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_minimal() + xlab("State of the game")+labs(fill = "Condition")+ 
  theme_bw(base_size = 15)+ scale_fill_grey(name = "Condition",start=0.4, end=0.9) +
  geom_point(data = optimal ,aes(y=optimal),
             stat="identity",
             position=position_dodge(width = .9),
             alpha=.8,
             size=3, shape=21,color="black") + 
  ggtitle("Optimal and observed thresholds")

p
quantile(subset(Threshold,Condition == 'Patent'&open == 0)$Threshold)
quantile(subset(Threshold,Condition == 'Singleton'&open == 0)$Threshold)


wilcox.test(subset(Thresholds, open == 0 & Condition == 'Patent')$Threshold,
            subset(Thresholds, open == 1 & Condition == 'Patent')$Threshold,
            alternative = "two.sided")
wilcox.test(subset(Thresholds, open == 0 & Condition == 'Singleton')$Threshold,
            subset(Thresholds, open == 1 & Condition == 'Singleton')$Threshold,
            alternative = "two.sided")
