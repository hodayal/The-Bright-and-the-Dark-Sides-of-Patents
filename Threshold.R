library(data.table)
library(ggplot2)

specification <- function(threshold,cost,search){
  result <- ifelse((cost>threshold&search==0)|(cost<threshold&search==1),1,0)
  return(result)
}
#table(PNPS$farsearch)
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

#figure 4
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


#threshold histogram 
ggplot(data = subset(Threshold,open == 0 & max >=0.7),aes(x = Threshold)) + 
   geom_density(aes(linetype= Condition, color = Condition), size =1) +  ylab("Density") + 
  theme(text = element_text(size=15)) + ggtitle('Cost thresholds in first treasures') + 
  geom_vline(xintercept=20.3, color = 'forest green', linetype= "longdash") + geom_vline(xintercept=16, color = 'red') +
  geom_vline(xintercept=16.5, color = 'red')

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
