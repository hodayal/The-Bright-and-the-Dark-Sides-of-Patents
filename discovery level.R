N <- 23800
discoveryLevel <- data.table(id   = character(N),
                       Condition = character(N),
                       Nplayers = numeric(N),
                       Nround = numeric(N),
                       IsParallel=numeric(N),
                       Finder=numeric(N),
                       Ngame= numeric(N),
                       Ngroup = numeric(N))


#sign the corner discoveries
discoveryLevel$Corner <- ifelse(grepl("\\(0-",discoveryLevel$ID)|
                                  grepl("\\(29-",discoveryLevel$ID)|
                                  grepl("-0\\)",discoveryLevel$ID)|
                                  grepl("-69\\)",discoveryLevel$ID),1,0)
table(discoveryLevel$Corner,discoveryLevel$Condition)
subset(discoveryLevel,Corner == 1)
j <- 1
for (i in 2:length(PNP$IDOpenDiscoveries1)) {
  if(PNP[i,"IDOpenDiscoveries1"] != "."&PNP[i-1,"IDOpenDiscoveries1"] != PNP[i,"IDOpenDiscoveries1"]){
     
    discoveryLevel[j,"id"] <- PNP[i,"IDOpenDiscoveries1"]
    discoveryLevel[j,"Condition"] <- PNP[i,"Condition"]
    discoveryLevel[j,"Ngame"] <- PNP[i,"Ngroup"]
    discoveryLevel[j,"Ngroup"] <- PNP[i,"GroupIndex"]
    j <- j+1
  }
}
library(stringr)
library(dplyr)
library(splitstackshape)
discoveryLevel <- cSplit(discoveryLevel, "id", ")(", "long")[id!=""]
table(discoveryLevel$IsParallel)
discoveryLevel$id <-  gsub('\\(', '', discoveryLevel$id)
discoveryLevel$id <-  gsub('\\)', '', discoveryLevel$id)

discoveryLevel <- unique(discoveryLevel)
for (i in 1:nrow(discoveryLevel)) {
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"]))
  for (j in 1:nrow(temp) ){
    if (grepl(discoveryLevel[i,"id"], temp[j,"Hexagon"])) {
      discoveryLevel[i,"Finder"] <- temp[j,"ID"]
    }
  }
}
discoveryLevel$ID <- with(discoveryLevel,paste0("(", id, ")"))
for (i in 1:nrow(discoveryLevel)) {
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"])&ID == as.numeric(discoveryLevel[i,"Finder"]))
      discoveryLevel[i,"Nround"] <- which(grepl(discoveryLevel[i,'ID'], t(temp[,"IDOpenDiscoveries3"])))[1]-
        which(grepl(discoveryLevel[i,'ID'], t(temp[,"IDOpenDiscoveries1"])))[1]
  
}
discoveryLevel[366,"ID"]
discoveryLevel[366,"Nround"] <- 1
discoveryLevel[268,"Nround"] <- NA

for (i in 1:nrow(discoveryLevel)) {
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"])&ID == as.numeric(discoveryLevel[i,"Finder"]))
  discoveryLevel[i,"IsParallel"] <- 
    max(str_count(as.data.table
                  (subset(temp,grepl(discoveryLevel[i,"id"], temp$IDOpenDiscoveries1)))$IDOpenDiscoveries1, "-"))
}
check <- subset(PNP,Ngroup == 59)
discoveryLevel[,(mean = mean(Nround, na.rm=TRUE)),by=Condition]
discoveryLevel <- as.data.table(discoveryLevel)
mean(discoveryLevel$Nround, na.rm=TRUE )
write.csv(discoveryLevel,"discovery level.csv")
as.numeric(discoveryLevel[1,1])
discoveryLevel[,mean(as.numeric(Nround)),by = Condition]
for (i in 1:nrow(discoveryLevel)) {
  x <- str_extract_all(discoveryLevel[i,"id"],"\\(?[0-9,.]+\\)?")[[1]]
  x <- gsub("\\(","",x)
  x <- gsub("\\)","",x)
  x <- as.numeric(x)
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"]))
  temp1 <- numeric(200)
  for (j in 1:nrow(temp)) {
    if(temp[j,"Hexagon"] != "."){
    y <- str_extract_all(temp[j,"Hexagon"],"\\(?[0-9,.]+\\)?")[[1]]
    y <- gsub("\\(","",y)
    y <- gsub("\\)","",y)
    y <- as.numeric(y)
    if (is_linked(x[1],x[2],y[1],y[2])){
      temp1[j] <- temp[j,"ID"]
    }
    }
    discoveryLevel[i,"Nplayers"] <- length(unique(temp1[temp1 != 0]))
  } 
}

for (i in 1:nrow(discoveryLevel)) {
  x <- str_extract_all(discoveryLevel[i,"id"],"\\(?[0-9,.]+\\)?")[[1]]
  x <- gsub("\\(","",x)
  x <- gsub("\\)","",x)
  x <- as.numeric(x)
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"]))
  temp1 <- numeric(200)
  for (j in 1:nrow(temp)) {
    if(temp[j,"Hexagon"] != "."){
      y <- str_extract_all(temp[j,"Hexagon"],"\\(?[0-9,.]+\\)?")[[1]]
      y <- gsub("\\(","",y)
      y <- gsub("\\)","",y)
      y <- as.numeric(y)
      if (is_linked(x[1],x[2],y[1],y[2])){
        temp1[j] <- temp[j,"ID"]
      }
    }
    discoveryLevel[i,"Nsearch"] <- length(temp1[temp1 != 0])
  } 
}
table(discoveryLevel$Condition,discoveryLevel$Nsearch) 
check <- subset(PNP,Ngroup == 64)
discoveryLevel$Nplayers <- as.numeric(discoveryLevel$Nplayers)
discoveryLevel$Nround <- as.numeric(discoveryLevel$Nround)

summary(discoveryLevel)
discoveryLevel[,mean(Nplayers),by = Condition]
table(discoveryLevel$Ngroup)
#clean the first game in each group
for (i in 1:nrow(discoveryLevel)) {
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"])&ID == as.numeric(discoveryLevel[i,"Finder"]))
  discoveryLevel[i,"order"] <- temp[1,"G"]
}
summary(subset(discoveryLevel,order != 1))
table(subset(discoveryLevel,order != 1)$Condition,subset(discoveryLevel,order != 1)$Nplayers)  
subset(discoveryLevel,order != 1)[,.(Nplayers = mean(Nplayers)),by = Condition]
subset(discoveryLevel,order != 1) %>% 
  group_by(Condition) %>%
  summarise_all("mean",na.rm=TRUE)
subset(discoveryLevel,order == 2)[,mean(Nplayers),by = Condition]
subset(discoveryLevel,order == 3)[,mean(Nplayers),by = Condition]
subset(discoveryLevel,order == 4)[,mean(Nplayers),by = Condition]
subset(discoveryLevel,order == 4&Condition == "Patent")
#failure searching around an open mine
library(stringr)
for (i in 1:nrow(discoveryLevel)) {
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"]) & 
                   (grepl(discoveryLevel[i,'ID'], t(PNP[,"IDOpenDiscoveries1"]))|
                      grepl(discoveryLevel[i,'ID'], t(PNP[,"IDOpenDiscoveries2"]))) & Hive == 1)
  x <- str_extract_all(discoveryLevel[i,"id"],"\\(?[0-9,.]+\\)?")[[1]]
  x <- gsub("\\(","",x)
  x <- gsub("\\)","",x)
  x <- as.numeric(x)
  fail <- 0
  for (j in 1:nrow(temp)) {
    
      y <- str_extract_all(temp[j,"Hexagon"],"\\(?[0-9,.]+\\)?")[[1]]
      y <- gsub("\\(","",y)
      y <- gsub("\\)","",y)
      y <- as.numeric(y)
    if(!is.na(y)&!is.na(x)){
      if (is_linked(x[1],x[2],y[1],y[2])){
        fail <- fail + 1
      }
    }
  }  
  discoveryLevel[i,"Failures"] <- fail
}
warning()
discoveryLevel <- as.data.table(discoveryLevel)
discoveryLevel[,mean(Failures),by = Condition]
table(discoveryLevel$Failures)
temp <- subset(PNP,ID == 312423015 & Ngroup == 50)
install.packages("devtools")
library(devtools) 
install_github("ProcessMiner/nlcor")
library(ggplot2)

for (i in 1:nrow(discoveryLevel)) {
  
  temp <- subset(PNP,Ngroup == as.numeric(discoveryLevel[i,"Ngame"]) & ID == as.numeric(discoveryLevel[i,"Finder"]))
  temp1 <- 0
  for (j in 1:nrow(temp)) {
    if(temp[j,"Hexagon"] != "."){
      if (temp[j,"farsearch"] == 1){
        temp1 <- temp1 + 1
      }
    }
    discoveryLevel[i,"farsearch"] <- ifelse(temp1 > 0 ,1,0)
  } 
}
check <- subset(discoveryLevel,farsearch ==0|Condition == "No Patent")
table(discoveryLevel$farsearch,discoveryLevel$Condition)

ggplot(subset(discoveryLevel, Condition == "No Patent"| farsearch == 0), aes(Nplayers, Nround) ) +
  geom_point() +
  stat_smooth()
ggplot(subset(discoveryLevel, Condition == "No Patent"| farsearch == 0), aes(Nplayers_noise, Nround_noise) ) +
  geom_point() +
  stat_smooth() + ggtitle("Productivity of searchers") +xlab("Nplayers") + ylab("Nround")+theme_grey(base_size = 15) +
  ylim(0, 12)
hist(discoveryLevel$Nround- discoveryLevel$Nround_noise)
ggplot(subset(discoveryLevel, Condition == "No Patent"| farsearch == 0), aes(Nplayers, Failures) ) +
  geom_point() +
  stat_smooth()
#figures 9,10
v1 <- matrix( rnorm(nrow(discoveryLevel),mean=0,sd=0.2),1,nrow(discoveryLevel)) 
v1 <- as.vector(v1)
v3 <- matrix( rnorm(nrow(discoveryLevel),mean=0,sd=0.2),1,nrow(discoveryLevel)) 
v3 <- as.vector(v3)
v2 <- matrix( rnorm(nrow(discoveryLevel),mean=0,sd=0.1),1,nrow(discoveryLevel)) 
v2 <- as.vector(v2)
discoveryLevel$Nround_noise <- discoveryLevel$Nround+v1
discoveryLevel$Failures_noise <- discoveryLevel$Failures+v3
discoveryLevel$Nplayers_noise <- discoveryLevel$Nplayers+v2

ggplot(subset(discoveryLevel, Condition == "No Patent"| farsearch == 0), aes(Nround, Failures,color = Condition) ) +
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ a * x + b, se = F)+ggtitle("Search Efficiency")

ggplot(subset(discoveryLevel, Condition == "No Patent"| farsearch == 0), aes(Nround_noise, Failures_noise,color = Condition) ) +
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ a * x + b, se = F)+
  ggtitle("Search Efficiency")+xlab("Nround")+ylab("Failures")+
  theme_grey(base_size = 15)

Threshold$high <- ifelse(Threshold$max >=0.8,1,0)
mean(Threshold$high)
hist(subset(discoveryLevel,farsearch == 0)$Nround)
ggplot(subset(discoveryLevel,farsearch == 0), aes(Nsearch, fill = Condition)) + geom_density(alpha = 0.2) + 
  ggtitle("Number of searches around a first discovery")
subset(discoveryLevel,Nplayers == 3 & Nround == 5 &farsearch == 1)
x<-subset(discoveryLevel, Condition == "No Patent"| farsearch == 0)

#for each hexagon, how many players search it at the same time
#figure 7
NP <- subset(PNP, Hive != 0)
NP <- as.data.table(NP)
dis_levelNP <- NP[,.(.N),by=.(Condition,Hexagon,Ngroup,GroupIndex,Hive)]
dis_levelNP <- dis_levelNP[,.(freq = .N),by=.(Condition,N,Hive)]
ggplot(subset(dis_levelNP,N>1&Hive==2),aes (N, freq ,fill = Condition)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  xlab("Number of players") + ylab("Frequency") + ggtitle("Multiple searches")+
  scale_x_continuous(breaks=seq(2,4,1))+ theme_grey(base_size = 15)
