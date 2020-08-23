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
