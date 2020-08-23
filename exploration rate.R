#check if two hexagons are linked
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


#patent - sign whether the state of the game is "open" or not
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


