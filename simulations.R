#simulation- patent and singleton
threshold_P <- data.table(seq_5 = (rep(0,7)),seq_10 = (rep(0,7)),seq_15 = (rep(0,7)),
                           seq_20 = (rep(0,7)),seq_25 = (rep(0,7)),seq_30 = (rep(0,7)),seq_35 = (rep(0,7)))
row.names(threshold_P) <- c("frst_5","frst_10","frst_15","frst_20","frst_25","frst_30","frst_35")
discoveries_P <- data.table(seq_5 = (rep(0,7)),seq_10 = (rep(0,7)),seq_15 = (rep(0,7)),
                             seq_20 = (rep(0,7)),seq_25 = (rep(0,7)),seq_30 = (rep(0,7)),seq_35 = (rep(0,7)))

for (s in 1:7) {
  for (t in 1:7){
    threshold_first <- 5*s
    threshold_seq <- 5*t
    
    pay <- c()
    ND <- c()
    for (j in 1:100) {
      
      Nround <- 1
      Ndiscoveries <- 0
      payoff1 <- data.table()
      payoff1[,"payoff0"] <- rep(0,1)
      
      while (Nround <= 50) {
        cost <- as.data.table(floor(runif(1, min=1, max=8))*5)
        find <- sample(1:20,1)
        name <- paste("payoff",Nround,sep = "")
        payoff1[,name] <- ifelse(cost<=threshold_first,ifelse(find == 20 ,320-cost,-1*cost),0)
        print(Nround)
        Nround <- Nround + 1
        find <- ifelse(find == 20 & cost<=threshold_first,1,0)
        if ( find == 1 & Nround<=50){
          print("first")
          Ndiscoveries <- Ndiscoveries + 1
          list <- search_sequential_patent(threshold_seq = threshold_seq, Ndiscoveries = Ndiscoveries, Nround = Nround, payoff1 = payoff1)
          payoff1 <- list$payoff
          Nround <- list$Nround
          Ndiscoveries <- list$Ndiscoveries
        }
      }
      pay1 <- mean(as.matrix(payoff1),na.rm = T)
      pay <- c(pay,pay1)
      ND <- c(ND,Ndiscoveries)
    }
    discoveries_P[s,t] <- mean(ND)
    threshold_P[s,t] <- mean(pay)
    
  }
}
search_sequential_patent <- function(threshold_seq = 0,Ndiscoveries = 0,Nround = 0,payoff1 = data.table()){
  
  gain <- 0
  prob <- data.table(hex1 = 1/6,hex2 = 1/6,hex3 = 1/6,
                     hex4 = 1/6,hex5 = 1/6,hex6 = 1/6)
  choices_cum <- c()
  
  
  while (!(prob[1,"hex1"]==100 & prob[1,"hex2"]==100) & Nround <= 50) {
     print(Nround)
    cost <- sample(1:7,1)*5
    choices<- 0
    if (prob[1,"hex1"]!=100 & prob[1,"hex2"]!=100){
      
     
        choices <- ifelse(cost  <= threshold_seq, 
                                  sample(prob[1,which(prob[1,]!=0)],1),0)
  
    }
    
    else{
      if (prob[1,"hex1"]==100 & prob[1,"hex2"]!=100){
       
          choices <- ifelse(cost  <= threshold_seq, 
                                    ifelse(prob[1,'hex6']==0,2,sample(c(2,6), 1)),0)
      }
      else{
        if (prob[1,"hex1"]!=100 & prob[1,"hex2"]==100){
            choices <- ifelse(cost  <= threshold_seq, 
                                      ifelse(prob[1,'hex3']==0,1,sample(c(1,3), 1)),0)
        }
      }
    }
    #updating payoffs
    
    gain <- ifelse(choices == 1|choices == 2, 80, 0)
                                              
    #updating Ndiscoveries
    print(choices)
    Ndiscoveries <- ifelse(choices==2 & choices==1,Ndiscoveries + 2, 
                           ifelse((choices==2 & choices!=1)|(choices!=2 & choices==1),
                                  Ndiscoveries + 1,Ndiscoveries))
    name <- paste("payoff",Nround,sep = "")
    
    payoff1[,name] <- ifelse(cost<=threshold_seq,-1*cost,0)+gain
    
    choices_cum <- c(choices_cum,choices)
    
    #updating the probabilities
    prob[1,'hex1'] <- ifelse(any(choices_cum==1),100,ifelse(any(choices_cum==2) & any(choices_cum==3),1,
                                                            ifelse(any(choices_cum==2) & !(any(choices_cum==3)),
                                                                   0.5,1/6)))
    prob[1,'hex2'] <- ifelse(any(choices_cum==2),100,ifelse(any(choices_cum==1) & any(choices_cum==6),1,
                                                            ifelse(any(choices_cum==1) & !(any(choices_cum==6)),
                                                                   0.5,1/6)))
    prob[1,'hex3'] <- ifelse(any(choices_cum==1) |any(choices_cum==3),0,ifelse(any(choices_cum==2),0.5,1/6))
    prob[1,'hex4'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) | any(choices_cum==4) ,0,1/6)
    prob[1,'hex5'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) |any(choices_cum==5),0,1/6)
    prob[1,'hex6'] <- ifelse(any(choices_cum==2) |any(choices_cum==6),0,ifelse(any(choices_cum==1),0.5,1/6))
    
    
   
     print(prob)
    Nround <- Nround + 1
  }
  myList <- list("payoff" = payoff1, "Nround" = Nround, "Ndiscoveries" = Ndiscoveries,"choises" = choices_cum, "prob" = prob)
  return(myList)
}
#heat map payoff
threshold_P1 <- as.matrix(threshold_P)
library(reshape2)
co=melt(threshold_P1)
co$Var1 <- paste("first_",5*co$Var1,sep = "")
co$Var1 <- ifelse(co$Var1 == "first_5","first_05",co$Var1)
head(co)
library(scales) # for muted function
ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Payoffs patent") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Payoffs")
#heat map discoveries
discoveries_P1 <- as.matrix(discoveries_P)*4
library(reshape2)
co=melt(discoveries_P1)
co$Var1 <- paste("first_",5*co$Var1,sep = "")
co$Var1 <- ifelse(co$Var1 == "first_5","first_05",co$Var1)
head(co)
library(scales) # for muted function
ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("No. discoveries patent") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="No. discoveries")
#simulation - no patent

 #searching for sequential discoveries

Nround <- 0

search_sequential <- function(threshold_seq = 0,Ndiscoveries = 0,Nround = 0,payoff1 = data.table(rep(NA,4))){
  
  gain <- as.data.table(rep(NA,Nplayers))
  prob <- data.table(hex1 = rep(1/6,4),hex2 = rep(1/6,4),hex3 = rep(1/6,4),
                              hex4 = rep(1/6,4),hex5 = rep(1/6,4),hex6 = rep(1/6,4))
  choices_cum <- data.table(player1 = 0,player2 = 0, player3 = 0, player4 = 0)
  

 while (!(prob[1,"hex1"]==100 & prob[1,"hex2"]==100) & Nround <= 50) {
  # print(Nround)
   cost <- as.data.table(floor(runif(4, min=1, max=8))*5)
    choices<- data.table(rep(0,Nplayers))
   if (prob[1,"hex1"]!=100 & prob[1,"hex2"]!=100){
     
        for (i in 1:4) {
        choices[i,'V1'] <- ifelse(cost[i,'V1']  <= threshold_seq, 
                                  sample(prob[i,which(prob[i,]!=0)],1),0)
        }
   }
   
       else{
   if (prob[1,"hex1"]==100 & prob[1,"hex2"]!=100){
     for (i in 1:Nplayers) {
       choices[i,'V1'] <- ifelse(cost[i,'V1']  <= threshold_seq, 
                                 ifelse(prob[i,'hex6']==0,2,sample(c(2,6), 1)),0)
     }
   }
     else{
       if (prob[1,"hex1"]!=100 & prob[1,"hex2"]==100){
         for (i in 1:Nplayers) {
           choices[i,'V1'] <- ifelse(cost[i,'V1']  <= threshold_seq, 
                                     ifelse(prob[i,'hex3']==0,1,sample(c(1,3), 1)),0)
         }
       }
     }
   }
     #updating payoffs
        
            gain$V1 <- ifelse(choices$V1 == 1, ifelse(sum(choices$V1 == 1)==1,80,
                                                         ifelse(sum(choices$V1 == 1)==2,16,
                                                                ifelse(sum(choices$V1 == 1)==3,4,
                                                                       0))),
                            ifelse(choices$V1 == 2, ifelse(sum(choices$V1 == 2)==1,80,
                                                         ifelse(sum(choices$V1 == 2)==2,16,
                                                                ifelse(sum(choices$V1 == 2)==3,4,
                                                                       0))),0))
      #updating Ndiscoveries
#print(choices)
        Ndiscoveries <- ifelse(any(choices==2) & any(choices==1),Ndiscoveries + 2, 
                               ifelse((any(choices==2) & !any(choices==1))|(!any(choices==2) & any(choices==1)),
                                      Ndiscoveries + 1,Ndiscoveries))
        name <- paste("payoff",Nround,sep = "")
        
     payoff1[,name] <- ifelse(cost$V1<=threshold_seq,-1*cost$V1,0)+gain$V1
     choices <-  transpose(choices)
     names(choices)[1] <- "player1"
     names(choices)[2] <- "player2"
     names(choices)[3] <- "player3"
     names(choices)[4] <- "player4"
     choices_cum <- rbind(choices_cum,choices)
   
   #updating the probabilities
    prob[1,'hex1'] <- ifelse(any(choices_cum==1),100,ifelse(any(choices_cum==2) & any(choices_cum[,1]==3),1,
                                                            ifelse(any(choices_cum==2) & !(any(choices_cum[,1]==3)),
                                                                   0.5,1/6)))
    prob[1,'hex2'] <- ifelse(any(choices_cum==2),100,ifelse(any(choices_cum==1) & any(choices_cum[,1]==6),1,
                                                            ifelse(any(choices_cum==1) & !(any(choices_cum[,1]==6)),
                                                                   0.5,1/6)))
    prob[1,'hex3'] <- ifelse(any(choices_cum==1) |any(choices_cum[,1]==3),0,ifelse(any(choices_cum==2),0.5,1/6))
    prob[1,'hex4'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) | any(choices_cum[,1]==4) ,0,1/6)
    prob[1,'hex5'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) |any(choices_cum[,1]==5),0,1/6)
    prob[1,'hex6'] <- ifelse(any(choices_cum==2) |any(choices_cum[,1]==6),0,ifelse(any(choices_cum==1),0.5,1/6))
    
    
     prob[2,'hex1'] <- ifelse(any(choices_cum==1),100,ifelse(any(choices_cum==2) & any(choices_cum[,2]==3),1,
                                                                                ifelse(any(choices_cum==2) & !(any(choices_cum[,2]==3)),
                                                                                           0.5,1/6)))
     prob[2,'hex2'] <- ifelse(any(choices_cum==2),100,ifelse(any(choices_cum==1) & any(choices_cum[,2]==6),1,
                                                             ifelse(any(choices_cum==1) & !(any(choices_cum[,2]==6)),
                                                                    0.5,1/6)))
     prob[2,'hex3'] <- ifelse(any(choices_cum==1) |any(choices_cum[,2]==3),0,ifelse(any(choices_cum==2),0.5,1/6))
     prob[2,'hex4'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) | any(choices_cum[,2]==4) ,0,1/6)
     prob[2,'hex5'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) |any(choices_cum[,2]==5),0,1/6)
     prob[2,'hex6'] <- ifelse(any(choices_cum==2) |any(choices_cum[,2]==6),0,ifelse(any(choices_cum==1),0.5,1/6))
  
     
     
    prob[3,'hex1'] <- ifelse(any(choices_cum==1),100,ifelse(any(choices_cum==2) & any(choices_cum[,3]==3),1,
                                                            ifelse(any(choices_cum==2) & !(any(choices_cum[,3]==3)),
                                                                   0.5,1/6)))
    prob[3,'hex2'] <- ifelse(any(choices_cum==2),100,ifelse(any(choices_cum==1) & any(choices_cum[,3]==6),1,
                                                            ifelse(any(choices_cum==1) & !(any(choices_cum[,3]==6)),
                                                                   0.5,1/6)))
    prob[3,'hex3'] <- ifelse(any(choices_cum==1) |any(choices_cum[,3]==3),0,ifelse(any(choices_cum==2),0.5,1/6))
    prob[3,'hex4'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) | any(choices_cum[,3]==4) ,0,1/6)
    prob[3,'hex5'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) |any(choices_cum[,3]==5),0,1/6)
    prob[3,'hex6'] <- ifelse(any(choices_cum==2) |any(choices_cum[,3]==6),0,ifelse(any(choices_cum==1),0.5,1/6))
  
    
    prob[4,'hex1'] <- ifelse(any(choices_cum==1),100,ifelse(any(choices_cum==2) & any(choices_cum[,4]==3),1,
                                                            ifelse(any(choices_cum==2) & !(any(choices_cum[,4]==3)),
                                                                   0.5,1/6)))
    prob[4,'hex2'] <- ifelse(any(choices_cum==2),100,ifelse(any(choices_cum==1) & any(choices_cum[,4]==6),1,
                                                            ifelse(any(choices_cum==1) & !(any(choices_cum[,4]==6)),
                                                                   0.5,1/6)))
    prob[4,'hex3'] <- ifelse(any(choices_cum==1) |any(choices_cum[,4]==3),0,ifelse(any(choices_cum==2),0.5,1/6))
    prob[4,'hex4'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) | any(choices_cum[,4]==4) ,0,1/6)
    prob[4,'hex5'] <- ifelse(any(choices_cum==1) |any(choices_cum==2) |any(choices_cum[,4]==5),0,1/6)
    prob[4,'hex6'] <- ifelse(any(choices_cum==2) |any(choices_cum[,4]==6),0,ifelse(any(choices_cum==1),0.5,1/6))
   # print(as.data.frame.matrix(prob))
    Nround <- Nround + 1
 }
  myList <- list("payoff" = payoff1, "Nround" = Nround, "Ndiscoveries" = Ndiscoveries,"choises" = choices_cum, "prob" = prob)
  return(myList)
}
#search for first discoveries
#find the optimal thresholds
threshold_NP <- data.table(seq_5 = (rep(0,7)),seq_10 = (rep(0,7)),seq_15 = (rep(0,7)),
                           seq_20 = (rep(0,7)),seq_25 = (rep(0,7)),seq_30 = (rep(0,7)),seq_35 = (rep(0,7)))
row.names(threshold_NP) <- c("frst_5","frst_10","frst_15","frst_20","frst_25","frst_30","frst_35")
discoveries_NP <- data.table(seq_5 = (rep(0,7)),seq_10 = (rep(0,7)),seq_15 = (rep(0,7)),
                             seq_20 = (rep(0,7)),seq_25 = (rep(0,7)),seq_30 = (rep(0,7)),seq_35 = (rep(0,7)))
for (s in 1:7) {
  for (t in 1:7){
    threshold_first <- 5*s
    threshold_seq <- 5*t
    
    pay <- c()
    ND <- c()
for (j in 1:100) {
  
payoff <- data.table()
payoff[,"payoff0"] <- c(0,0,0,0)
Nround <- 1
Ndiscoveries <- 0
  payoff1 <- data.table()
  payoff1[,"payoff0"] <- c(0,0,0,0)
  
while (Nround <= 50) {
  cost <- as.data.table(floor(runif(4, min=1, max=8))*5)
  find <- as.data.table(sample(1:20,4,replace = T))
  name <- paste("payoff",Nround,sep = "")
  payoff1[,name] <- ifelse(cost$V1<=threshold_first,ifelse(find$V1 == 20 ,320-cost$V1,-1*cost$V1),0)
  #print(Nround)
  Nround <- Nround + 1
  find$V1 <- ifelse(find$V1 == 20 & cost$V1<=threshold_first,1,0)
  if ( any(find$V1 == 1) & Nround<=50){
   # print("first")
    Ndiscoveries <- Ndiscoveries + 1
    list <- search_sequential(threshold_seq = threshold_seq, Ndiscoveries = Ndiscoveries, Nround = Nround, payoff1 = payoff1)
    payoff1 <- list$payoff
    Nround <- list$Nround
    Ndiscoveries <- list$Ndiscoveries
    choices_cum <- rbind(choices_cum,list$choises)
  }
}
pay1 <- mean(as.matrix(payoff1),na.rm = T)
pay <- c(pay,pay1)
ND <- c(ND,Ndiscoveries)
}
discoveries_NP[s,t] <- mean(ND)
threshold_NP[s,t] <- mean(pay)

  }
}
#heat map of payoffs
threshold_NP1 <- as.matrix(threshold_NP)
library(reshape2)
co=melt(threshold_NP1)
co$Var1 <- paste("first_",5*co$Var1,sep = "")
co$Var1 <- ifelse(co$Var1 == "first_5","first_05",co$Var1)
head(co)
library(scales) # for muted function
ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Payoffs as a function of thresholds") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Payoffs")

#heat map of discoveries
discoveries_NP1 <- as.matrix(discoveries_NP)
library(reshape2)
co=melt(discoveries_NP1)
co$Var1 <- paste("first_",5*co$Var1,sep = "")
co$Var1 <- ifelse(co$Var1 == "first_5","first_05",co$Var1)
head(co)
library(scales) # for muted function
ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("No. discoveries as a function of thresholds") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="No. discoveries")