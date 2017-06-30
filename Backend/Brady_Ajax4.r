#load in the quarterly data
Rus_2000_Quarterly <- read.csv('Russell_2000_Finance_quarterly_Jun22_update.csv')
Rus_2000_Quarterly$X <- NULL
#Rus_2000_Annuals<- Rus_2000_Annuals[!duplicated(Rus_2000_Annuals),]
#length(unique(Rus_2000_Quarterly$Ticker))
Rus_2000_Quarterly[(which(is.na(Rus_2000_Quarterly$Date))),]
#unique(levels(Rus_2000_Quarterly$Date))
Rus_2000_Quarterly[is.na(Rus_2000_Quarterly)] <- 0 
Rus_2000_Quarterly$Qtr <- rep(5:1, length.out = nrow(Rus_2000_Quarterly))


#####################################################################################################33
install.packages('caret')
library(caret)

AVG <- aggregate(. ~Ticker,data=Rus_2000_Quarterly[,-44], mean)
#RusQ1 <- subset(Rus_2000_Quarterly, Date == "2016-03-31")

#RusQ1<- RusQ1[!duplicated(RusQ1),]
##################################
##################################
AVG.tickers <- AVG[,1]
AVG <- AVG[,-1]

#RusQ1[is.na(RusQ1)] <- 0 
ColtoDel <- nearZeroVar(AVG)
AVG.Clean <- AVG[,-ColtoDel]

################################################### CLUSTERIG ##########################################

unlist(lapply(AVG.Clean,function(x)sum(is.na(x))))
AVG.Clean.scaled <- scale(AVG.Clean) #standardize variables
# see how many clusters we should center 
set.seed(2020)
within.sum.of.squares <- rep(0,30) 
for (i in 1:30) {
  within.sum.of.squares[i] <- sum( kmeans(AVG.Clean.scaled,centers=i)$withinss )
}
plot(1:30,within.sum.of.squares,xlab="Number of clusters")

set.seed(2016)
CLUST <- kmeans(AVG.Clean.scaled,centers=6)
AVG_Clusters <- as.data.frame(AVG.tickers)

AVG_Clusters$Cluster <- CLUST$cluster
table(AVG_Clusters$Cluster)
library(cluster)
#clusplot(AVG.Clean.scaled,CLUST$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#merge the group with quarterly data
Rus_2000_Quarterly <- merge(Rus_2000_Quarterly,AVG_Clusters,by.x= "Ticker", by.y = "AVG.tickers",all.x = T)
setdiff(unique(Rus_2000_Quarterly$Ticker),AVG_Clusters$AVG.tickers) 
#the reason why thre is a discrepancy in the tickers is that Quarter Dates are not always on the same dates

install.packages('randomUniformForest')
library(randomUniformForest)
#unsupervised learning
AVG.Clean.scaled <- scale(AVG.Clean) 
start<-Sys.time()
AVG.Clean_unsupervised = unsupervised.randomUniformForest(AVG.Clean.scaled[,-44],
                                                          ntree = 1000, nodesize = 10, sparseProximities = TRUE, endModel = "SpectralkMeans",
                                                          bagging = TRUE, clusters = 12, seed = 2016)
Sys.time()-start

AVG.Clean_unsupervised$rUF
## 7 - Look for influential variables (before clustering)
summary(AVG.Clean_unsupervised$rUF)
## assess quality of the clustering and remove/add/merge clusters to see if things are better
#AVG.Clean.scaled$params
AVG_Clusters <- as.data.frame(AVG.tickers)
AVG_Clusters$Cluster <- AVG.Clean_unsupervised$unsupervisedModel$cluster
plot(AVG.Clean_unsupervised)
AVG.Clean_unsupervised
Rus_2000_Quarterly <- merge(Rus_2000_Quarterly,AVG_Clusters,by.x= "Ticker", by.y = "AVG.tickers",all.x = T)
Rus_2000_Quarterly$Cluster.x <- NULL
head(Rus_2000_Quarterly)
FinHealth <- data.frame(Ticker=Rus_2000_Quarterly$Ticker,
                        Date=Rus_2000_Quarterly$Date,
                        Cluster=Rus_2000_Quarterly$Cluster,Qtr=row.names(Rus_2000_Quarterly),
                        Revenue=Rus_2000_Quarterly$Total.Revenue,Cost.of.Rev=Rus_2000_Quarterly$Cost.of.Revenue..Total,
                        Net.Income=Rus_2000_Quarterly$Net.Income, Diluted.EPS=Rus_2000_Quarterly$Diluted.Normalized.EPS,
                        EBIT=Rus_2000_Quarterly$Total.Revenue-Rus_2000_Quarterly$Total.Operating.Expense)

FinHealth$Qtr <- rep(5:1, length.out = nrow(to.keep.stocks.2))
#############################################
?as.Date
FinHealth$Date 
to.keep.stocks.1<- subset(FinHealth, Date > "2015-12-31" & Qtr=="1")

tickers_to_keep <- (to.keep.stocks.1$Ticker)
to.keep.stocks.2 <- subset(FinHealth, FinHealth$Ticker %in% tickers_to_keep )
to.keep.stocks.2<- droplevels(to.keep.stocks.2)

getOption("max.print")
to.keep.stocks.2$Qtr <- rep(5:1, length.out = nrow(to.keep.stocks.2))
Cluster1 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 1 ),]
Cluster2 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 2 ),]
Cluster3 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 3 ),]
Cluster4 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 4 ),]
Cluster5 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 5 ),]
Cluster6 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 6 ),]
Cluster7 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 7 ),]
Cluster8 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 8 ),]
Cluster9 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 9 ),]
Cluster10 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 10 ),]
Cluster11 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 11 ),]
Cluster12 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 12 ),]

#cluster optimization
#Cluster1$Qtr.Rev.Gr <- c( -diff(Cluster1$Revenue),NA)
#Cluster1$Qtr.Cost.of.Rev.Gr <- c(-diff(Cluster1$Cost.of.Rev),NA)
#Cluster1$Qtr.Net.Income.Gr <- c(-diff(Cluster1$Cost.of.Rev),NA)
#Cluster1$Qtr.Diluted.EPS.Gr <- c(-diff(Cluster1$Diluted.EPS),NA)
#Cluster1$Qtr.EBIT.Gr <- c(-diff(Cluster1$EBIT),NA)



#### Codes for cluster 1 
for (i in 1:nrow(Cluster1)) {
   Cluster1$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster1$Qtr[i+1] < Cluster1$Qtr[i], -(Cluster1$Revenue[i+1]-Cluster1$Revenue[i])/Cluster1$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster1)) {
  Cluster1$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster1$Qtr[i+1] < Cluster1$Qtr[i], -(Cluster1$Cost.of.Rev[i+1]-Cluster1$Cost.of.Rev[i])/Cluster1$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster1)) {
  Cluster1$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster1$Qtr[i+1] < Cluster1$Qtr[i], -(Cluster1$Net.Income[i+1]-Cluster1$Net.Income[i])/Cluster1$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster1)) {
  Cluster1$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster1$Qtr[i+1] < Cluster1$Qtr[i], -(Cluster1$Diluted.EPS[i+1]-Cluster1$Diluted.EPS[i])/Cluster1$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster1)) {
  Cluster1$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster1$Qtr[i+1] < Cluster1$Qtr[i], -(Cluster1$EBIT[i+1]-Cluster1$EBIT[i])/Cluster1$EBIT[i+1],0)
}
#arpansoni [3:55 PM] 

df_Cluster1 <-NULL
for(i in unique(Cluster1$Ticker))
{
  
  temp <- subset(Cluster1, Cluster1$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
                       )
  
  df_Cluster1 <-rbind(df_Cluster1,temp3)
}
length(df_Cluster1)
df_Cluster1[is.na(df_Cluster1)] <- 0

df_Cluster1[,2:21] <- lapply(df_Cluster1[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster1, class)

Tickers <- df_Cluster1$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster1[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster1[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster1,df_Cluster1$q5_q4_rev_gr_perc>quantile(df_Cluster1$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER1<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER1$Sum_Score <- apply(df_CLUSTER1[,2:21],1,sum)



################################## Cluster 2 

#### Codes for cluster 2 
for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Revenue[i+1]-Cluster2$Revenue[i])/Cluster2$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Cost.of.Rev[i+1]-Cluster2$Cost.of.Rev[i])/Cluster2$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Net.Income[i+1]-Cluster2$Net.Income[i])/Cluster2$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Diluted.EPS[i+1]-Cluster2$Diluted.EPS[i])/Cluster2$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$EBIT[i+1]-Cluster2$EBIT[i])/Cluster2$EBIT[i+1],0)
}

df_Cluster2 <-NULL
for(i in unique(Cluster2$Ticker))
{
  
  temp <- subset(Cluster2, Cluster2$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster2 <-rbind(df_Cluster2,temp3)
}
length(df_Cluster2)
df_Cluster2[is.na(df_Cluster2)] <- 0

df_Cluster2[,2:21] <- lapply(df_Cluster2[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster2, class)

Tickers <- df_Cluster2$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster2[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster2[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster1,df_Cluster1$q5_q4_rev_gr_perc>quantile(df_Cluster1$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER2<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER2$Sum_Score <- apply(df_CLUSTER2[,2:21],1,sum)


######################## Cluster 3
#### Codes for cluster 3
for (i in 1:nrow(Cluster3)) {
  Cluster3$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster3$Qtr[i+1] < Cluster3$Qtr[i], -(Cluster3$Revenue[i+1]-Cluster3$Revenue[i])/Cluster3$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster3)) {
  Cluster3$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster3$Qtr[i+1] < Cluster3$Qtr[i], -(Cluster3$Cost.of.Rev[i+1]-Cluster3$Cost.of.Rev[i])/Cluster3$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster3)) {
  Cluster3$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster3$Qtr[i+1] < Cluster3$Qtr[i], -(Cluster3$Net.Income[i+1]-Cluster3$Net.Income[i])/Cluster3$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster3)) {
  Cluster3$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster3$Qtr[i+1] < Cluster3$Qtr[i], -(Cluster3$Diluted.EPS[i+1]-Cluster3$Diluted.EPS[i])/Cluster3$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster3)) {
  Cluster3$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster3$Qtr[i+1] < Cluster3$Qtr[i], -(Cluster3$EBIT[i+1]-Cluster3$EBIT[i])/Cluster3$EBIT[i+1],0)
}

#structure
df_Cluster3 <-NULL
for(i in unique(Cluster3$Ticker))
{
  
  temp <- subset(Cluster3, Cluster3$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster3 <-rbind(df_Cluster3,temp3)
}
length(df_Cluster3)
df_Cluster3[is.na(df_Cluster3)] <- 0

df_Cluster3[,2:21] <- lapply(df_Cluster3[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster3, class)

Tickers <- df_Cluster3$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster3[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster3[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER3<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER3$Sum_Score <- apply(df_CLUSTER3[,2:21],1,sum)



######################## Cluster 4
#### Codes for cluster 4
for (i in 1:nrow(Cluster4)) {
  Cluster4$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster4$Qtr[i+1] < Cluster4$Qtr[i], -(Cluster4$Revenue[i+1]-Cluster4$Revenue[i])/Cluster4$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster4)) {
  Cluster4$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster4$Qtr[i+1] < Cluster4$Qtr[i], -(Cluster4$Cost.of.Rev[i+1]-Cluster4$Cost.of.Rev[i])/Cluster4$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster4)) {
  Cluster4$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster4$Qtr[i+1] < Cluster4$Qtr[i], -(Cluster4$Net.Income[i+1]-Cluster4$Net.Income[i])/Cluster4$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster4)) {
  Cluster4$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster4$Qtr[i+1] < Cluster4$Qtr[i], -(Cluster4$Diluted.EPS[i+1]-Cluster4$Diluted.EPS[i])/Cluster4$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster4)) {
  Cluster4$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster4$Qtr[i+1] < Cluster4$Qtr[i], -(Cluster4$EBIT[i+1]-Cluster4$EBIT[i])/Cluster4$EBIT[i+1],0)
}

#structure
df_Cluster4 <-NULL
for(i in unique(Cluster4$Ticker))
{
  
  temp <- subset(Cluster4, Cluster4$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster4 <-rbind(df_Cluster4,temp3)
}
length(df_Cluster4)
df_Cluster4[is.na(df_Cluster4)] <- 0

df_Cluster4[,2:21] <- lapply(df_Cluster4[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster4, class)

Tickers <- df_Cluster4$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster4[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster4[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER4<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER4$Sum_Score <- apply(df_CLUSTER4[,2:21],1,sum)



######################## Cluster 5
#### Codes for cluster 5
for (i in 1:nrow(Cluster5)) {
  Cluster5$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster5$Qtr[i+1] < Cluster5$Qtr[i], -(Cluster5$Revenue[i+1]-Cluster5$Revenue[i])/Cluster5$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster5)) {
  Cluster5$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster5$Qtr[i+1] < Cluster5$Qtr[i], -(Cluster5$Cost.of.Rev[i+1]-Cluster5$Cost.of.Rev[i])/Cluster5$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster5)) {
  Cluster5$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster5$Qtr[i+1] < Cluster5$Qtr[i], -(Cluster5$Net.Income[i+1]-Cluster5$Net.Income[i])/Cluster5$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster5)) {
  Cluster5$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster5$Qtr[i+1] < Cluster5$Qtr[i], -(Cluster5$Diluted.EPS[i+1]-Cluster5$Diluted.EPS[i])/Cluster5$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster5)) {
  Cluster5$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster5$Qtr[i+1] < Cluster5$Qtr[i], -(Cluster5$EBIT[i+1]-Cluster5$EBIT[i])/Cluster5$EBIT[i+1],0)
}


#structure
df_Cluster5 <-NULL
for(i in unique(Cluster5$Ticker))
{
  
  temp <- subset(Cluster5, Cluster5$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster5 <-rbind(df_Cluster5,temp3)
}
length(df_Cluster5)
df_Cluster5[is.na(df_Cluster5)] <- 0

df_Cluster5[,2:21] <- lapply(df_Cluster5[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster5, class)

Tickers <- df_Cluster5$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster5[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster5[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER5<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER5$Sum_Score <- apply(df_CLUSTER5[,2:21],1,sum)


######################## Cluster 6
#### Codes for cluster 6
for (i in 1:nrow(Cluster6)) {
  Cluster6$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster6$Qtr[i+1] < Cluster6$Qtr[i], -(Cluster6$Revenue[i+1]-Cluster6$Revenue[i])/Cluster6$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster6)) {
  Cluster6$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster6$Qtr[i+1] < Cluster6$Qtr[i], -(Cluster6$Cost.of.Rev[i+1]-Cluster6$Cost.of.Rev[i])/Cluster6$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster6)) {
  Cluster6$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster6$Qtr[i+1] < Cluster6$Qtr[i], -(Cluster6$Net.Income[i+1]-Cluster6$Net.Income[i])/Cluster6$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster6)) {
  Cluster6$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster6$Qtr[i+1] < Cluster6$Qtr[i], -(Cluster6$Diluted.EPS[i+1]-Cluster6$Diluted.EPS[i])/Cluster6$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster6)) {
  Cluster6$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster6$Qtr[i+1] < Cluster6$Qtr[i], -(Cluster6$EBIT[i+1]-Cluster6$EBIT[i])/Cluster6$EBIT[i+1],0)
}

#structure
df_Cluster6 <-NULL
for(i in unique(Cluster6$Ticker))
{
  
  temp <- subset(Cluster6, Cluster6$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster6 <-rbind(df_Cluster6,temp3)
}
length(df_Cluster6)
df_Cluster6[is.na(df_Cluster6)] <- 0

df_Cluster6[,2:21] <- lapply(df_Cluster6[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster6, class)

Tickers <- df_Cluster6$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster6[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster6[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER6<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER6$Sum_Score <- apply(df_CLUSTER6[,2:21],1,sum)



######################## Cluster 7
#### Codes for cluster 7
for (i in 1:nrow(Cluster7)) {
  Cluster7$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster7$Qtr[i+1] < Cluster7$Qtr[i], -(Cluster7$Revenue[i+1]-Cluster7$Revenue[i])/Cluster7$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster7)) {
  Cluster7$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster7$Qtr[i+1] < Cluster7$Qtr[i], -(Cluster7$Cost.of.Rev[i+1]-Cluster7$Cost.of.Rev[i])/Cluster7$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster7)) {
  Cluster7$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster7$Qtr[i+1] < Cluster7$Qtr[i], -(Cluster7$Net.Income[i+1]-Cluster7$Net.Income[i])/Cluster7$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster7)) {
  Cluster7$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster7$Qtr[i+1] < Cluster7$Qtr[i], -(Cluster7$Diluted.EPS[i+1]-Cluster7$Diluted.EPS[i])/Cluster7$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster7)) {
  Cluster7$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster7$Qtr[i+1] < Cluster7$Qtr[i], -(Cluster7$EBIT[i+1]-Cluster7$EBIT[i])/Cluster7$EBIT[i+1],0)
}

#structure
df_Cluster7 <-NULL
for(i in unique(Cluster7$Ticker))
{
  
  temp <- subset(Cluster7, Cluster7$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster7 <-rbind(df_Cluster7,temp3)
}
length(df_Cluster7)
df_Cluster7[is.na(df_Cluster7)] <- 0

df_Cluster7[,2:21] <- lapply(df_Cluster7[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster7, class)

Tickers <- df_Cluster7$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster7[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster7[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER7<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER7$Sum_Score <- apply(df_CLUSTER7[,2:21],1,sum)


######################## Cluster 8
#### Codes for cluster 8
for (i in 1:nrow(Cluster8)) {
  Cluster8$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster8$Qtr[i+1] < Cluster8$Qtr[i], -(Cluster8$Revenue[i+1]-Cluster8$Revenue[i])/Cluster8$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster8)) {
  Cluster8$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster8$Qtr[i+1] < Cluster8$Qtr[i], -(Cluster8$Cost.of.Rev[i+1]-Cluster8$Cost.of.Rev[i])/Cluster8$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster8)) {
  Cluster8$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster8$Qtr[i+1] < Cluster8$Qtr[i], -(Cluster8$Net.Income[i+1]-Cluster8$Net.Income[i])/Cluster8$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster8)) {
  Cluster8$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster8$Qtr[i+1] < Cluster8$Qtr[i], -(Cluster8$Diluted.EPS[i+1]-Cluster8$Diluted.EPS[i])/Cluster8$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster8)) {
  Cluster8$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster8$Qtr[i+1] < Cluster8$Qtr[i], -(Cluster8$EBIT[i+1]-Cluster8$EBIT[i])/Cluster8$EBIT[i+1],0)
}

#structure
df_Cluster8 <-NULL
for(i in unique(Cluster8$Ticker))
{
  
  temp <- subset(Cluster8, Cluster8$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster8 <-rbind(df_Cluster8,temp3)
}
length(df_Cluster8)
df_Cluster8[is.na(df_Cluster8)] <- 0

df_Cluster8[,2:21] <- lapply(df_Cluster8[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster8, class)

Tickers <- df_Cluster8$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster8[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster8[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER8<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER8$Sum_Score <- apply(df_CLUSTER8[,2:21],1,sum)


######################## Cluster 9
#### Codes for cluster 9
for (i in 1:nrow(Cluster9)) {
  Cluster9$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster9$Qtr[i+1] < Cluster9$Qtr[i], -(Cluster9$Revenue[i+1]-Cluster9$Revenue[i])/Cluster9$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster9)) {
  Cluster9$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster9$Qtr[i+1] < Cluster9$Qtr[i], -(Cluster9$Cost.of.Rev[i+1]-Cluster9$Cost.of.Rev[i])/Cluster9$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster9)) {
  Cluster9$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster9$Qtr[i+1] < Cluster9$Qtr[i], -(Cluster9$Net.Income[i+1]-Cluster9$Net.Income[i])/Cluster9$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster9)) {
  Cluster9$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster9$Qtr[i+1] < Cluster9$Qtr[i], -(Cluster9$Diluted.EPS[i+1]-Cluster9$Diluted.EPS[i])/Cluster9$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster9)) {
  Cluster9$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster9$Qtr[i+1] < Cluster9$Qtr[i], -(Cluster9$EBIT[i+1]-Cluster9$EBIT[i])/Cluster9$EBIT[i+1],0)
}

#structure
df_Cluster9 <-NULL
for(i in unique(Cluster9$Ticker))
{
  
  temp <- subset(Cluster9, Cluster9$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster9 <-rbind(df_Cluster9,temp3)
}
length(df_Cluster9)
df_Cluster9[is.na(df_Cluster9)] <- 0

df_Cluster9[,2:21] <- lapply(df_Cluster9[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster9, class)

Tickers <- df_Cluster9$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster9[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster9[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER9<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER9$Sum_Score <- apply(df_CLUSTER9[,2:21],1,sum)


######################## Cluster 10
#### Codes for cluster 10
for (i in 1:nrow(Cluster10)) {
  Cluster10$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster10$Qtr[i+1] < Cluster10$Qtr[i], -(Cluster10$Revenue[i+1]-Cluster10$Revenue[i])/Cluster10$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster10)) {
  Cluster10$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster10$Qtr[i+1] < Cluster10$Qtr[i], -(Cluster10$Cost.of.Rev[i+1]-Cluster10$Cost.of.Rev[i])/Cluster10$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster10)) {
  Cluster10$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster10$Qtr[i+1] < Cluster10$Qtr[i], -(Cluster10$Net.Income[i+1]-Cluster10$Net.Income[i])/Cluster10$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster10)) {
  Cluster10$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster10$Qtr[i+1] < Cluster10$Qtr[i], -(Cluster10$Diluted.EPS[i+1]-Cluster10$Diluted.EPS[i])/Cluster10$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster10)) {
  Cluster10$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster10$Qtr[i+1] < Cluster10$Qtr[i], -(Cluster10$EBIT[i+1]-Cluster10$EBIT[i])/Cluster10$EBIT[i+1],0)
}

#structure
df_Cluster10 <-NULL
for(i in unique(Cluster10$Ticker))
{
  
  temp <- subset(Cluster10, Cluster10$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster10 <-rbind(df_Cluster10,temp3)
}
length(df_Cluster10)
df_Cluster10[is.na(df_Cluster10)] <- 0

df_Cluster10[,2:21] <- lapply(df_Cluster10[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster10, class)

Tickers <- df_Cluster10$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster10[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster10[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER10<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER10$Sum_Score <- apply(df_CLUSTER10[,2:21],1,sum)


######################## Cluster 11
#### Codes for cluster 11
for (i in 1:nrow(Cluster11)) {
  Cluster11$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster11$Qtr[i+1] < Cluster11$Qtr[i], -(Cluster11$Revenue[i+1]-Cluster11$Revenue[i])/Cluster11$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster11)) {
  Cluster11$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster11$Qtr[i+1] < Cluster11$Qtr[i], -(Cluster11$Cost.of.Rev[i+1]-Cluster11$Cost.of.Rev[i])/Cluster11$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster11)) {
  Cluster11$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster11$Qtr[i+1] < Cluster11$Qtr[i], -(Cluster11$Net.Income[i+1]-Cluster11$Net.Income[i])/Cluster11$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster11)) {
  Cluster11$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster11$Qtr[i+1] < Cluster11$Qtr[i], -(Cluster11$Diluted.EPS[i+1]-Cluster11$Diluted.EPS[i])/Cluster11$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster11)) {
  Cluster11$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster11$Qtr[i+1] < Cluster11$Qtr[i], -(Cluster11$EBIT[i+1]-Cluster11$EBIT[i])/Cluster11$EBIT[i+1],0)
}

#structure
df_Cluster11 <-NULL
for(i in unique(Cluster11$Ticker))
{
  
  temp <- subset(Cluster11, Cluster11$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster11 <-rbind(df_Cluster11,temp3)
}
length(df_Cluster11)
df_Cluster11[is.na(df_Cluster11)] <- 0

df_Cluster11[,2:21] <- lapply(df_Cluster11[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster11, class)

Tickers <- df_Cluster11$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster11[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster11[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER11<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER11$Sum_Score <- apply(df_CLUSTER11[,2:21],1,sum)



######################## Cluster 12
#### Codes for cluster 12
for (i in 1:nrow(Cluster12)) {
  Cluster12$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster12$Qtr[i+1] < Cluster12$Qtr[i], -(Cluster12$Revenue[i+1]-Cluster12$Revenue[i])/Cluster12$Revenue[i+1],0)
}

for (i in 1:nrow(Cluster12)) {
  Cluster12$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster12$Qtr[i+1] < Cluster12$Qtr[i], -(Cluster12$Cost.of.Rev[i+1]-Cluster12$Cost.of.Rev[i])/Cluster12$Cost.of.Rev[i+1],0)
}

for (i in 1:nrow(Cluster12)) {
  Cluster12$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster12$Qtr[i+1] < Cluster12$Qtr[i], -(Cluster12$Net.Income[i+1]-Cluster12$Net.Income[i])/Cluster12$Net.Income[i+1],0)
}

for (i in 1:nrow(Cluster12)) {
  Cluster12$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster12$Qtr[i+1] < Cluster12$Qtr[i], -(Cluster12$Diluted.EPS[i+1]-Cluster12$Diluted.EPS[i])/Cluster12$Diluted.EPS[i+1],0)
}

for (i in 1:nrow(Cluster12)) {
  Cluster12$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster12$Qtr[i+1] < Cluster12$Qtr[i], -(Cluster12$EBIT[i+1]-Cluster12$EBIT[i])/Cluster12$EBIT[i+1],0)
}

#structure
df_Cluster12 <-NULL
for(i in unique(Cluster12$Ticker))
{
  
  temp <- subset(Cluster12, Cluster12$Ticker == i)
  
  #temp <- subset(Cluster1, Cluster1$Ticker %in% c('ABM'))
  temp2 <- as.data.frame(t(temp))
  temp2 <- temp2[,-5]
  temp3 <- cbind.data.frame(i,temp2[10,],temp2[11,],temp2[12,],temp2[13,],temp2[14,])
  colnames(temp3) <- c("Ticker","q5_q4_rev_gr_perc", "q4_q3_rev_gr_perc", "q3_q2_rev_gr_perc", "q2_q1_rev_gr_perc",
                       "q5_q4_cost_rev_gr_perc", "q4_q3_cost_rev_gr_perc", "q3_q2_cost_rev_gr_perc", "q2_q1_cost_rev_gr_perc",
                       "q5_q4_netinc_gr_perc", "q4_q3_netinc_gr_perc", "q3_q2_netinc_gr_perc", "q2_q1_netinc_gr_perc",
                       "q5_q4_eps_gr_perc", "q4_q3_eps_gr_perc", "q3_q2_eps_gr_perc", "q2_q1_eps_gr_perc",
                       "q5_q4_ebit_gr_perc", "q4_q3_ebit_gr_perc", "q3_q2_ebit_gr_perc", "q2_q1_ebit_gr_perc"
  )
  
  df_Cluster12 <-rbind(df_Cluster12,temp3)
}
length(df_Cluster12)
df_Cluster12[is.na(df_Cluster12)] <- 0

df_Cluster12[,2:21] <- lapply(df_Cluster12[,2:21], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df_Cluster12, class)

Tickers <- df_Cluster12$Ticker
df_tick1 <- as.data.frame(lapply(df_Cluster12[,c(2:5,10:21)], function(x) ifelse(x >0, 1, 0)))
df_tick2 <- as.data.frame(lapply(df_Cluster12[,6:9], function(x) ifelse(x >=0, 0, 1)))
?quantile
#filter(df_Cluster3,df_Cluster3$q5_q4_rev_gr_perc>quantile(df_Cluster3$q5_q4_rev_gr_perc,probs = c(.75)))
df_CLUSTER12<- cbind(Tickers,df_tick1,df_tick2)

df_CLUSTER12$Sum_Score <- apply(df_CLUSTER12[,2:21],1,sum)



A1 <- df_CLUSTER1[which(df_CLUSTER1$Sum_Score>= 10),c(1,22)]
A2 <- df_CLUSTER2[which(df_CLUSTER2$Sum_Score>= 10),c(1,22)]
A3 <- df_CLUSTER3[which(df_CLUSTER3$Sum_Score>= 10),c(1,22)]
A4 <- df_CLUSTER4[which(df_CLUSTER4$Sum_Score>= 10),c(1,22)]
A5 <- df_CLUSTER5[which(df_CLUSTER5$Sum_Score>= 10),c(1,22)]
A6 <- df_CLUSTER6[which(df_CLUSTER6$Sum_Score>= 10),c(1,22)]
A7 <- df_CLUSTER7[which(df_CLUSTER7$Sum_Score>= 10),c(1,22)]
A8 <- df_CLUSTER8[which(df_CLUSTER8$Sum_Score>= 10),c(1,22)]
A9 <- df_CLUSTER9[which(df_CLUSTER9$Sum_Score>= 10),c(1,22)]
A10 <- df_CLUSTER10[which(df_CLUSTER10$Sum_Score>= 10),c(1,22)]
A11 <- df_CLUSTER11[which(df_CLUSTER11$Sum_Score>= 10),c(1,22)]
A12 <- df_CLUSTER12[which(df_CLUSTER12$Sum_Score>= 10),c(1,22)]

Portfolio <- rbind(A1, A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)
(Portfolio)

t(Portfolio)
Portfolio <- t(Portfolio)
Portfolio[1,]
colnames(Portfolio) <- Portfolio[1,]


Data <- as.data.frame(Data)
slicedData <- Data[intersect(colnames(Data),colnames(Portfolio))]
slilcedData.naremoved <- slicedData[, -which((is.na(slicedData[1, ])))]
View(slilcedData.naremoved)

returns <- slilcedData.naremoved
dim(returns)

#create vector of row means 
ew_preturns <- rowMeans(returns)

View(returns)
library(tseries)
opt <- portfolio.optim(returns)
returns$CATM <- NULL
returns$DOOR <- NULL
returns$FF <- NULL
returns$HNNA <- NULL
returns$IESC <- NULL
returns$ISIL <- NULL
returns$MENT <- NULL
returns$MGPI <- NULL
returns$ODP <- NULL
returns$SRNE <- NULL
returns$ISLE <- NULL

class(returns)

print(returns)
View(returns)
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)

for ( i in 1:nrow(returns)) {
  returns$wts[i] <- .2079002 
}
w <- returns$wts
portfolio_monthly_returns <- Return.portfolio(returns, weights = w)
portfolio_excess_returns <- Return.excess(returns, Rf = .0003) 
sharpe_ratio_manual <- round(mean(portfolio_excess_returns)/StdDev(portfolio_excess_returns), 4)

print(returns$wts)
