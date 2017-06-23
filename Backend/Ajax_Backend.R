#load in the quarterly data
Rus_2000_Quarterly <- read.csv('Russell_2000_Finance_quarterly_Jun22_update.csv')
Rus_2000_Quarterly$X <- NULL
Rus_2000_Annuals<- Rus_2000_Annuals[!duplicated(Rus_2000_Annuals),]
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
#RusQ1$Trend <- seq(1:nrow(RusQ1))# WTF is this? brady?
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
CLUST <- kmeans(AVG.Clean.scaled,centers=8)
AVG_Clusters <- as.data.frame(AVG.tickers)

AVG_Clusters$Cluster <- CLUST$cluster
table(AVG_Clusters$Cluster)
library(cluster)
clusplot(AVG.Clean.scaled,CLUST$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

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


FinHealth <- data.frame(Ticker=Rus_2000_Quarterly$Ticker,
                        Date=Rus_2000_Quarterly$Date,
                        Cluster=Rus_2000_Quarterly$Cluster,Qtr=row.names(Rus_2000_Quarterly),
                        Revenue=Rus_2000_Quarterly$Total.Revenue,Cost.of.Rev=Rus_2000_Quarterly$Cost.of.Revenue..Total,
                        Net.Income=Rus_2000_Quarterly$Net.Income, Diluted.EPS=Rus_2000_Quarterly$Diluted.Normalized.EPS,
                        EBIT=Rus_2000_Quarterly$Total.Revenue-Rus_2000_Quarterly$Total.Operating.Expense)


#############################################    BGINN CLUSTER OPTIMIZATION

FinHealth$Date <- as.Date(FinHealth$Date, format= "%Y-%m-%d")
to.keep.stocks.1<- subset(FinHealth, Date > "2015-12-31" & Qtr=="1")

tickers_to_keep <- (to.keep.stocks.1$Ticker)
to.keep.stocks.2 <- subset(FinHealth, FinHealth$Ticker %in% tickers_to_keep )
to.keep.stocks.2<- droplevels(to.keep.stocks.2)


to.keep.stocks.2$Qtr <- rep(5:1, length.out = nrow(to.keep.stocks.2))
Cluster2 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 1 ),]
Cluster2 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 2 ),]
Cluster3 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 3 ),]
Cluster4 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 4 ),]
Cluster5 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 5 ),]
Cluster6 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 6 ),]
Cluster7 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 7 ),]
Cluster8 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 8 ),]
Cluster9 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 9 ),]
Cluster20 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 10 ),]
Cluster21 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 11 ),]
Cluster22 <- to.keep.stocks.2[ which(to.keep.stocks.2$Cluster == 12 ),]

#cluster optimization
for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Rev.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Revenue[i+1]-Cluster2$Revenue[i])/Cluster2$Revenue[i+1],0)*(.1)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Cost.of.Rev.Gr_perc[i] <- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Cost.of.Rev[i+1]-Cluster2$Cost.of.Rev[i])/Cluster2$Cost.of.Rev[i+1],0)*(.05)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Net.Income.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Net.Income[i+1]-Cluster2$Net.Income[i])/Cluster2$Net.Income[i+1],0)*(.4)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.Diluted.EPS.Gr_perc[i] <- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$Diluted.EPS[i+1]-Cluster2$Diluted.EPS[i])/Cluster2$Diluted.EPS[i+1],0)*(.3)
}

for (i in 1:nrow(Cluster2)) {
  Cluster2$Qtr.EBIT.Gr_perc[i]<- ifelse(Cluster2$Qtr[i+1] < Cluster2$Qtr[i], -(Cluster2$EBIT[i+1]-Cluster2$EBIT[i])/Cluster2$EBIT[i+1],0)*(.15)
}

# Making new 
df_Cluster2 <-NULL
for(i in unique(Cluster2$Ticker))
{
  
  temp <- subset(Cluster2, Cluster2$Ticker ==i)
  temp2 <- as.data.frame(t(temp))
  temp3 <- apply(temp2[c(3:nrow(temp2)),],1,as.numeric) 
  temp4 <- cbind.data.frame(i,(temp3[1,8]-temp3[2,8]),  (temp3[2,8]-temp3[3,8]),  (temp3[3,8]-temp3[4,8]),  (temp3[4,8]-temp3[5,8]),
                            (temp3[1,9]-temp3[2,9]),  (temp3[2,9]-temp3[3,9]),  (temp3[3,9]-temp3[4,9]),  (temp3[4,9]-temp3[5,9]),
                            (temp3[1,10]-temp3[2,10]),  (temp3[2,10]-temp3[3,10]),  (temp3[3,10]-temp3[4,10]),  (temp3[4,10]-temp3[5,10]),
                            (temp3[1,11]-temp3[2,11]),  (temp3[2,11]-temp3[3,11]),  (temp3[3,11]-temp3[4,11]),  (temp3[4,11]-temp3[5,11]),
                            (temp3[1,12]-temp3[2,12]),  (temp3[2,12]-temp3[3,12]),  (temp3[3,12]-temp3[4,12]),  (temp3[4,12]-temp3[5,12]))
  colnames(temp4) <- c("Ticker",
                       "q5_q4_rev_gr_perc_diff", "q4_q3_rev_gr_perc_diff", "q3_q2_rev_gr_perc_diff", "q2_q1_rev_gr_perc_diff",
                       "q5_q4_cost_rev_gr_perc_diff", "q4_q3_cost_rev_gr_perc_diff", "q3_q2_cost_rev_gr_perc_diff", "q2_q1_cost_rev_gr_perc_diff",
                       "q5_q4_netinc_gr_perc_diff", "q4_q3_netinc_gr_perc_diff", "q3_q2_netinc_gr_perc_diff", "q2_q1_netinc_gr_perc_diff",
                       "q5_q4_eps_gr_perc_diff", "q4_q3_eps_gr_perc_diff", "q3_q2_eps_gr_perc_diff", "q2_q1_eps_gr_perc_diff",
                       "q5_q4_ebit_gr_perc_diff", "q4_q3_ebit_gr_perc_diff", "q3_q2_ebit_gr_perc_diff", "q2_q1_ebit_gr_perc_diff"
                       )
  
  df_Cluster2 <-rbind(df_Cluster2,temp4)
  row.names(df_Cluster2) <- NULL
}


#df_Cluster2[,2:41] <- lapply(df_Cluster2[,2:41], function(x) {
#  if(is.factor(x)) as.numeric(as.character(x)) else x
#})
#sapply(df_Cluster2, class)

df_Cluster2[is.na(df_Cluster2)] <- 0

Tickers <- df_Cluster2$Ticker
# if stocks q5 to q4 diff is above 75% quantile, then give them 1, quantile is between .75 and .25, then give them 0.5, and below 0.25, give them 0.
df_clust2_returnsGr <- as.data.frame(lapply(df_Cluster2[,c(2:5,10:21)], function(x) ifelse(x >= quantile(x,probs = c(.75)),1,
                                                                                ifelse(x < quantile(x,probs = c(.75)) & x > quantile(x,probs = c(.25)),0.5,0))))

df_clust2_costGr <- as.data.frame(lapply(df_Cluster2[,c(6:9)], function(x) ifelse(x >= quantile(x,probs = c(.75)),0,
                                                                                ifelse(x < quantile(x,probs = c(.75)) & x > quantile(x,probs = c(.25)),0.5,1))))
CLUSTER2.results<- cbind(Tickers,df_clust2_returnsGr,df_clust2_costGr)

CLUSTER2.results$Sum_Score <- apply(CLUSTER2.results[,2:21],1,sum)



############################

A<- as.data.frame(lapply(df_Cluster2[,2:21],as.numeric(x)))


