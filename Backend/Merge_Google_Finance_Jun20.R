
getwd()
setwd('/home/ajax/Documents/MVP/Codes/')

as.data.frame(load('/home/ajax/Documents/MVP/Codes/Russell_financials_All.RData'))
vector_russel_2k <- as.data.frame(load('/home/ajax/Documents/MVP/Codes/Russell_financials_All.RData'))
  

library(quantmod)
#TAL.f, SILC.f,
library(plyr)

ls()  
  
#Load the LPL_werk Data


for(i in ls()){
  if(!is.na(get(i)[[1]]) && class(get(i))[1] == "xts" ){
    vector_russel_2k =  c(vector_russel_2k,i)
    rm(list=c(i))
  }
  else{
    if(i != "vector_russel_2k"){
      rm(list=c(i))
    }
    
  }
}

rm(i)


#Load the .f data

checkRow <- function(var_dataFrame){
  
  
  if(nrow(var_dataFrame) <5)
  {
    
    new.row <- data.frame("Dat" = "random", stringsAsFactors=F)
    
    var_dataFrame <- rbind.fill(var_dataFrame, new.row)
    print(ncol(var_dataFrame))
    var_dataFrame <- var_dataFrame[-which(colnames(var_dataFrame)=="Dat")]
    print(ncol(var_dataFrame))
    print(nrow(var_dataFrame))
    
    
  }
  
  
  
  
  if(nrow(var_dataFrame)<5){
    print("hello")
    var_dataFrame <- checkRow(var_dataFrame)
  }
  
  return(var_dataFrame)
}


df <- data.frame()
list_obj = ls()

for(i in vector_russel_2k){
  
  if(any(paste0(i,".f") == list_obj)){
    
    assign("temp",get(paste0(i,".f")))
    temp<-data.frame(t(viewFinancials(temp,type=c("BS"),"A")))
    temp<-cbind(temp,"Date" = rownames(temp))
    temp<-checkRow(temp)
    rownames(temp)<-NULL
    assign("temp_2",get(paste0(i,".f")))
    temp_2<-data.frame(t(viewFinancials(temp_2,type=c("IS"),"A")))
    rownames(temp_2)<-NULL
    temp_2<-checkRow(temp_2)
    temp_2 = cbind(temp,temp_2)
    assign("temp_3",get(paste0(i,".f")))
    temp_3<-data.frame(t(viewFinancials(temp_3,type=c("CF"),"A")))
    rownames(temp_3)<-NULL
    temp_3<-checkRow(temp_3)
    
    temp_3 = cbind(temp_2,temp_3)
    temp_3 = cbind("Ticker"=rep(i,nrow(temp_3)),temp_3)
    print("hey")
    df<- rbind(df,temp_3)
    print("done")
  }
  else{
    next
  }
  
  
  
}


write.csv(df,"Russell_2000_Finance_quarterly_Jun22.csv")

write.csv(df,"Russell_2000_Finance_annually_Jun22.csv")



#rm(GSOL.f)
#rm(SCAI.f)
#rm(HMSY.f)
#rm(EXTR.f)
#rm(TAL.f)
#rm(GLOB.f)
