for(i in ls()){
  if(!is.na(get(i)[[1]]) && class(get(i))[1] == "xts" ){
    assign("temp",get(i))
    temp <- data.frame(temp)
    temp<-cbind(temp,"Date" = rownames(temp))
    temp <- temp[c(6,4)]
    rownames(temp) <- NULL
    assign(i, temp)
  }
  else
  {
    rm(list = c(i))
  }
    
}

df<- AAC

for(i in ls()){

  if(!length(colnames(get(i))) > 2){
    print(i)
    df<-merge.data.frame(df,get(i),by="Date", all = TRUE)
  }
  else{
    print(i)
  }

  
}



