correctGrowthRateMonocot<-function(x){
  
  plantID<-as.numeric(as.factor(sapply(strsplit(x$FileName, split="_"), function(x){x[[1]]})))
  
  #x is an architect object
  
  for (i in 1:max(plantID)){
    
    sub<-x[which(plantID==i), c("Time", "TRL", "L1R", "L2LR")] #subset data to compute growth rate
    sub<-sub[order(sub$Time),] #order data by date
    sub<-rbind(sub[1,], diff(as.matrix(sub)))
    sub[sub<0]<-0 #Negative values to zero
    x[which(plantID==i), c("GRTR", "GR1R", "GR2L")]<-sub[,c("TRL", "L1R", "L2LR")]/sub$Time}
  
  return(x)}