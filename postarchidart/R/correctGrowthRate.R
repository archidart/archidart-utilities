correctGrowthRate<-function(x){
  
  #x is an architect object
  
  files<-sapply(strsplit(x$FileName, split="_"), function(x){paste(x[1], x[2], sep="_")})
  plantID<-as.numeric(sapply(strsplit(plant.dicot$FileName, split="_"), function(x){x[3]}))
  
  for (i in 1:max(plantID)){
    
    sub<-x[which(plantID==i), c("Time", "TRL", "L1R", "L2LR", "L3LR")] #subset data to compute growth rate
    sub<-sub[order(sub$Time),] #order data by date
    sub<-rbind(sub[1,], diff(as.matrix(sub)))
    sub[sub<0]<-0 #Negative values to zero
    x[which(plantID==i), c("GRTR", "GR1R", "GR2L", "GR3L")]<-sub[,c("TRL", "L1R", "L2LR", "L3LR")]/sub$Time}
  
  return(x)}