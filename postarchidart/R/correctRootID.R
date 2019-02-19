correctRootID<-function(x){
  
  #x is a root object
  
  files<-unique(x$file)
  plantID<-unique(x$plant)
  
  for (i in 1:length(files)){
    for (j in 1:length(plantID)){
      
      rootID<-x$root[which(x$file==files[i] & x$plant==plantID[j])]
      parentrootID<-x$parentroot[which(x$file==files[i] & x$plant==plantID[j])]
      newrootID<-c(1:length(rootID))
      newparentrootID<-newrootID[match(parentrootID, rootID)]
      newparentrootID[is.na(newparentrootID)==TRUE]<-0
      
      x$root[which(x$file==files[i] & x$plant==plantID[j])]<-newrootID
      x$parentroot[which(x$file==files[i] & x$plant==plantID[j])]<-newparentrootID}}
  
  return(x)}