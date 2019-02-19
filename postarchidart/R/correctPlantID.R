correctPlantID<-function(x){
  
  #x is a rsmlToTable object
  
  if ("rsmlToTable" %in% class(x)){} else {stop("x must be a rsmlToTable object")}
  
  files<-unique(x$file) #List file names
  nfiles<-length(files) #Total number of rsml files
  
  for (i in 1:nfiles){
    index<-which(x$order[x$file==files[i]]==1 & x$bran[x$file==files[i]]=="true")
    nplant<-length(index) #Number of plants
    xloc<-x$x1[x$file==files[i] & x$order==1 & x$bran=="true"] #x coordinate of the first node of the primary root
    xloc.sort<-sort(xloc) #sort x coordinates
    plantID<-match(xloc.sort, xloc) #find location of each plant in rsml
    
    newplant<-x$plant[x$file==files[i]]
    
    for (j in 1:nplant){
      if (j==nplant){newplant[index[j]:length(newplant)]<-plantID[j]}
      else {newplant[index[j]:(index[j+1]-1)]<-plantID[j]}}
    
    x$plant[x$file==files[i]]<-newplant}
  
  return(x)}