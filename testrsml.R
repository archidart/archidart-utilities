#Function to detect roots with length=0 in RSML files
#inputrsml is a character string specifying the path to the folder containing the RSML files

testrsml<-function(inputrsml){
  
  data<-data.frame(rsml=NA, root=NA)
  
  filenames.rsml<-list.files(path=inputrsml, pattern="\\.rsml$")
  path<-rep(inputrsml, length.out=length(filenames.rsml))
  path.rsml<-paste(path, filenames.rsml, sep="/")
  filenamesrsml<-sub(x=filenames.rsml, pattern="\\.rsml$", replacement="")
  message(paste("Number of rsml files in inputrsml:", length(filenames.rsml), sep=" "))
  
  if (length(filenames.rsml)>1) {pb<-txtProgressBar(min=1, max=length(filenames.rsml), style=3)}
  
  j<-0
  
  for (i in 1:length(filenames.rsml)){#For each rsml file
    
    rsml <- xmlToList(xmlParse(path.rsml[i]))
    plants <- rsml$scene
    
    for (r0 in plants){#For each plant
      
      for (r1 in r0){#For each first-order root
        
        if (class(r1)=="list"){
          
          if (length(r1$geometry$polyline)==1){
            j<-j+1
            data[j,]<-c(filenamesrsml[i], as.vector(r1$.attrs)[1])}}
        
      for (r2 in r1){#For each second-order root
        
        if (class(r2)=="list"){
          
          if (length(r2$geometry$polyline)==1){
            j<-j+1
            data[j,]<-c(filenamesrsml[i], as.vector(r2$.attrs)[1])}}
      
      for (r3 in r2){#For each third-order root
        
        if (class(r3)=="list"){
          
          if (length(r3$geometry$polyline)==1){
            j<-j+1
            data[j,]<-c(filenamesrsml[i], as.vector(r3$.attrs)[1])}}}}}}
    
    if (length(filenames.rsml)>1) {setTxtProgressBar(pb, i)}
  }
  
  if (is.na(data[1,1])==TRUE) {message("Status: OK")} else {message(paste("Number of roots to delete (length=0):", nrow(data), sep=" "))}
  
  return(data)}