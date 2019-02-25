correctFirstOrderRoots<-function(x, y){
  
  #x is a rsmlToTable object
  #y is an architect object
  
  if ("rsmlToTable" %in% class(x)){} else {stop("x must be a rsmlToTable object")}
  
  #Create dataframe to store the results
  #res<-data.frame(LPR=NA, LBR=NA, GRPR=NA, GRBR=NA, TNBR=NA, MLBR=NA, MDPR=NA, MDBR=NA,
                  #SPR=NA, SBR=NA, VPR=NA, VBR=NA)
  res<-data.frame()
  
  #LPR: length of the primary root
  #LBR: length of basal roots
  #GRPR: growth rate of the primary root
  #GRBR: growth rate of the basal roots
  #TNPR: total number of primary roots
  #TNBR: total number of basal roots
  #MLBR: mean length of the basal roots
  #MDPR: mean diameter of the primary root
  #MDBR: mean diameter of the basal roots
  #SPR: total surface of the primary roots
  #SBR: total surface of the basal roots
  #VPR: total volume of the primary root
  #VBR: total volume of the basal roots
  
  files<-unique(x$file)

  for (i in files){
    
    pr<-max(x[x$file==i, "parentroot"]) #PR carries all the laterals in the maize images
    
    if (pr==0){#there is just a primary root with no lateral. No basal root.
      
      subPR<-x[x$file==i & x$order==1,]
      subBR<-NULL}
    
    else{#there are laterals, and maybe basal roots
    
      subPR<-x[x$file==i & x$order==1 & x$root==pr,]
      subBR<-x[x$file==i & x$order==1 & x$root!=pr,]}
    
    indexPR<-which(subPR$apic=="true")
    indexBR<-which(subBR$apic=="true")
    
    res1<-data.frame(LPR=sum(subPR$length), 
                     LBR=sum(subBR$length), 
                     GRPR=sum(subPR$length), 
                     GRBR=sum(subBR$length), 
                     TNBR=length(unique(subBR$root)),
                     TNPR=length(unique(subPR$root)),
                     MLBR=sum(subBR$length)/sum(subBR$root!=0), 
                     MDPR=if (is.null(subPR)==FALSE) {if (nrow(subPR)!=0) {mean(c(subPR$diameter1, subPR$diameter2[indexPR]))} else {NA}} else {NA}, 
                     MDBR=if (is.null(subBR)==FALSE) {if (nrow(subBR)!=0) {mean(c(subBR$diameter1, subBR$diameter2[indexBR]))} else {NA}} else {NA},
                     SPR=if (is.null(subPR)==FALSE) {if (nrow(subPR)!=0) {sum(subPR$surface)} else {NA}} else {NA}, 
                     SBR=if (is.null(subBR)==FALSE) {if (nrow(subBR)!=0) {sum(subBR$surface)} else {NA}} else {NA}, 
                     VPR=if (is.null(subPR)==FALSE) {if (nrow(subPR)!=0) {sum(subPR$volume)} else {NA}} else {NA}, 
                     VBR=if (is.null(subBR)==FALSE) {if (nrow(subBR)!=0) {sum(subBR$volume)} else {NA}} else {NA})
    
    res<-rbind(res, res1)}
    res$MLBR[is.na(res$MLBR)==TRUE]<-0

    #Fuse res and y (make new architect table)
    
    res<-res[match(paste(files, "_1", sep=""), y$FileName),]
    y<-data.frame(y, res)
    
    #Update D2LR for monocots
    
    y$D2LR<-y$TNLR/y$LPR
  
  return(y)}