#Combines link probabilities in both directions

combID<-function(x,shp,alr,var){
#  cat ("\n", which(shp$ID==x), " links out of ", length(shp$ID),"\n")
  cat('\r',which(shp$ID==x), " out of ", length(shp$ID))
  flush.console()
  if (x%in%already==FALSE) {
    b<-which(shp$ID==x) #identify 1st direction
    y<-which(shp$FromNode==shp[b]$ToNode & shp$ToNode==shp[b]$FromNode)#identify 2nd direction

    comb<-shp[c(b,y),get(var)]
    shp[c(b,y),which(colnames(shp)==var):=pUnion(comb)] #combine Pinv
    assign(x = "already",value=c(alr,shp[c(b,y),ID]),envir = .GlobalEnv) #add to already done
  }
}

