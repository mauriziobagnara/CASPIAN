#calculates the proportion of suitable landcover for establishment for each segment,
# based on an existing list  (as the one provided in data/LClist.Rda),
# and land cover IDs (see data/LClegend.rda).
# Returns a data.table object with proportion of suitable habitat and segments IDs.

LCproportion <- function (IDs,List,LandCoverID){
  ListSub<-subset(List,names(List)%in%IDs)
  X<-sapply(ListSub, function(x) sum(x$prop[x$LC_ID%in%LandCoverID]))
  X<-data.table(ID=names(X), Pe=X,stringsAsFactors = FALSE)
  X[Pe>1,Pe:=1]
  return(X)
}
