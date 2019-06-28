plotCASPIAN<-function(list_results,shapeObj,variable,save_plot,save_dir,lwd=1){
  #  border_shp <- readOGR(dsn=file.path(dir_data,"gadm36_DEU_shp"),layer="gadm36_DEU_1",stringsAsFactors = F)
  num_col<-5
  #create palette
  norm<-as.character(seq(0,1,length.out=c(10^num_col+1)))
  colfunc <- colorRampPalette(c("green3","gold","darkorange","red3","darkred"))
  color<-colfunc(10^num_col+1)
  pal<-data.table(norm=norm,color=color)
  pal$color<-as.character(pal$color)

  #plotting

  for (i in 1:length(list_results)){
    cat("\n Creating map",i,"out of", length(list_results),"\n")
    time_plot<-proc.time()

    shapeObj@data <- copy(list_results[[i]])
    colNum<-which(colnames(shapeObj@data)==variable)

    cat("\n Combining probabilities of invasion in both directions \n")
    assign(x="already",value=c(),envir=.GlobalEnv)
    already<-c()
    invisible(
      mapply(combID,shapeObj@data$ID,MoreArgs = list(shp=shapeObj@data,alr=already,var=variable))
    )
    setkey(shapeObj@data,Order)

    rm("already",envir = .GlobalEnv)
    # get color palette here
    shapeObj@data$norm <- as.character(round(shapeObj@data[,get(variable)],num_col))

    shapeObj@data<-merge(shapeObj@data,pal,by="norm",all.x=TRUE,sort=FALSE)

    #isolating segments where species has been introduced, not introduced, or has invaded
    shapeObj@data$norm<-as.numeric(shapeObj@data$norm)
    shapeObj@data<-as.data.table(shapeObj@data)
    setkey(shapeObj@data,Order)

    Inv<-shapeObj[shapeObj@data$norm>0,]
    Not_inv<-shapeObj[shapeObj@data$norm==0,]

    if (save_plot==T) {png(filename = file.path(save_dir,(paste0("SpreadModel_map",sprintf("%04d", as.numeric(names(list_results)[i])),".png"))),width=10,height = 8,units = "in",res=c(3*72))
    } else {
      x11(width=10,height = 10)
    }

   layout(matrix(1:2,nrow=1),widths=c(0.2,0.8))

   # legend part
        op <- par(mar=c(0.1,0.1,0.1,0.5))
   num_legend<-40
   color_legend <- rev(c(#"black",
     "darkgray", colfunc(num_legend)))
   xl <- 1
   yb <- 1
   xr <- 1.5
   yt <- 2
   plot(NA,type="n",ann=FALSE,xlim=c(0,1.7),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
   rect(
     xl,
     head(seq(yb,yt,(yt-yb)/(num_legend+1)),-1),
     xr,
     tail(seq(yb,yt,(yt-yb)/(num_legend+1)),-1),
     col= color_legend,
     border = color_legend
   )
   text(x=c(xl-0.1), y = seq(yt,yb,length.out = c(num_legend+1))[c(1,seq(2,c(num_legend+1),length.out = 11))],adj = 1,
        labels = c("Prob = 0",paste(variable, "=",seq(0,1,by=0.1),sep=" ")),cex=.9)

    #actual map plotting
    op <- par(mar=c(0.1,0.2,0.1,0.2))

    plot(Not_inv,xlim=c(xmin(shapeObj),xmax(shapeObj)),ylim=c(ymin(shapeObj),ymax(shapeObj)),
         axes=F,col="darkgray",
         panel.first=rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],lwd=lwd)
         )
    plot(Inv,add=T,col=Inv@data$color,lwd=lwd)
    legend("topleft",c(paste0("Iter. #",names(list_results)[i])),box.col = "white",bg = "white")

    if (save_plot==T) dev.off()
    cat("\n Map",i,"completed \n")
    print(proc.time() - time_plot)
  }
}

#plotCASPIAN(list_results = results[3],shapeObj = Road_Railway_Network[Road_Railway_Network@data$Typ%in%netw_type,],variable="Pinv",save_plot = FALSE)
