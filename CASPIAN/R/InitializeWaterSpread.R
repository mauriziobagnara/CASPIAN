#INCOMPLETE

InitializeWaterSpread<-function(Water_netw_data,
                                file_init="water_init_data.Rdata",save_init=TRUE, #netw_type=c("all"),
                                dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords,max_dist,save_dir,
                                species_preferences,
                                traffic_type=c("all")
                                ){

  ### load and format shapefiles ######################################################
  tmp2 <- proc.time()

  cat("\n Loading network \n")
  water_shp<-Water_netw_data

  colnames(water_shp@data) <- c("FromNode","ToNode","Motorized", "Non_motorized","Length","ID","Order","CargoToNode",   "velocity","River","Flow", "RiverSegm", "Temperature","Conductivity")

  water_netw <- as.data.table(water_shp@data)
  water_netw[,Order:=c(1:nrow(water_netw))]

  suppressWarnings(
  if (all(traffic_type!=c("all"))) {
    colTraffic<-which(colnames(water_netw)%in%traffic_type)
    water_netw[,Traffic:=rowSums(water_netw[, ..colTraffic])]} else {
                                                    water_netw[,Traffic:=rowSums(cbind(Motorized,Non_motorized))]}
  )
  water_netw[,Traffic:=round((Traffic)/12,0)]  # assuming the data are yearly traffic
  set(water_netw, j=which(colnames(water_netw) %in% c("Motorized","Non_motorized")), value=NULL )

  if (any(water_netw[,Length==0])){ #assign length of 10m to segments with length 0
    options(warn=1)
    water_netw[Length==0,Length:=0.01]
    warning("Links of length 0 detected in Water_netw_data. Their length has been set to 10m. ")
    options(warn=0)
  }

  #water_netw <- water_netw[,.(Von_Knoten,Nach_Knote,Laenge,Typ, Traffic,ID)]

  cat("\n Initializing node states \n")

  water_netw[,newarrivals:=0]
  water_netw[,newarrivals:=as.numeric(newarrivals)]
  water_netw[,stateFromNode:=0]
  water_netw[,stateToNode:=0]

  ###############################################################
  cat("\n Identifying initial invasion segments \n")

  init_segm <- getNeighbourSegmCoord(shapeObj=water_shp,init_coords=init_coords,max_dist=max_dist)

  #Assigning initial state=1 to initially invaded nodes.
  init_nodes <- water_netw[ID%in%init_segm,c(FromNode,ToNode)] # new
  water_netw[FromNode%in%init_nodes,stateFromNode:=1]
  water_netw[ToNode%in%init_nodes,stateToNode:=1]

  ############################################################### # new
  cat("\n Calculating suitability of habitats \n")

  maxTemp <- max(water_netw$Temperature,na.rm=T) # identify max values in network file
  maxCond <- max(water_netw$Conductivity,na.rm=T) # identify max values in network file
  water_netw$Temperature_norm <- water_netw$Temperature/maxTemp # normalise environmental variables to weight them equally
  water_netw$Conductivity_norm <- water_netw$Conductivity/maxCond # normalise environmental variables to weight them equally
  specTemp_norm <- species_preferences$specTemp/maxTemp # normalise species preferences
  specCond_norm <- species_preferences$specCond/maxCond # normalise species preferences

  ## calculate euclidean distance between species preferences and environmental variables
  water_netw$suitability <- 1-sqrt( (specCond_norm - water_netw$Conductivity_norm)^2 + (specTemp_norm - water_netw$Temperature_norm)^2 )

  ###########################################################

  cat("\n Assembling initialization object \n")
  setkey(water_netw,Order)
  water_shp@data<-water_netw

  init_water_data<-list(water_shp,init_segm)
  names(init_water_data)<-c("water_shp","init_segm")

  if (save_init) {
    cat("\n Saving initialization object \n")
    save(init_water_data, file = file.path(save_dir,file_init))
  }
  print(proc.time() - tmp2)

  return(init_water_data)
}
