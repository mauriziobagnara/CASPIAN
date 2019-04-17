InitializeSpread<-function(Terrestrial_netw_data,Commodities_shape_data,
                           Pallets_netw_data,Container_netw_data,
                           file_init="init_data.Rdata",save_init=TRUE,netw_type=c("all"),
                           dir_data=NULL, netw_data=NULL,Rdata_file=NULL,init_coords,max_dist,save_dir,
                           species_preferences,traffic_type=c("all"),
                           incl_containers=T,Cont_treshold=0,
                           incl_pallets=T,Pall_threshold=0){

  ### load and format shapefiles (takes a while!) ######################################################
  tmp2 <- proc.time()

  ####################################################################
  ### load road and railway network file #############################

  cat("\n Loading network \n")
  roads_shp<-Terrestrial_netw_data

  colnames(roads_shp@data) <- c("FromNode","ToNode","Type","Length","cargo","passengers", "ID")

  if (all(netw_type!=c("all"))) roads_shp<-roads_shp[roads_shp@data$Type%in%netw_type,]

  road_netw <- as.data.table(roads_shp@data)
  road_netw[,Order:=c(1:nrow(roads_shp@data))]

  ## subsetting road/railway network according to traffic_type
  suppressWarnings(
    if (all(traffic_type!=c("all"))) {
      colTraffic<-which(colnames(road_netw)%in%traffic_type)
      road_netw[,Traffic:=rowSums(road_netw[, ..colTraffic])]
    } else {
      road_netw[,Traffic:=rowSums(cbind(cargo,passengers))]
    }
  )
  road_netw[,Traffic:=round((Traffic)*365/12,0)]
  set(road_netw, j=which(colnames(road_netw) %in% c("cargo","passengers")), value=NULL )

  if (any(road_netw[,Length==0])){ #assign length of 10m to segments with length 0
    options(warn=1)
    road_netw[Length==0,Length:=0.01]
    warning("Links of length 0 detected in Terrestrial_netw_data. Their length has been set to 10m. ")
    options(warn=0)
  }

  ## set initial values for spread
  road_netw[,newarrivals:=0]
  road_netw[,newarrivals:=as.numeric(newarrivals)]
  road_netw[,stateFromNode:=0]
  road_netw[,stateToNode:=0]


  ###############################################################
  ## Identify starting nodes ####################################

  cat("\n Initializing node states \n")

  ## Identifying initial invasion segments
  init_segm <- getNeighbourSegmCoord(shapeObj=roads_shp,init_coords=init_coords,max_dist=max_dist)

  ## Assigning initial state=1 to starting nodes ###################
  init_nodes <- road_netw[ID%in%init_segm,c(FromNode,ToNode)] # new
  road_netw[FromNode%in%init_nodes,stateFromNode:=1]
  road_netw[ToNode%in%init_nodes,stateToNode:=1]


  ####################################################################
  ## identify nodes in each cargo area if commodities are considered

  if  (incl_containers==TRUE | incl_pallets==TRUE) {
    cat("\n Initializing trade regions \n")
    CargoAreas<-Commodities_shape_data
    CargoAreas@data$AreaContainer <- as.character(CargoAreas@data$AreaContainer)
    CargoAreas@data$AreaPallet <- as.character(CargoAreas@data$AreaPallet)

    NodesCoords<-getNodesCoord(roads_shp)
    coordinates(NodesCoords)<- ~ Long+Lat
    proj4string(NodesCoords)<-proj4string(CargoAreas)

    Nodes_CargoCell<-over(NodesCoords,CargoAreas)
    Nodes_CargoCell<-as.data.table(cbind(Nodes_CargoCell,NodesCoords@data$nodeID,stringsAsFactors=F))
    setnames(Nodes_CargoCell,c(names(Nodes_CargoCell)[1:2],"NodeID"))

    ## subsetting nodes according to subset of road_netw
    Nodes_CargoCell <- Nodes_CargoCell[NodeID%in%road_netw$ToNode]
  }

  ####################################################################
  ## identify Areas initialy invaded (for pallets only) from init_coords

  if (incl_pallets==TRUE){
    init_coords2<-init_coords
    coordinates(init_coords2)<- ~ Long+Lat
    proj4string(init_coords2)<-proj4string(CargoAreas)
    init_Areas<-as.character(over(init_coords2,CargoAreas)$AreaPallet)
  }

  ###############################################################
  ## Pallets flows ##############################################

  if  (incl_pallets==TRUE) {
    cat("\n Initializing pallets flows \n")
    #load pallet flow
    Pallets_netw<-as.data.table(Pallets_netw_data)#need to update to provide external file here. Perhaps also in network file?
    Pallets_netw$ToArea <- (as.character(Pallets_netw$ToArea))
    Pallets_netw$FromArea <- (as.character(Pallets_netw$FromArea))
    Pallets_netw<-Pallets_netw[FromArea!=ToArea,] #remove traffic from/to same area
    Pallets_netw<-Pallets_netw[ToArea%in%Nodes_CargoCell$AreaPallet,] #subset, keep only areas where there are traffic nodes of the chosen netw_type

    # remove links with number of exchanged pallets per year < than Pall_threshold:
    if (Pall_threshold>0) { Pallets_netw<-Pallets_netw[numPallets>=Pall_threshold,]}

    Pallets_netw[,numPallets:=numPallets/12] #monthly scale
    #assign Area LinkIDs
    Pallets_netw[,AreaLinkID:=paste(FromArea,ToArea,sep="_")]

    # initialize state of Cargo Areas
    Pallets_netw[,stateFromArea:=0]
    Pallets_netw[,stateToArea:=0]
    Pallets_netw[,newarrivals:=0]

    # Update initial state of Cargo Areas
    Pallets_netw[FromArea%in%init_Areas,stateFromArea:=1]
    Pallets_netw[ToArea%in%init_Areas,stateToArea:=1]
  }

  ###############################################################
  ## Container flows ############################################

  if  (incl_containers==TRUE) {

    cat("\n Initializing Containers Flow \n")
    ## container data
    Container_netw<-as.data.table(Container_netw_data) #need to update to provide external file here. Perhaps also in network file?
    Container_netw$ToArea <- as.character(Container_netw$ToArea)
    Container_netw$FromArea <- as.character(Container_netw$FromArea)
    Container_netw<-Container_netw[FromArea!=ToArea,] #remove traffic from/to same area
    Container_netw<-Container_netw[ToArea%in%Nodes_CargoCell$AreaContainer,] #subset, keep only areas where there are traffic nodes of the chosen netw_type

    # remove areas with number of arriving containers per year < than Cont_treshold:
    if (Cont_treshold>0) { Container_netw<-Container_netw[numContainers>=Cont_treshold,]}

    Container_netw[,numContainers:=numContainers/12] #monthly scale
    Container_netw<-as.data.table(aggregate(numContainers ~ ToArea, Container_netw, sum))
    Nodes_ContCell<-Nodes_CargoCell[,c(1,3)]
    colnames(Nodes_ContCell)<-c("ToArea","FromNode")

    setkey(Container_netw,ToArea)
    setkey(Nodes_ContCell,ToArea)
    Container_netw<-merge(Container_netw,Nodes_ContCell,by="ToArea")

    #divide number of container per number of nodes in each area (assumes each nodes gets same number of containers)
    NodesPerArea<-as.data.table(table(Container_netw$ToArea))
    colnames(NodesPerArea)<-c("ToArea","numNodes")
    setkey(NodesPerArea,ToArea)
    Container_netw<-merge(Container_netw,NodesPerArea,by="ToArea")
    Container_netw[,numContainers:=numContainers/numNodes]
    set(Container_netw, j=which(colnames(Container_netw) %in% c("ToArea","numNodes")), value=NULL )
  }
  # setkey(road_netw,FromNode)


  ###############################################################
  ## Calculate suitability of terrestrial habtitats #############

  cat("\n Calculating suitability of habitats \n")
  fpath<-system.file("extdata", package="CASPIAN")
  LCdata <- readRDS(file.path(fpath,"LandCover_RailsRoadsInters_50m.rds"))
  categories <- read.xlsx(file.path(fpath,"clc_legend_categories.xlsx"),sheet=2) # load new categories
  categories <- categories[,c("GRID_CODE","LC_cat_ID")]
  categories<-as.data.table(categories)

  setkey(categories,LC_cat_ID)
  setkey(species_preferences,LC_cat_ID)
  categories <- species_preferences[categories]

  ### assign new land cover categories and species preferences
  LCdata$LCtype <- categories$LC_cat_ID[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
  LCdata$SpecPref <- categories$Species_preferences[match(LCdata$LC_ID,categories$GRID_CODE)] # assign new categories
  LCdata$LCprop <- LCdata$prop * LCdata$SpecPref

  ## calculate suitability of habitats for each segment
  LCdata <- as.data.table(LCdata)
  road_segm_suit <- LCdata[,sum(LCprop),by=list(LinkID)]
  road_segm_suit[V1>1,V1:=1]

  ## merge land cover suitability and road_netw
  colnames(road_segm_suit) <- c("ID","LCsuit")

  setkey(road_segm_suit,ID)
  setkey(road_netw,ID)
  road_netw <- road_segm_suit[road_netw]

  ###########################################################
  ## Combine all relevant data files ########################

  cat("\n Assembling initialization object \n")
  setkey(road_netw,Order)
  roads_shp@data<-road_netw

  if (incl_pallets==FALSE & incl_containers==FALSE){
    init_obj<-list(roads_shp,init_segm)
    names(init_obj)<-c("roads_shp","init_segm")
  } else if (incl_pallets==FALSE & incl_containers==TRUE){
    init_obj<-list(roads_shp,init_segm,Nodes_CargoCell,Container_netw)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Container_netw")
  } else if (incl_pallets==TRUE & incl_containers==FALSE){
    init_obj<-list(roads_shp,init_segm,Nodes_CargoCell,Pallets_netw,init_Areas)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Pallets_netw","init_Areas")
  } else if (incl_pallets==TRUE & incl_containers==TRUE){
    init_obj<-list(roads_shp,init_segm,Nodes_CargoCell,Container_netw,Pallets_netw,init_Areas)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Container_netw","Pallets_netw","init_Areas")
  }

  if (save_init) {
    cat("\n Saving initialization object \n")
    save(init_obj, file = file.path(save_dir,file_init))
  }
  print(proc.time() - tmp2)

  return(init_obj)
}
