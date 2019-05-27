####################################################################
#### Model settings and parameters #################################
####################################################################

####################################################################
## General model settings ##########################################

makeplot<-TRUE # should model results be plotted as maps at steps of iter_save_?
save_plot<-TRUE # If TRUE, plots are created in the newly created folder as .png files. If FALSE, an x11() device is opened. Only considered if makeplot=TRUE.

save.restart=FALSE #should results be saved in order to resume the simulation at a later stage?
restart=FALSE #Should the simulation be resumed from previously saved results? Results are saved automatically in restart.rData
file_restart=NULL #if restart=TRUE, the FULL path of the file to be read in (previously created by ModelSpread() or runCASPIAN() ).MUST BE an .Rdata file
export_results=c("txt","csv","shp")  #Should results be exported in the newly created folder? Supported values are "csv","txt",and "shp for ESRI Shapefiles.
#This will create one file per each iteration saved.
#For "csv" and "txt", the spatial information (e.g. links coordinates) will NOT be retained.

initialize<-TRUE  # Whether the model should be initialized.
save_init<-FALSE # if initialize=TRUE, should the initialization file be saved?
file_init<- "init_data.Rdata" # if initialize=TRUE, the name of the file to be created by InitializeSpread().
#   in the newly created folder (default  "init_data.rData" if save_init=TRUE). If initialize=FALSE, the FULL path
#   of the file to be read in (created by InitializeSpread() or ModelSpread() ). MUST BE an .Rdata file.

plot_funct_rel <- FALSE # plot functional relationships of dispersal kernels



######################################################################
### Settings for terrestrial model ###################################

runTerrestrialModel<-TRUE # consider terrestrial spread?

## model structure: select pathways of dispersal #####################
incl_attachment<-TRUE # if attachment to vehicles should be considered.
incl_airflow<-TRUE # if vehicle airstream should be considered.
incl_natural<-TRUE #if natural dispersal should be considered.
incl_containers<-FALSE #if container flow should be considered.
incl_pallets<-FALSE #if pallets flow should be considered.

## simulation: set simulation setting ################################
num_iter_T<- 10 # simulation steps. Terrestrial model only.
iter_save_T <- c(1,num_iter_T) # time steps at which results should be stored. Terrestrial model only.

netw_type <- c("Rail", "A") # types of network considered : "Rail" "A"    "B"    "L"    "S"    "K"    "F"    "G"    "X"    "R"    "k"
traffic_type_T <- c("all") # types of traffic considered : "cargo" (trucks and cargo trains), "passengers" (cars and passenger trains),   "all"

## initial conditions ################################################
## load data set of initial distribution of sprecies
init_coords_T <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  # Hamburg Hafen & Berlin airport. Terrestrial Model

max_dist_T<-10^4 # Maximum distance (m) from initial coordinates for a segment to be considered infected. Terrestrial model only.



####################################################################
## Terrestrial parameters ##########################################

############################################################
## WARNING: The parameter values were obtained from       ##
## extensive calibration analyses. Change only with care. ##
############################################################

## Dispersal parameters ####################################
## attachment kernel parameters
par_att0_Roads <- 8.371457e-07 ## pick-up probability on roads
par_att0_Railways<- 9.398072e-06 ## pick-up probability on railways

par_att1 <- 6.627903e-01  # parameter c in Taylor et al. 2012, Model 2
par_att2 <-  -3.681763e-02   # parameter b in Taylor et al. 2012, Model 2. Calculated as average of b, table 2, on paved roads
par_att3 <- 3.569937e-01  # parameter g in Taylor et al. 2012, Model 2

## airflow kernel parameters
par_air0_Roads<-0.001 ## pick-up probability on Roads
par_air0_Railways<-0.005 ## pick-up probability on Railways

par_air1<-2.307 # parameter b in Von Der Lippe et al. 2013, Lognormal. Values for A. altissima
par_air2<-0.724 # parameter a in Von Der Lippe et al. 2013, Lognormal. Values for A. altissima

## natural dispersal kernel parameter
par_nat1<- 1.06 # González-Martínez et al. 2006, P. pinaster. Always >0
par_nat2<- 4.813674e-01 # González-Martínez et al . 2006, P. pinaster.   >1: thin-tailed ; <1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)

## parameter for introduction by container
par_cont<-10^3 #increase for lower container probability (not fitted)
## parameter for introduction by pallet
par_pall<-10^3 #increase for lower pallet probability (not fitted)

## Treshold for container volume: all areas with number of containers arriving per year lower than Cont_treshold will not be considered
Cont_treshold<-5
## Treshold for pallets volume: all links with number of pallets exchanged per year lower than Pall_threshold will not be considered
Pall_threshold<-5

## Terrestrial establishment scale parameter ################
par_est_T<- 9.409051e-01 # <=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.


## Set landcover IDs suitability for establishment ##########

Urban_areas	<- 1    #	LC_cat_ID=1
Arable_land	<-	0.5 #LC_cat_ID=2
Pastures	<-	0.1 #LC_cat_ID=3
Forests	<-	0   #LC_cat_ID=4
Wetlands	<-	0   #LC_cat_ID=5



######################################################################
### Settings for aquatic model #######################################

runAquaticModel<-TRUE # consider aquatic spread?

## model structure: select pathways of dispersal #####################
incl_natural_water<-TRUE #if natural dispersal along rivers should be considered.
incl_hullfouling <-TRUE #if hull-fouling dispersal along rivers should be considered.
incl_ballast<-TRUE #if ballast water should be considered.

## simulation: set simulation setting ################################
num_iter_W<- 10 # simulation steps. Acquatic model only.
iter_save_W <- c(1,num_iter_W) # time steps at which results should be stored. Terrestrial model only.

traffic_type_W <- c("all") # types of traffic considered : "Motorized", "Non-motorized",   "all"

## initial conditions ################################################
## load data set of initial distribution of sprecies
init_coords_W <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  # Hamburg Hafen & Berlin airport. Acquatic Model
max_dist_W<-10^3 # Maximum distance (m) from initial coordinates for a segment to be considered infected. Acquatic model only.



######################################################################
# Aquatic Parameters: ################################################

############################################################
## WARNING: The parameter values were obtained from       ##
## extensive calibration analyses. Change only with care. ##
############################################################

## Dispersal parameters ####################################
## Natural dispersal: see Elliot 2003, eq. 2a
par_nat_a<- 6.104603e+00 # scale parameter for Gammarus spp (Elliot 2003, Tab.1)
par_nat_b<- 3.483644e+00 # shape parameter for Gammarus spp (Elliot 2003, Tab.1)


<<<<<<< HEAD
## Introduction through hull-fouling: see Sylvester 2011, eq. 9
=======
#hull-fouling: see Sylvester 2011, eq. 9
par_hull0 <- 0.000001 ## pick-up probability
>>>>>>> master
par_a <-5.85 * 10^-20
par_c1 <-20.9
par_g <-1.03 * 10^-10
par_c2 <-3.63
par_b <-2.569536e+00  # Sylvester 2011 used 3.15 * 10^-7
par_c3 <-2.311053e+00 # ideally <3 and >2 (arbitrarily)

## The following factors related to hull fouling are implemented but not
## tested due to the lack of data, and therefore not considered in the
## current version. If data are available, the data has to be provided in
## correct order. Depending on the kind of data, this could be changed by
## incorporating the data directly into the shapefile.
Port_time<-NA # Time spent in port. Set to strictly >0 if data are provided, otherwise NA
Paint_time<-NA # Time since last antifouling painting. Set to strictly >=0 if data are provided, otherwise NA

## parameter for introduction by ballast water
par_ball<-8.253108e+04 # shape parameter

## Aquatic establishment scale parameter
par_est_W<- 7.001148e-01 #arbitrary,<=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.



#### Initialization info #################################

## inclue readOGR in InitializeSpread for data


Terrestrial_netw_data <- "RailRoadNetw_Intersection_100519.shp" # A shapefile containing a terrestrial traffic network; will be read in as a SpatialLinesDataFrame object
Water_netw_data <- "Waterways_Netw.shp"

env_terrestrial <- ""

Commodities_shape_data<-Cargo_shp # file has three names: Commodities_shape_data, CargoAreas and Cargo_shp
Pallets_netw_data<-PalletsFlow
Container_netw_data<-ContainerFlow

