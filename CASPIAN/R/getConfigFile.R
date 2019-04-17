getConfigFile <- function(outputDir){
  if (file.exists(file.path(outputDir,"ConfigFile.R"))) warning("Existing configuration file has been overwritten")
  file.copy(from=file.path(system.file("extdata", package="CASPIAN"),"ConfigFile.R"),to=file.path(outputDir,"ConfigFile.R"),overwrite=T)
  cat("\n Configuration file saved in ",outputDir, " \n")
  }
