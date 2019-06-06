#' @title Function to obtain the Configuration File for CASPIAN.
#' @description This function allows the user to obtain the Configuration File needed to run CASPIAN. An existing file with the same name will be overwritten.
#' It is STRONGLY RECOMMENDED to change only the values of parameters and variables, not their names, or the model will not recognize them.
#' @param outputDir character, the path where the Configuration File will be saved
#'
#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' #saving the configuration file in "C:"
#' getConfigFile("C:/")
#'
#' #saving the configuration file in the current working directory:
#' getConfigFile(getwd())
#'

getConfigFile <- function(outputDir){
  if (file.exists(file.path(outputDir,"ConfigFile.R"))) warning("Existing configuration file has been overwritten")
  file.copy(from=file.path(system.file("extdata", package="CASPIAN"),"ConfigFile.R"),to=file.path(outputDir,"ConfigFile.R"),overwrite=T)
  cat("\n Configuration file saved in ",outputDir, " \n")
  }
