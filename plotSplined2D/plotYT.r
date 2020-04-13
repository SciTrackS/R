#install required packages if not yet installed and require them
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if (isRStudio) {
  requiredPackages <- c('plotly', 'rstudioapi')
} else {
  requiredPackages <-  c('plotly')
}
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#find out the directory of this script
if (isRStudio) {
  source_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  source_directory <- getwd()
}


#evaluate preferences
source(paste(source_directory,"preferences.r", sep="/"))

#prepare configuration variables for plotting
firstVariableName<- 'time'
secondVariableName <- 'YSplined'
firstAxisName <- 'time'
secondAxisName <- 'Y'

#read data and plot
source(paste(source_directory,"plotCommon.r", sep="/"))

#show the plot (this works only if sourced with Echo)
plt