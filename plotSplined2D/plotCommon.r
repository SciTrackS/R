

# select a file
Filters <- matrix(c("Splined 3D data", "*Splined.csv"), 1, 2, byrow = TRUE)
data.3d.path <- choose.files(caption="Select data file", filters = Filters, index = 1)

if(!file.exists(data.3d.path)) {
  stop("3D File does not exist")
}
data.3d <- read.table(data.3d.path, header=TRUE, sep=";")
if (!'XSplined' %in% colnames(data.3d) || !'YSplined' %in% colnames(data.3d) || !'ZSplined' %in% colnames(data.3d) || !"object" %in% colnames(data.3d) || !"time" %in% colnames(data.3d) ) {
  stop(paste("3D File did not contain expected column names XSplined,YSplined,ZSplined,object,time",","))
}

#filter out data according to preferences and skip any invalid data
data.3d <- data.3d[
  !is.nan(data.3d$time)
  &!is.nan(data.3d$object)
  &!is.nan(data.3d$XSplined)
  &!is.nan(data.3d$YSplined)
  &!is.nan(data.3d$ZSplined)
  &data.3d$time>startTime
  &data.3d$time<endTime
  &data.3d$object>=minObject3D
  &data.3d$object<=maxObject3D,]

if (nrow(data.3d)==0) {
  stop("No data left after filtering")
}


#collect unique object indices
objectIndices <-  unique(data.3d$object)

#
#
titleFont     <- list(size = 14)
legendFont    <- list(size = 12)
axisLabelFont <- list(size = 12)
axisTickFont  <- list(size = 10)

legendtitle <- list(yref='paper',xref="paper",y=1.05,x=1.13, text="IDs",showarrow=F,font=legendFont)

x <- data.3d[,firstVariableName]
y <- data.3d[,secondVariableName]
minX<-min(x)
maxX<-max(x)
minY<-min(y)
maxY<-max(y)


#see https://www.rdocumentation.org/packages/plotly/versions/4.9.0

plt <- plot_ly(mode = 'lines', color=1) %>%
  layout(title = '',font=titleFont,plot_bgcolor='rgb(234, 234, 234)', annotations=legendtitle,legend=list(font=legendFont),
         xaxis = list(zeroline = FALSE, title = firstAxisName, titlefont = axisLabelFont, tickfont = axisTickFont,gridcolor='#FFFFFF',ticks='outside'),
         yaxis = list(zeroline = FALSE, title = secondAxisName, titlefont = axisLabelFont, tickfont = axisTickFont,gridcolor='#FFFFFF',ticks='outside'))

# loop over object indices and draw the data with an individual color for each index
iColor <- 0
for(i in objectIndices) {
  object.rows <- data.3d[data.3d$object==i,]
  
  #skip very short tracks
  if (length(object.rows$time)<2) {
    next;
  }
  x <- object.rows[,firstVariableName]
  y <- object.rows[,secondVariableName]
  iColor <- iColor+1
  
  #uncomment/comment the following lines if you want to draw lines and/or markers
  #plt <- add_trace(plt, y = y, x = x, name = iColor, type = 'scatter', mode = 'markers', color=iColor, symbol=(iColor%%6))
  plt <- add_trace(plt, y = y, x = x, name = iColor, type = 'scatter', mode = 'lines', color=iColor, colors = "Set1", line=list(width=1.0))
}

plt <- hide_colorbar(plt)

  
