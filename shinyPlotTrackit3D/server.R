library(shiny)
library(rgl)

if (interactive()) {
  options(shiny.maxRequestSize = 500*1024^2, rgl.useNULL = FALSE)
} else {
  options(shiny.maxRequestSize = 500*1024^2, rgl.useNULL = TRUE)
}
shinyServer(function(input, output, session) {
  
  # initial values to detect change
  datapath <- ""
  startTime <- 0
  endTime <- 0
  minObjectNumber <- 0
  maxObjectNumber <- 0
  
  # start of warnings section
  addWarning <- function(warningText) {
    warnings( paste(warningText,"<br>", warnings()))
  }
  
  # start of warnings section
  warnings <- reactiveVal("")
  
  output$warnings <- renderUI({
    HTML(warnings())
  })
  
  observeEvent(input$clearMessageButton, {
    warnings ("")
  })
  
  output$hasWarnings <-reactive({
    
    hasWarning <- nchar(warnings())==0
  })
  
  
  # start of output section
  output$rglWindow <- renderRglwidget({
    # explicitly detect change (observeEvent did not prevent update)
    if (is.null(input$trackitFile$datapath)||input$trackitFile$datapath == "") {
      if (length(rgl.dev.list())==0){
        try(rgl.close())
        open3d()
      }
      return(rglwidget(scene3d()))
    }
    if (datapath==input$trackitFile$datapath && minObjectNumber==input$minObjectNumber && maxObjectNumber==input$maxObjectNumber && startTime==input$startTime && endTime==input$endTime ) {
      return(rglwidget(scene3d()))   
    }
    
    updateMinMax <- FALSE
    if ( datapath != input$trackitFile$datapath) {
      updateMinMax <- TRUE
    }
    datapath <<- input$trackitFile$datapath
    minObjectNumber <<- input$minObjectNumber
    maxObjectNumber <<- input$maxObjectNumber
    startTime <<- input$startTime
    endTime <<- input$endTime
    
    #start of rendering logic
    if(length(rgl.dev.list())>0){
      rgl.clear()
    } else {
      try(rgl.close())
      open3d()
    }
 
    inFile <- input$trackitFile
    if (is.null(inFile)) { 
      trackit.plot.box(minX=0.0, maxX=1.0, minY=0.00, maxY=1.0, minZ=0, maxZ=1.0)
      scene1 <- scene3d()
      
      return(rglwidget(scene1)) 
    }
    print(paste("renderRglwidget", inFile$datapath))

    isOK <- trackit.plot.data.3d(file.name=inFile$datapath, startTime=input$startTime, endTime=input$endTime, minObjectNumber=input$minObjectNumber, maxObjectNumber=input$maxObjectNumber, addWarning=addWarning, updateMinMax=updateMinMax, session=session)
    if (!isOK) { return(NULL) }
    scene1 <- scene3d()
    rglwidget(scene1)
  })
  
})


trackit.plot.data.3d <- function(file.name, startTime, endTime, minObjectNumber, maxObjectNumber, addWarning, updateMinMax, session){
  print(paste("trackit.plot.data.3d", file.name))
  if(!file.exists(file.name)) {
    addWarning("File does not exist")
    return(FALSE)
  }
  data.3d <- read.table(file.name, header=TRUE, sep=";")
  if ('XSplined' %in% colnames(data.3d) && "YSplined" %in% colnames(data.3d) && "ZSplined" %in% colnames(data.3d)&& "object" %in% colnames(data.3d)&& "time" %in% colnames(data.3d)) {
  } else {
    addWarning("3D File did not contain expected column names object,XSplined,YSplined,ZSplined,time")
    return(FALSE)
  } 
  
  if (updateMinMax) {
    data.3d <- data.3d[
      !is.nan(data.3d$time)
      &!is.nan(data.3d$object)
      &!is.nan(data.3d$XSplined)
      &!is.nan(data.3d$YSplined)
      &!is.nan(data.3d$ZSplined),]
    startTime <- min(data.3d$time )
    endTime   <- max(data.3d$time )
    minObjectNumber <- min(data.3d$object )
    maxObjectNumber <- max(data.3d$object )
    updateNumericInput(session, "minObjectNumber", value = minObjectNumber)
    updateNumericInput(session, "maxObjectNumber", value = maxObjectNumber)
    updateNumericInput(session, "startTime", value = startTime)
    updateNumericInput(session, "endTime"  , value = endTime)
  }     
  
  
  if(length(rgl.dev.list())>0){
    rgl.clear()
  }
  
  data.3d <- data.3d[
    data.3d$time    >= startTime
    &data.3d$time   <= endTime
    &data.3d$object >= minObjectNumber
    &data.3d$object <= maxObjectNumber
    &!is.nan(data.3d$XSplined)
    &!is.nan(data.3d$YSplined)
    &!is.nan(data.3d$ZSplined),]
  
  if (nrow(data.3d)==0) {
    addWarning("No data points found under given constraints")
    return(FALSE)
  }
  else {
  
    min.3d.index<-min(data.3d$object)
    max.3d.index<-max(data.3d$object)
    minX <- min(data.3d$XSplined)
    maxX <- max(data.3d$XSplined)
    minY <- min(data.3d$YSplined)
    maxY <- max(data.3d$YSplined)
    minZ <- min(data.3d$ZSplined)
    maxZ <- max(data.3d$ZSplined)
    
    myColours <- c("red","green","blue","brown", "darkcyan", "darkgray", "violet", "pink","spring green","medium sea green","dodger blue","dark magenta","chartreuse","cyan","burlywood", "gold1", "gray", "plum2","seagreen1","slateblue1","yellowgreen")
   
    #scale the time axis so that it is 3 times x or y dimension, whatever is longer
    rgl_add_axes(x=data.3d$XSplined, y=data.3d$YSplined, z=data.3d$ZSplined,xlab = 'X',ylab='Y', zlab='Z',show.plane=FALSE,show.bbox=TRUE)
    trackit.plot.box(minX=minX, maxX=maxX, minY=minY, maxY=maxY, minZ=minZ, maxZ=maxZ)
    
    min.object.index<-min(data.3d$object)
    max.object.index<-max(data.3d$object)
    for(i in min.object.index:max.object.index) {
      
      #assign a different color for each object by looping through the colours in myColours
      colour <- myColours[ 1 + i %% length(myColours) ]
      object.rows <- data.3d[data.3d$object==as.character(i),]
      plot3d(x=object.rows$XSplined, y=object.rows$YSplined, z=object.rows$ZSplined, xlab="X", ylab="Y", zlab="Z", type = "p",size=3, col=colour, add=TRUE)
      plot3d(x=object.rows$XSplined, y=object.rows$YSplined, z=object.rows$ZSplined, xlab="X", ylab="Y", zlab="Z", type = "l",lwd=2, col=colour, add=TRUE)
      
    }
    
    #plot3d(x=d$XSplinedx, y=d$YSplinedy, z=d$ZSplinedz, xlab="X", ylab="Y", zlab="Z", type = "p", col=d$cc, add=TRUE)
  }
  
  TRUE
}


rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#555599","black"))
{ 
  getDigits <- function(x) {
    if      (x>500) {c(-2, 50) } 
    else if (x>100) {c(-1, 10) } 
    else if (x> 50) {c(-1,  5) } 
    else if (x> 10) {c( 0,  1) } 
    else if (x>  5) {c( 0,0.5) } 
    else if (x>  1) {c( 0,0.1) } 
    else if (x>0.5) {c( 1,0.05) } 
    else            {c( 2,0.01) } 
  }
  
  lim <- function(x){
    minVal <- min(x)
    maxVal <- max(x)
    digits <- getDigits(maxVal-minVal)
    minVal2 <- round(minVal-0.01*(maxVal-minVal),digits[1])
    maxVal2 <- round(maxVal+0.01*(maxVal-minVal),digits[1])
    c(minVal2, maxVal2, digits[2])
  }
  # Add axes
  xlim3 <- lim(x); ylim3 <- lim(y); zlim3 <- lim(z)
  xlim <- c(xlim3[1],xlim3[2]); ylim <- c(ylim3[1],ylim3[2]); zlim <- c(zlim3[1],zlim3[2]); 
  xMin <- xlim3[1]; yMin <- ylim3[1]; zMin <- zlim3[1]; 
  rgl.lines(xlim, c(yMin, yMin), c(zMin, zMin), color = axis.col)
  rgl.lines(c(xMin, xMin), ylim, c(zMin, zMin), color = axis.col)
  rgl.lines(c(xMin, xMin), c(yMin, yMin), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], yMin, zMin), c(xMin, ylim[2], zMin), 
                c(xMin, yMin, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) {
    xlim <- xlim/1.1; zlim <- zlim /1.1
    rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
               z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  }
  
  # Add bounding box decoration
  if(show.bbox){
    xTicks <- seq(from=xMin, to=xlim3[2], by=xlim3[3])
    yTicks <- seq(from=yMin, to=ylim3[2], by=ylim3[3])
    zTicks <- seq(from=zMin, to=zlim3[2], by=zlim3[3])
    
    #NOTE: make marklen bigger to make leght of mark lines shorter
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             marklen=30.0, xat=xTicks, yat=yTicks, zat=zTicks, xlab=xTicks, ylab=yTicks, zlab=zTicks) 
  }
}

trackit.plot.box <- function(minX,maxX,minY,maxY,minZ,maxZ){
  
  # Floor
  segments3d(c(minX,maxX), c(minY,minY), c(minZ,minZ), add=FALSE, col="blue")
  segments3d(c(maxX,maxX), c(minY,maxY), c(minZ,minZ), add=FALSE, col="blue")
  segments3d(c(minX,maxX), c(maxY,maxY), c(minZ,minZ), add=FALSE, col="blue")
  segments3d(c(minX,minX), c(minY,maxY), c(minZ,minZ), add=FALSE, col="blue")
  
  # Roof
  segments3d(c(minX,maxX), c(minY,minY), c(maxZ,maxZ), add=FALSE, col="blue")
  segments3d(c(maxX,maxX), c(minY,maxY), c(maxZ,maxZ), add=FALSE, col="blue")
  segments3d(c(minX,maxX), c(maxY,maxY), c(maxZ,maxZ), add=FALSE, col="blue")
  segments3d(c(minX,minX), c(minY,maxY), c(maxZ,maxZ), add=FALSE, col="blue")
  
  # vertical edges
  segments3d(c(minX,minX), c(minY,minY), c(minZ,maxZ), add=FALSE, col="blue")
  segments3d(c(maxX,maxX), c(minY,minY), c(minZ,maxZ), add=FALSE, col="blue")
  segments3d(c(minX,minX), c(maxY,maxY), c(minZ,maxZ), add=FALSE, col="blue")
  segments3d(c(maxX,maxX), c(maxY,maxY), c(minZ,maxZ), add=FALSE, col="blue")
}

