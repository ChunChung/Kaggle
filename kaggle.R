library(prospectr)
dir="/Users/chunchung/Documents/Pitt/2015 spring/Data Mining/Project/drivers"
submitPath="/Users/chunchung/Documents/Pitt/2015 spring/Data Mining/Project/data2"


driver <- function(dir,submitPath,driverID,pathID){
  driverID=toString(driverID)
  pathID=toString(pathID)
  pathID=paste(pathID,"csv",sep=".")
  mainDir=paste(dir,driverID,sep='/')
  #setwd(mainDir)
  path(mainDir, driverID,submitPath)  
}

path <- function(mainDir,driverID,submitPath){
  output=NULL
  for(pathID in 1:200){
    pathID=paste(pathID,"csv",sep=".")
    
  readPath=paste(mainDir,pathID,sep='/')
  data=read.csv(readPath)
  
  sg <- savitzkyGolay(data, p = 0, w = 1, m = 0)
  
  #Calculate the parameters
  distances=sqrt(diff(data$x)^2 + diff(data$y)^2)
  speed =sqrt(diff(data$x)^2 + diff(data$y)^2)*3.6 
  acceleration =diff(speed)
  acceleration=append(0,acceleration)
  radius=atan2(data[-1,]$y,data[-1,]$x)  
  distance=sum(distances)
  speed=distance/nrow(data)
  distances_no_stops = distances[distances > 1.5]
  stops_ratio=length(distances[distances < 1.5]) / (length(distances) + 1.0)
  distance_no_stops = sum(distances_no_stops)
  speed_no_stops = distance_no_stops/(length(distance_no_stops) + 1)
  output = rbind(output,data.frame(distance=distance,speed=speed,stops_ratio=stops_ratio,distance_no_stops=distance_no_stops,speed_no_stops=speed_no_stops))

  }
  #Output
  dirr=paste(submitPath,driverID,sep="/")
  dirr=paste(dirr,"",sep=".csv")
  write.csv(output, dirr, row.names=FALSE)
}


## main function
for(driverID in 1){
    driver(dir,submitPath,driverID,pathID)
  
}
