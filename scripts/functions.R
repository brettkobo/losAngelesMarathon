library("RJSONIO")
library("RCurl")
library("httr")
library("emil")

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

blankToNA <- function(x) {
  x[sapply(x, is_blank)] <- NA
  return(x)
}  

cityToGPS <- function(city) {
  get.addy <- rawToChar(GET(paste("http://localhost:8080/maps/api/geocode/json?sensor=false&address=", URLencode(city), sep = ""))$content)
  
  if(get.addy == "<h1>Internal Server Error</h1>") {
    latLon <- data.frame("NULL", "NULL")
    colnames(latLon) <- c("lat", "lon")
    return(latLon)
  }
  
  result <- RJSONIO::fromJSON(get.addy)
  
  if(result$status == "ZERO_RESULTS") {
    latLon <- data.frame("NULL", "NULL")
    colnames(latLon) <- c("lat", "lon")
  }
  
  if(result$status == "OK") {
    lat <- result$results[[1]]$geometry$location[[1]]
    lon <- result$results[[1]]$geometry$location[[2]]
    latLon <- data.frame(lat, lon)
    colnames(latLon) <- c("lat", "lon")
  }
  
  closeAllConnections()
  return(latLon)
}

# get.cZone <- rawToChar(GET(paste("http://localhost:8080/coordinates2politics/", lat, "%2c", lon, sep = ""))$content)  
# result <- RJSONIO::fromJSON(get.cZone)
# 
# get.addy <- rawToChar(GET(paste("http://localhost:8080/maps/api/geocode/json?sensor=false&address=", URLencode("mexico city, nm"), sep = ""))$content)
# result <- RJSONIO::fromJSON(get.addy)

gpsToCenusBlock <- function(lat, lon) {
  get.fpis <- rawToChar(GET(paste("http://data.fcc.gov/api/block/find?latitude=", lat, "&longitude=", lon, "&showall=true&format=json", sep = ""))$content)
    
    if(grepl("Error report", get.fpis)) {
      cenusData <- data.frame("NULL","NULL", "NULL")
      colnames(cenusData) <- c("county","tract", "block")
      return(cenusData)
      }
  
  resultFPIS <- RJSONIO::fromJSON(get.fpis)
  fips <- resultFPIS$Block[1]
  
  county <- substring(fips, 3, 5)
  tract <- substring(fips, 6, 11)
  block <- substring(fips, 12, 15)
  
  cenusData <- data.frame(county, tract, block)
  colnames(cenusData) <- c("county","tract", "block")
  return(cenusData)
}  

#get.fpis <- rawToChar(GET(paste("http://data.fcc.gov/api/block/find?latitude=", modRaceData$lat[p], "&longitude=", modRaceData$lon[p], "&showall=true&format=json", sep = ""))$content)

#http://data.fcc.gov/api/block/find?latitude=33.97395&longitude=-118.24846&showall=true&format=json

#View(table(paste(latLon$lat, ",", latLon$lon, sep = "")))

