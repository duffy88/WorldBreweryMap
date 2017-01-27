
# Load Packages
library(ggmap)

# Load cleaned player info
beerGraphs <- readRDS("data/BeerGraphsBreweries(wStreets).rds")

# 3734 to 5000,5001 to 7158
for(i in 6234:7158){ #nrow(beerGraphs)
  
  if(!is.na(beerGraphs$FinalLocationFull[i])){
    
    
    lonlat <- geocode(beerGraphs$FinalLocationFull[i]) 
  }
  
  # Compile output of locations
  if(i ==6234){
    temp2 <- cbind(beerGraphs$Brewery[i],beerGraphs$FinalLocationFull[i], lonlat)
    print(i)
  }else {
    temp2 <- rbind(temp2, cbind(beerGraphs$Brewery[i],beerGraphs$FinalLocationFull[i], lonlat))
    print(i)
  }
  
  if(i ==7158){
    temp2[ is.na(temp2[,2]),"lon"] <- NA
    temp2[ is.na(temp2[,2]),"lat"] <- NA
    saveRDS(temp2, "data/Locations2017Jan/BreweryLocations(Batch-4)FullLoc.rds")
    
  }
}



