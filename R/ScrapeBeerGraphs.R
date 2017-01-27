library(XML)

# Max page number of beers
#  http://beergraphs.com/leaderboards/?page=1
pages <- 2394

# Progress bar
pb <- txtProgressBar(min = 1, max = pages, style = 3)

for(i in 2394:pages){ # 2388 missing i == 110,  173,187, 195, 266, 350,637, 657,
                      #         767, 777,799, 872, 1478, 1774, 2045,2048, 2056, 2147, 
                      #         2241, 2393
  
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  url <- paste("http://beergraphs.com/leaderboards/?page=",i, sep="")
  
  table <- readHTMLTable(url, stringsAsFactors=FALSE)
  table <- table[[1]]
  
  table <- table[ ,-1]
  names(table) <- c("Beer","Style","Brewery","Origin","ABV","StylePl","BAR")
  
  if(i==1){
    beerGraphs <- table
  }else {
    beerGraphs <- rbind(beerGraphs,table)
  }
    
  if(i ==pages){
    for(j in c("ABV","StylePl","BAR")){
      beerGraphs[,j] <- as.numeric(beerGraphs[,j])
    }
    
    saveRDS(beerGraphs, "data/BeerGraphsBeers.rds" )
    
  }
}

# Close progress bar
close(pb)


