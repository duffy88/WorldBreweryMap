# Scrape Logo links for breweries from Google
library(RCurl)
library(XML)
library(rvest)

# Load cleaned brewery infos
beerGraphs <- readRDS("data/BeerGraphsBreweries(wStreets).rds")

# Progress bar
pb <- txtProgressBar(min = 1, max = nrow(beerGraphs), style = 3)


for(i in 1:nrow(beerGraphs)){ #
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  
  
  if(beerGraphs$Brewery[i] %in% c("Great Lakes Brewery")){
    url <- paste("https://www.google.ca/search?site=imghp&tbm=isch&source=hp&biw=1280&bih=611&q=",
                 gsub(" ","+",beerGraphs$Brewery[i]),"+toronto+logo", sep="")
  }else if(beerGraphs$Brewery[i] %in% c("Steamworks Brewing Co.")){
    url <- paste("https://www.google.ca/search?site=imghp&tbm=isch&source=hp&biw=1280&bih=611&q=",
                 gsub(" ","+",beerGraphs$Brewery[i]),"+colorado+logo", sep="")
  }else {
    url <- paste("https://www.google.ca/search?site=imghp&tbm=isch&source=hp&biw=1280&bih=611&q=",
                 gsub(" ","+",beerGraphs$Brewery[i]),"+logo", sep="")
  }
  
  ht <- read_html(url)
  img <- ht %>% html_node(xpath='//a/img')
  link <- html_attr(img, "src")
  
  if(i ==1){
    logos <- data.frame(Brewery = beerGraphs$Brewery[i],
                        BrewID = beerGraphs$BrewID[i],
                        logoLink = link)
  }else {
    temp <- data.frame(Brewery = beerGraphs$Brewery[i],
                       BrewID = beerGraphs$BrewID[i],
                       logoLink = link)
    logos <- rbind(logos, temp)
  }
  
  if(i == nrow(beerGraphs)){
    
    saveRDS(logos, "data/LogoLinks/BreweryLogoLinks.rds")
    
  }
  
}
close(pb)