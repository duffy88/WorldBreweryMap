# Look up street address and main webpage for each brewery
# load packages
library(RCurl)
library(XML)
library(rvest)


# Load cleaned brewery infos
beerGraphs <- readRDS("data/BeerGraphsBreweries.rds")

# Remove Duplicated brewery names
# Sorted in desc TotalBAR, Always removes lower ranked duplicate breweries
beerGraphs <- beerGraphs[ !duplicated(beerGraphs$Brewery), ]

# Progress bar
pb <- txtProgressBar(min = 1, max = nrow(beerGraphs), style = 3)

for(i in 1:nrow(beerGraphs)){ # nrow(beerGraphs)
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  if(beerGraphs$Brewery[i]  %in% c("Epic Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+NZ",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("WEST Brewery")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+glasgow",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("San Gabriel")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+brewery+italy",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Malt Shovel Brewery (Lion Nathan)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","malt shovel brewery"),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Hogs Back Brewery","Phoenix Brewery","Milton Brewery","Abbey Ales")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+great+britain",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Wood's")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+brewing+united+kingdom",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Rooster's")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+North+Yorkshire",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Brasserie Dupont")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+belgium",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Sankt Gallen Brewery","K's Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+japan",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Nikenjayamochi Kadoya Honten Co.")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+japan+brewing",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Black Dog Brewery","Batch Brewing Co","Kooinda Boutique Brewery")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+Australia",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Steamworks Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+vancouver",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Steamworks Brewing Co.")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+colorado",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Beer By Cartel")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+arizona",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Tin Mill Brewery")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+missouri",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Haven Brewing","Island Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+california",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("The Brew Kettle")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+ohio",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Local Option Bierwerker")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+illinois",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Broadway Pub")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+quebec",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Notch Brewing")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+massachusetts",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Smirnoff")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Paradox Brewing")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+colorado",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Jupiter")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+california+brewing",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Bamberg Bier","Cervejaria Sudbrack Ltda.","Baden Baden")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+brazil",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("East End Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+pittsburgh",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Port Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+San+Marcos",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Grassroots Brewing","Collaboration: Hill Farmstead/Grassroots")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Grassroots Brewing"),"+vermont",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Knightberg")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+russia",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Cannery Brewing Company")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+canada",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Kirkstall Brewery")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),"+company",sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("SOC Brewing Inc.")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","north island beer japan"),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Central City Brewing Co. (Red Racer, Red Betty)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Central City Brewing Co."),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Orkney Brewery (Sinclair Breweries)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Orkney Brewery"),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Little Creatures Brewing (Lion Nathan)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Little Creatures Brewing"),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Moscow Brewing Company (Московская Пивоваренная Компания)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Moscow Brewing Company Russia"),sep="")
    
  }else if(grepl("&", beerGraphs$Brewery[i])  ){
    temp <- gsub("&","and", beerGraphs$Brewery[i])
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",temp),sep="")
    
  }else if(!grepl("Aspall", beerGraphs$Brewery[i]) &
           !grepl("Bier", beerGraphs$Brewery[i]) &
           !grepl("bier", beerGraphs$Brewery[i]) &
           !grepl("Brew", beerGraphs$Brewery[i]) &
           !grepl("brew", beerGraphs$Brewery[i]) &
           !grepl("Birra", beerGraphs$Brewery[i]) &
           !grepl("beer", beerGraphs$Brewery[i]) &
           !grepl("Beverage", beerGraphs$Brewery[i])&
           !grepl("Bryggeri", beerGraphs$Brewery[i])&
           !grepl("Brygghus", beerGraphs$Brewery[i])&
           !grepl("Brauerei", beerGraphs$Brewery[i])&
           !grepl("Brouwerij", beerGraphs$Brewery[i]) &
           !grepl("brouwerij", beerGraphs$Brewery[i]) &
           !grepl("Brasserie", beerGraphs$Brewery[i]) &
           !grepl("brasserie", beerGraphs$Brewery[i]) &
           !grepl("brouwerij", beerGraphs$Brewery[i]) &
           !grepl("Cervejas", beerGraphs$Brewery[i]) &
           !grepl("Cerveja", beerGraphs$Brewery[i]) &
           !grepl("Cervejaria", beerGraphs$Brewery[i]) &
           !grepl("Cervecera", beerGraphs$Brewery[i]) &
           !grepl("Cervecer", beerGraphs$Brewery[i]) &
           !grepl("Cerveceria", beerGraphs$Brewery[i]) &
           !grepl("Cider", beerGraphs$Brewery[i]) &
           !grepl("Orchards", beerGraphs$Brewery[i]) &
           !grepl("Bräu", beerGraphs$Brewery[i])&
           !grepl("Brugg", beerGraphs$Brewery[i])&
           !grepl("Brau", beerGraphs$Brewery[i])&
           !grepl("Browar", beerGraphs$Brewery[i])&
           !grepl("Birrone", beerGraphs$Brewery[i])&
           !grepl("Birrificio", beerGraphs$Brewery[i])&
           !grepl("Pivovar", beerGraphs$Brewery[i])&
           !grepl("Pivar", beerGraphs$Brewery[i])&
           !grepl("Sörfőzde", beerGraphs$Brewery[i])&
           !grepl("Pilsen Of Uruguay", beerGraphs$Brewery[i])&
           !grepl("Argentina", beerGraphs$Brewery[i])&
           !grepl("pivovar", beerGraphs$Brewery[i])){
    temp <- paste(beerGraphs$Brewery[i],"brewing", sep=" ")
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",temp),sep="")
    
  }else if(beerGraphs$Brewery[i]  %in% c("Bières de Chimay (Abbaye Notre Dame de Scourmont)")){
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+","Bières de Chimay"),sep="")
    
  }else  {
    url <- paste("https://www.google.ca/search?q=",gsub(" ","+",beerGraphs$Brewery[i]),sep="")
    
  }
  
  ht <- read_html(url)
  links <- ht %>% html_nodes(xpath='//h3/a') %>% html_attr('href')
  links <- gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))
  
  links <- links[links!="http://www.google.ca/aclk?sa=L"]
  links <- links[links!="http://www.google.ca/aclk?sa=l"]
  
  if(beerGraphs$Brewery[i]  %in% c("Malt Shovel Brewery (Lion Nathan)")){
    url2 <- paste(url,"+australia+address",sep="")
    
  }else  {
    url2 <- paste(url,"+address",sep="")
    
  }
  
  ht <- read_html(url2)
  
  # Get link URLs
  adds <- ht %>% # feed `main.page` to the next step
    html_nodes("span") %>% html_text()

  adds <- as.data.frame(adds)
  names(adds) <- "nodes"
  
  adds$nodes <- as.character(adds$nodes)
  
  
  # Find the span's that contain address-y things, then we'll take the top one
  adds <- adds[ substr(adds$nodes, 1,1) %in% 0:9 |
                  substr(adds$nodes, 1,3) =="Rue" | # French
                  substr(adds$nodes, 1,4) =="Arch" | # London, GB
                  grepl("straat", adds$nodes) | # Danish
                  grepl("gade", adds$nodes) | # Danish
                  grepl("gatan", adds$nodes) | # Swedish
                  grepl("^Lunde", adds$nodes) | # Norwegian?
                  grepl(", Netherlands", adds$nodes) |
                  grepl(", United States", adds$nodes) |
                  grepl(", USA", adds$nodes) |
                  grepl(", Belgium", adds$nodes) |
                  grepl(", Denmark", adds$nodes) |
                  grepl(", Iceland", adds$nodes) |
                  grepl(", Great Britain", adds$nodes) |
                  grepl(", United Kingdom", adds$nodes) |
                  grepl(", UK", adds$nodes) |
                  grepl(", New Zealand", adds$nodes) |
                  grepl(", Norway", adds$nodes) |
                  grepl(", Canada", adds$nodes) |
                  grepl(", Australia", adds$nodes) |
                  grepl(", Switzerland", adds$nodes) |
                  grepl(", Germany", adds$nodes) |
                  grepl(", Japan", adds$nodes) |
                  grepl("^Japan, ", adds$nodes) |
                  grepl(", Italy", adds$nodes) |
                  grepl(", Brazil", adds$nodes) |
                  grepl(", France", adds$nodes) |
                  grepl(", Austria", adds$nodes) |
                  grepl(", Hong Kong", adds$nodes) |
                  grepl(", Ireland", adds$nodes) |
                  grepl(", Sweden", adds$nodes) |
                  grepl(", Russia", adds$nodes) |
                  grepl(", China", adds$nodes) |
                  grepl("^China, ", adds$nodes) |
                  grepl(", Namibia", adds$nodes) |
                  grepl(", Poland", adds$nodes) |
                  grepl(", Czech Republic", adds$nodes) |
                  grepl(", Czechia", adds$nodes) |
                  grepl(", Luxembourg", adds$nodes) |
                  grepl(", Spain", adds$nodes) |
                  grepl(", South Africa", adds$nodes) |
                  grepl(", Puerto Rico", adds$nodes) |
                  grepl(", Mozambique", adds$nodes) |
                  grepl(", Mexico", adds$nodes) |
                  grepl(", Vatican City", adds$nodes) |
                  grepl(", Cayman Islands", adds$nodes) |
                  grepl(", Slovenia", adds$nodes) |
                  grepl(", Aruba", adds$nodes) |
                  grepl(", Greece", adds$nodes) |
                  grepl(", Venezuela", adds$nodes) |
                  grepl(", Costa Rica", adds$nodes) |
                  grepl(", Israel", adds$nodes) |
                  grepl(", Barbados", adds$nodes) |
                  grepl(", Portugal", adds$nodes) |
                  grepl(", Bosnia and Herzegovina", adds$nodes) |
                  grepl(", Armenia", adds$nodes) |
                  grepl(", Jamaica", adds$nodes) |
                  grepl(", Zimbabwe", adds$nodes) |
                  grepl(", Zambia", adds$nodes) |
                  grepl(", Guatemala", adds$nodes) |
                  grepl(", Belize", adds$nodes) |
                  grepl(", Egypt", adds$nodes) |
                  grepl(", Philippines", adds$nodes) |
                  grepl(", Mauritius", adds$nodes) |
                  grepl(", Nepal", adds$nodes) |
                  grepl(", Cyprus", adds$nodes) |
                  grepl(", Croatia", adds$nodes) |
                  grepl(", Estonia", adds$nodes) |
                  grepl(", Lithuania", adds$nodes) |
                  grepl(", Finland", adds$nodes) |
                  grepl(", Honduras", adds$nodes)|
                  grepl(", Nicaragua", adds$nodes) |
                  grepl(", Panama", adds$nodes) |
                  grepl(", Cuba", adds$nodes) |
                  grepl(", Turks and Caicos Islands", adds$nodes) |
                  grepl(", Bermuda", adds$nodes) |
                  grepl(", The Bahamas", adds$nodes) |
                  grepl(", Haiti", adds$nodes) |
                  grepl(", Barbados", adds$nodes) |
                  grepl(", Aruba", adds$nodes) |
                  grepl(", Colombia", adds$nodes) |
                  grepl(", Suriname", adds$nodes) |
                  grepl(", French Guiana", adds$nodes) |
                  grepl(", Peru", adds$nodes) |
                  grepl(", Bolivia", adds$nodes) |
                  grepl(", Chile", adds$nodes) |
                  grepl(", Uruguay", adds$nodes) |
                  grepl(", Argentina", adds$nodes) |
                  grepl(", Madagascar", adds$nodes) |
                  grepl(", Angola", adds$nodes) |
                  grepl(", Tanzania", adds$nodes) |
                  grepl(", Kenya", adds$nodes) |
                  grepl(", Ethiopia", adds$nodes) |
                  grepl(", Macedonia", adds$nodes) |
                  grepl(", Albania", adds$nodes) |
                  grepl(", Pristina", adds$nodes) |
                  grepl(", Romania", adds$nodes) |
                  grepl(", Hungary", adds$nodes) |
                  grepl(" Hungary", adds$nodes) |
                  grepl(", Slovakia", adds$nodes) |
                  grepl(", Latvia", adds$nodes) |
                  grepl(", India", adds$nodes) |
                  grepl(", Myanmar", adds$nodes) |
                  grepl(", Thailand", adds$nodes) |
                  grepl(", Vietnam", adds$nodes) |
                  grepl(", Cambodia", adds$nodes) |
                  grepl(", Singapore", adds$nodes) |
                  grepl(", Indonesia", adds$nodes) |
                  grepl(", Fiji", adds$nodes) |
                  grepl(", South Korea", adds$nodes) |
                  grepl(" South Korea", adds$nodes) |
                  grepl(", El Salvador", adds$nodes) ,] # 
  adds <- as.data.frame(adds)
  names(adds) <- "nodes"
  adds$nodes <- as.character(adds$nodes)
  
  adds$nodes <- gsub("[^[:alnum:],]"," ", adds$nodes)
  
  adds <- adds[ nchar(adds$nodes)>3,]
  
  
  adds <- as.data.frame(adds)
  names(adds) <- "nodes"
  
  # Manual corrections
  if(beerGraphs$Brewery[i]=="Mikkeller"){ # Take Mikeller address in Copenhagen
    links <- links[1]
    add <- "Stefansgade 35, 2200 København N, Denmark"
    
  }else if(beerGraphs$Brewery[i]=="Bell's Brewery, Inc."){ # Fix Bell's Beers address...gives bellwoods brewery?(Fuck your IP locating google)
    links <- links[1]
    add <- "355 E Kalamazoo Ave, Kalamazoo, MI 49007, United States"
    
  }else if(beerGraphs$Brewery[i]=="Great Lakes Brewing Company"){ # Fix Great Lakes Brewing (Cleveland,OH)...gives Great Lakes Toronto?(Fuck your IP locating google)
    links <- "https://www.greatlakesbrewing.com/"
    add <- "2516 Market Ave, Cleveland, OH 44113, USA"
    
  }else if(beerGraphs$Brewery[i]=="New Belgium Brewing Company"){ # ????
    links <- links[1]
    add <- "500 Linden St, Fort Collins, CO 80524, USA"
    
  }else if(beerGraphs$Brewery[i]=="Pizza Port"){ # ????
    links <- links[1]
    add <- "571 Carlsbad Village Dr, Carlsbad, CA 92008, United States"
    
  }else if(beerGraphs$Brewery[i]=="AF Brew"){ # ????
    links <- links[1]
    add <- "St. Petersburg, Russia"
    
  }else if(beerGraphs$Brewery[i]=="Magic Rock Brewing"){ # ????
    links <- links[1]
    add <- "Willow Park Business Centre, Willow Ln, Huddersfield HD1 5EB, United Kingdom"
    
  }else if(beerGraphs$Brewery[i]=="Lawson's Finest Liquids"){ # ????
    links <- links[1]
    add <- "Warren, VT, United States"
    
  }else if(beerGraphs$Brewery[i]=="Rogue Ales"){ # ????
    links <- links[1]
    add <- "2320 SE Marine Science Dr, Newport, OR 97365, USA"
    
  }else if(beerGraphs$Brewery[i]=="Grimm Artisanal Ales"){ # ????
    links <- links[1]
    add <- "Brooklyn, NY, United States"
    
  }else if(beerGraphs$Brewery[i]=="Yeastie Boys"){ # ????
    links <- links[1]
    add <- "Pipitea, Wellington, NZ"
    
  }else if(beerGraphs$Brewery[i]=="Liberty Brewing"){ # ????
    links <- links[1]
    add <- "69 Mill Rd, Helensville, Auckland, NZ"
    
  }else if(beerGraphs$Brewery[i]=="Panhead Brewery"){ # ????
    links <- links[1]
    add <- "Unit 21, South Pacific Tyres Industrial Park, Blenheim Street, Upper Hutt, New Zealand"
    
  }else if(beerGraphs$Brewery[i]=="Fuller, Smith & Turner"){ # ????
    links <- links[1]
    add <- "Shepherd's Bush, Greater London, GB"
    
  }else if(beerGraphs$Brewery[i]=="Stillwater Artisanal Ales"){ # ????
    links <- links[1]
    add <- "Baltimore, MD, US"
    
  }else if(beerGraphs$Brewery[i]=="Iron Hill Brewery & Restaurant"){ # ????
    links <- links[1]
    add <- "Wilmington, DE, US"
    
  }else if(beerGraphs$Brewery[i]=="Mckenzie's Hard Cider"){ # ????
    links <- links[1]
    add <- "West Seneca, NY, US"
    
  }else if(beerGraphs$Brewery[i]=="Creature Comforts Brewing Company"){ # ????
    links <- links[1]
    add <- "271 W Hancock Ave, Athens, GA 30601, United States"
    
  }else if(beerGraphs$Brewery[i]=="Hertog Jan (ABInBev)"){ # ????
    links <- "http://www.hertogjan.nl/"
    add <- "Kruisweg 44, 5944 EN Arcen, Netherlands"
    
  }else if(beerGraphs$Brewery[i]=="Mike's Hard Lemonade Company"){ # ????
    links <- "http://www.mikeshardlemonade.com/"
    add <- "159 S Jackson St # 400, Seattle, WA 98104, United States"
    
  }else if(beerGraphs$Brewery[i]=="Kyodo Shoji Koedo Brewery"){ # ????
    links <- "www.coedobrewery.com/en/"
    add <- "川越市, 埼玉県, JP"
    
  }else if(beerGraphs$Brewery[i]=="Les Trois Mousquetaires"){ # ????
    links <- links[1]
    add <- "3755 Boulevard Matte, Local C, Brossard, QC J4Y 2P4"
    
  }else if(beerGraphs$Brewery[i]=="Pretty Things Beer & Ale Project"){ # ????
    links <- links[1]
    add <- "Boston, MA, US"
    
  }else if(beerGraphs$Brewery[i]=="Bakunin"){ # ????
    links <- links[1]
    add <- "Saint Petersburg, Russia"
    
  }else if(beerGraphs$Brewery[i]=="Doctor's Orders Brewing"){ # ????
    links <- links[1]
    add <- "Sydney, NSW, AU"
    
  }else if(beerGraphs$Brewery[i]=="Birra Baladin"){ # ????
    links <- links[1]
    add <- "Piazza V Luglio 1944, 15, 12060 Piozzo CN, Italy"
    
  }else if(beerGraphs$Brewery[i]=="Kiuchi Brewery"){ # ????
    links <- links[1]
    add <- "Japan, 〒311-0133 Ibaraki Prefecture, Naka, 鴻巣1257"
    
  }else if(beerGraphs$Brewery[i]=="Shenanigans Brewing Co."){ # ????
    links <- "http://shenanigansbrewing.com/"
    add <- "Sydney, Australia"
    
  }else if(beerGraphs$Brewery[i]=="Behemoth Brewing Company"){ # ????
    links <- links[1]
    add <- "Auckland, New Zealand"
    
  }else if(beerGraphs$Brewery[i]=="Augustiner-Bräu Wagner"){ # ????
    links <- links[1]
    add <- "Landsberger Str. 35, 80339 München, Germany"
    
  }else if(beerGraphs$Brewery[i]=="1 tonna"){ # ????
    links <- links[1]
    add <- "Moscow, Russia"
    
  }else if(beerGraphs$Brewery[i]=="A.J.I. Beer Inc."){ # ????
    links <- links[1]
    add <- "豊中市, 大阪府, JP"
    
  }else if(beerGraphs$Brewery[i]=="Northern Brewer Homebrew Supply"){ # ????
    links <- links[1]
    add <- "6021 Lyndale Ave S, Minneapolis, Minnesota"
    
  }else if(beerGraphs$Brewery[i]=="Cervejaria Sudbrack Ltda."){ # ????
    links <- links[1]
    add <- "Blumenau, SC, BR"
    
  }else if(beerGraphs$Brewery[i]=="Brau Union Österreich"){ # ????
    links <- links[1]
    add <- "Linz, Oberösterreich, AT"
    
  }else if(beerGraphs$Brewery[i]=="Staromestny Pivovar"){ # ????
    links <- links[1]
    add <- "Minsk, Belarus"
    
  }else if(beerGraphs$Brewery[i]=="5 Stones Craft Brewing Company"){ # ????
    links <- links[1]
    add <- "850 Schneider, Cibolo, TX 78108, United States"
    
  }else if(beerGraphs$Brewery[i]=="Wakasaimo Honpo"){ # ????
    links <- links[1]
    add <- "登別市, 北海道, JP"
    
  }else if(beerGraphs$Brewery[i]=="Velka Morava"){ # ????
    links <- "www.moravabeer.ru/"
    add <- "Moscow, Russia"
    
  }else if(beerGraphs$Brewery[i]=="Pinta"){ # ????
    links <- "www.browarpinta.pl/"
    add <- "Wroclaw, Poland"
    
  }else if(beerGraphs$Brewery[i]=="Bach Brewing"){ # ????
    links <- links[1]
    add <- "Auckland, New Zealand"
    
  }else if(beerGraphs$Brewery[i]=="High Water Brewing"){ # ????
    links <- links[1]
    add <- "Chico, CA, US"
    
  }else if(beerGraphs$Brewery[i]=="Birrificio Artigianale Lariano"){ # ????
    links <- links[1]
    add <- "Dolzago, Lombardy , Italy"
    
  }else if(beerGraphs$Brewery[i]=="Tamashii Brewing Company"){ # ????
    links <- links[1]
    add <- "Kent Island, Maryland, United States"
    
  }else if(beerGraphs$Brewery[i]=="Black Isle"){ # ????
    links <- links[1]
    add <- "Old Allangrange, Munlochy IV8 8NZ, United Kingdom"
    
  }else if(beerGraphs$Brewery[i]=="LaBEERint Brewery"){ # ????
    links <- links[1]
    add <- "Kaluzhskaya oblast, Obninsk, Russia"
    
  }else if(beerGraphs$Brewery[i]=="Smirnoff"){ # ????
    links <- links[1]
    add <- "London, Greater London, GB"
    
  }else if(beerGraphs$Brewery[i]=="Dennis Beer Co."){ # ????
    links <- links[1]
    add <- "Sydney, New South Wales, Australia"
    
  }else if(beerGraphs$Brewery[i]=="Twisted Tea Brewing Co"){ # ????
    links <- links[1]
    add <- "Cincinnati, OH, US"
    
  }else if(beerGraphs$Brewery[i]=="Blue Lobster Brewing Company"){ # ????
    links <- links[1]
    add <- "845 Lafayette Rd, Hampton, NH 03842, United States"
    
  }else if(beerGraphs$Brewery[i]=="Private Landbrauerei Schönram"){ # ????
    links <- "http://www.brauerei-schoenram.de/"
    add <- adds$nodes[1]
    
  }else if(beerGraphs$Brewery[i]=="Dum"){ # ????
    links <- links[1]
    add <- "Curitiba, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="Hocus Pocus"){ # ????
    links <- links[1]
    add <- "Rio de Janeiro, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="Backlash Beer Company"){ # ????
    links <- links[1]
    add <- "Holyoke, MA, US"
    
  }else if(beerGraphs$Brewery[i]=="Guinness"){ # ????
    links <- "https://www.guinness.com/en-ca/"
    add <- "St James's Gate, Dublin 8, Ireland"
    
  }else if(beerGraphs$Brewery[i]=="Red Oak Brewery"){ # ????
    links <- links[1]
    add <- "6901 Konica Dr, Whitsett, NC 27377, USA"
    
  }else if(beerGraphs$Brewery[i]=="Hyouko Yashiki no Mori Brewery"){ # ????
    links <- links[1]
    add <- "Niigata-ken Akano-shi, Japan"
    
  }else if(beerGraphs$Brewery[i]=="Sapporo Breweries"){ # ????
    links <- "https://sapporobeer.com/"
    add <- "渋谷区, 東京都, JP"
    
  }else if(beerGraphs$Brewery[i]=="Triple Digit Brewery"){ # ????
    links <- "http://tripledigitbrewing.com/"
    add <- "1621 Dana Avenue, Cincinnati, OH"
    
  }else if(beerGraphs$Brewery[i]=="Boxing Cat Brewery"){ # ????
    links <- links[1]
    add <- "519 Fuxing Middle Rd, Shanghai, China"
    
  }else if(beerGraphs$Brewery[i]=="Moosehead Breweries Limited"){ # ????
    links <- links[1]
    add <- "89 Main St, Saint John, NB E2K 1H2, Canada"
    
  }else if(beerGraphs$Brewery[i]=="Lost Coast Brewery"){ # ????
    links <- links[1]
    add <- "617 Fourth St. Eureka, CA 95501, United States"
    
  }else if(beerGraphs$Brewery[i]=="Prancing Pony Brewery"){ # ????
    links <- "http://prancingponybrewery.com.au/"
    add <- adds$nodes[1]
    
  }else if(beerGraphs$Brewery[i]=="2Cabeças Cervejas"){ # ????
    links <- links[1]
    add <- "Rio de Janeiro, RJ, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="BJ's Restaurant and Brewhouse"){ # ????
    links <- links[1]
    add <- "200 Main Street, Huntington Beach, CA 92648, United States"
    
  }else if(beerGraphs$Brewery[i]=="Brewmaster Jack"){ # ????
    links <- links[1]
    add <- "Northampton, Massachusetts, United States"
    
  }else if(beerGraphs$Brewery[i]=="Rooster's"){ # ????
    links <- links[1]
    add <- "Unit 3-4 Grimbald Park, Wetherby Road, Knaresborough, North Yorkshire, GB"
    
  }else if(beerGraphs$Brewery[i]=="Nikenjayamochi Kadoya Honten Co."){ # ????
    links <- links[1]
    add <- "34 Ujiimazaike-cho, Ise City, Mie"
    
  }else if(beerGraphs$Brewery[i]=="Yo-Ho Brewing Company"){ # ????
    links <- links[1]
    add <- "1119-1 Otai, Saku city, Nagano, Japan"
    
  }else if(beerGraphs$Brewery[i]=="Bevog"){ # ????
    links <- links[1]
    add <- "Gewerbepark B 9, A-8490 Bad Radkersburg, Austria"
    
  }else if(beerGraphs$Brewery[i]=="Bath Ales"){ # ????
    links <- links[1]
    add <- "Bath Ales Ltd, Hare House, Southway Drive, Warmley, Bristol BS30 5LW"
    
  }else if(beerGraphs$Brewery[i]=="Phoenix Brewery"){ # ????
    links <- links[1]
    add <- "Green Lane, Heywood, Greater Manchester, OL10 2EP, Great Britain"
    
  }else if(beerGraphs$Brewery[i]=="Baden Baden"){ # ????
    links <- links[1]
    add <- "Avenida Matheus da Costa Pinto, 1653, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="Cervejaria Júpiter"){ # ????
    links <- links[1]
    add <- "Rua Augusta, 2381, Jardim Paulista, Sao Paulo-SP, 01413-000, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="American Vintage Beverage Co."){ # ????
    links <- links[1]
    add <- "Seattle, Washington, United States"
    
  }else if(beerGraphs$Brewery[i]=="Long Beach"){ # ????
    links <- links[1]
    add <- "Long Beach, New York, United States"
    
  }else if(beerGraphs$Brewery[i]=="Kent Brewery"){ # ????
    links <- links[1]
    add <- "Birling Place Farm, Stangate Road, Birling, Kent ME19 5JN, United Kingdom"
    
  }else if(beerGraphs$Brewery[i]=="Brauhaus Riegele"){ # ????
    links <- links[1]
    add <- "Inh. Riegele KG, Frölichstraße 26, 86150 Augsburg, Germany"
    
  }else if(beerGraphs$Brewery[i]=="Sebago Brewing Company"){ # ????
    links <- links[1]
    add <- "48 Sanford Dr, Gorham, Maine, 04038, United States"
    
  }else if(beerGraphs$Brewery[i]=="Arbor Brewing Company"){ # ????
    links <- links[1]
    add <- "114 E Washington St, Ann Arbor, Michigan, 48104, United States"
    
  }else if(beerGraphs$Brewery[i]=="Brewery Budweiser Budvar (BBNP)"){ # ????
    links <- "http://www.budejovickybudvar.cz/en/index.html"
    add <- "Karolíny Světlé 512/4, 370 21 České Budějovice, Czechia"
    
  }else if(beerGraphs$Brewery[i]=="Pracownia Piwa"){ # ????
    links <- links[1]
    add <- "Świętego Jana 30, 31-018 Kraków, Poland"
    
  }else if(beerGraphs$Brewery[i]=="Morada Cervejaria Artesanal"){ # ????
    links <- links[1]
    add <- "Curitiba, Brazil"
    
  }else if(beerGraphs$Brewery[i]=="De Proefbrouwerij"){ # ????
    links <- "http://www.proefbrouwerij.com/index.aspx?lng=en"
    add <- adds$nodes[1]
    
  }else if(beerGraphs$Brewery[i]=="Wahoo Brewing Company"){ # ????
    links <- "http://www.woohabrewing.com/"
    add <- adds$nodes[1]
    
  }else if(beerGraphs$Brewery[i]=="Island Brewing Co. (Fiji)"){ # ????
    links <- "http://www.vonubeer.com/"
    add <- "Lot 2, Kabani Rd, Nadi, Fiji"
    
  }else {
    links <- links[1]
    add <- adds$nodes[1]
    
  }
  
  
  if(i ==1){
    streets <- data.frame(Brewery = beerGraphs$Brewery[i],
                          BrewID = beerGraphs$BrewID[i],
                        link = links,
                        address = add)
  }else {
    temp <- data.frame(Brewery = beerGraphs$Brewery[i],
                       BrewID = beerGraphs$BrewID[i],
                       link = links,
                       address = add)
    streets <- rbind(streets, temp)
  }
  
  if(i == nrow(beerGraphs)){
    
    saveRDS(streets, "data/StreetWebLocations/BreweryStreetWebLocations.rds")
    
  }
}

close(pb)


# Remove Duplicated brewery names
# Sorted in desc TotalBAR, Always removes lower ranked duplicate breweries
streets <- streets[ !duplicated(streets$Brewery), ]

beerGraphs <- merge(beerGraphs, streets[,c("BrewID","link","address")], by = "BrewID", all.x=T)

beerGraphs <- beerGraphs[ order(beerGraphs$TotalBAR, decreasing = T), ]

#rem <- c()
#adds <- unique(beerGraphs$address)
#for(i in 1:length(adds)){ #length(adds)
 # temp <- subset(beerGraphs, address == adds[i])
  #
#  if(nrow(temp)>1){
 #   rem <- c(rem, row.names(temp)[ 2:length(row.names(temp))])
  #}
  
#}

#if(length(rem)>0){
 # beerGraphs <- beerGraphs[ !(row.names(beerGraphs) %in% rem), ]
#}

# ~600 (~500 non NAs) duplicated addresses even after removing duplicated brewery names
# Generally gets rid of breweries which are duplicates with slightly different names
# Code in some exceptions where duplicate address are OK (co-habitation, etc)
beerGraphs <- beerGraphs[ !duplicated(beerGraphs$address) &
                            beerGraphs$Brewery!= "La Quince Brewery"&
                            beerGraphs$Brewery!= "Far From The Tree"&
                            beerGraphs$Brewery!= "My Beer Company" | 
                            is.na(beerGraphs$address)|
                            beerGraphs$address== "Detroit, MI, USA"|
                            beerGraphs$address== "Auckland, New Zealand"|
                            beerGraphs$address== "Carmel, IN, USA"|
                            beerGraphs$address== "Wichita, KS, USA"|
                            beerGraphs$address== "Curitiba, Brazil"|
                            beerGraphs$address== "Moscow, Russia"|
                            (beerGraphs$address== "1901 Dewdney Ave, Regina, SK S4R 8R2" & beerGraphs$Brewery=="Rebellion Brewing")|
                            (beerGraphs$address== "Palm Desert, CA, USA" & beerGraphs$Brewery=="La Quinta Brewing Co.")|
                            (beerGraphs$address== "164 Evans Ave, Etobicoke, ON M8Z 1J4" & beerGraphs$Brewery=="Spearhead Brewing Company")|
                            (beerGraphs$address== "1083 Richter St, Kelowna, BC V1Y 2K6" & beerGraphs$Brewery=="Tree Brewing")|
                            beerGraphs$address== "4700 Ohio Ave S, Seattle, WA 98134, USA"|
                            grepl("San Marcos, CA 92069, USA",beerGraphs$address)|
                            grepl("San Diego, CA, USA",beerGraphs$address), ]


beerGraphs <- beerGraphs[ order(beerGraphs$TotalBAR, decreasing = T), ]


beerGraphs$stLoc <- strsplit(as.character(beerGraphs$address),", ")
beerGraphs$stPlace1 <- beerGraphs$stPlace2 <- beerGraphs$stPlace3 <- NA 

for(i in 1:nrow(beerGraphs)){
  if(length(beerGraphs$stLoc[[i]])>2){
    beerGraphs$stPlace3[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])]
    beerGraphs$stPlace2[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-1]
    beerGraphs$stPlace1[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-2]
    
  }else if(length(beerGraphs$stLoc[[i]])==2){
    beerGraphs$stPlace3[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])]
    beerGraphs$stPlace2[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-1]
    
  }
  
} 

# Canadian addresses don't have country info... need to correct them
beerGraphs$temp <- strsplit(beerGraphs$stPlace3," ")
canadaProv <- c("AB","BC","MB","NB","NL","NS","NT","NU","ON","PE","QC","SK","YT")
for(i in 1:nrow(beerGraphs)){
  if(beerGraphs$temp[[i]][1] %in% canadaProv ){
    beerGraphs$stPlace1[i] <-  beerGraphs$stPlace2[i] 
    beerGraphs$stPlace2[i] <-  beerGraphs$stPlace3[i]
    beerGraphs$stPlace3[i] <- "Canada"
    
  }
  
  
}
beerGraphs$temp <- NULL

beerGraphs$stPlace3[beerGraphs$stPlace3=="USA" ] <- "United States"
beerGraphs$stPlace3[beerGraphs$stPlace3=="US" ] <- "United States"
beerGraphs$stPlace3[beerGraphs$stPlace3=="UK" ] <- "United Kingdom"
beerGraphs$stPlace3[beerGraphs$stPlace3=="NZ" ] <- "New Zealand"
beerGraphs$stPlace3[beerGraphs$stPlace3=="GB" ] <- "Great Britain"
beerGraphs$stPlace3[beerGraphs$stPlace3=="AU" ] <- "Australia"
beerGraphs$stPlace3[beerGraphs$stPlace3=="JP" ] <- "Japan"
beerGraphs$stPlace3[beerGraphs$stPlace3=="BR" ] <- "Brazil"
beerGraphs$stPlace3[beerGraphs$stPlace3=="AT" ] <- "Austria"

# Cleaning address info by country
# Every country has kinda different formatting of address info
states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
            "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
            "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
            "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
            "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia",
            "Washington","West Virginia","Wisconsin","Wyoming","District of Columbia","Virgin Islands")
statesAbbv <- c("AL","AK","AZ","AR","CA","CO","CT",
            "DE","FL","GA","HI","ID","IL","IN","IA",
            "KS","KY","LA","ME","MD","MA","MI","MN",
            "MS","MO","MT","NE","NV","NH","NJ","NM",
            "NY","NC","ND","OH","OK","OR","PA",
            "RI","SC","SD","TN","TX","UT","VT","VA",
            "WA","WV","WI","WY","DC","VI")

canadaProvFull <- c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
                    "Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island","Quebec","Saskatchewan","Yukon")
canadaProv <- c("AB","BC","MB","NB","NL","NS","NT","NU","ON","PE","QC","SK","YT")

ausProv <- c("Victoria","New South Wales","Western Australia","Queensland","South Australia","Tasmania",
             "Australian Capital Territory")
ausProvAbbv <- c("VIC","NSW","WA","QLD","SA","TAS","ACT")

italyProv <- c("Agrigento","Alessandria","Ancona", "Aosta","Arezzo","Ascoli Piceno",
               "Asti","Avellino","Bari","Barletta-Andria-Trani","Belluno","Benevento",
               "Bergamo","Biella","Bologna","South Tyrol","Brescia","Brindisi",
               "Cagliari","Caltanissetta","Campobasso","Carbonia-Iglesias","Caserta",
               "Catania","Catanzaro","Chieti","Como","Cosenza","Cremona","Crotone","Cuneo",
               "Enna","Fermo","Ferrara","Florence","Foggia","Forli-Cesena","Frosinone",
               "Genoa","Gorizia","Grosseto","Imperia","Isernia","La Spezia","L'Aquila",
               "Latina","Lecce","Lecco","Livorno","Lodi","Lucca","Macerata","Mantua",
               "Massa and Carrara","Matera","Medio Campidano","Messina","Milan","Modena",
               "Monza and Brianza","Naples","Novara","Nuoro","Ogliastra","Olbia-Tempio",
               "Oristano","Padua","Palermo","Parma","Pavia","Perugia","Pesaro and Urbino",
               "Pescara","Piacenza","Pisa","Pistoia","Pordenone","Potenza","Prato",
               "Ragusa","Ravenna","Reggio Calabria","Reggio Emilia","Rieti","Rimini",
               "Rome","Rovigo","Salerno","Sassari","Savona","Siena","Sondrio","Syracuse",
               "Taranto","Teramo","Terni","Trapani","Trentino","Treviso","Trieste",
               "Turin","Udine","Varese","Venice","Verbano-Cusio-Ossoia","Vercelli",
               "Verona","Vibo Velntia","Vicenza","Viterbo")
italyProvAbbv <- c("AG","AL","AN", "AO","AR","AP",
               "AT","AV","BA","BT","BL","BN",
               "BG","BI","BO","BZ","BS","BR",
               "CA","CL","CB","CI","CE",
               "CT","CZ","CH","CO","CS","CR","KR","CN",
               "EN","FM","FE","FI","FG","FC","FR",
               "GE","GO","GR","IM","IS","SP","AQ",
               "LT","LE","LC","LI","LO","LU","MC","MN",
               "MS","MT","VS","ME","MI","MO",
               "MB","NA","NO","NU","OG","OT",
               "OR","PD","PA","PR","PV","PG","PU",
               "PE","PC","PI","PT","PN","PZ","PO",
               "RG","RA","RC","RE","RI","RN",
               "RM","RO","SA","SS","SV","SI","SO","SR",
               "TA","TE","TR","TP","TN","TV","TS",
               "TO","UD","VA","VE","VB","VC",
               "VR","VV","VI","VT")

brazilProv <- c("Acre","Alagoas","Amapá","Anazibas","Bahia","Ceará",
                "Distrito Federal","Espírito Santo","Goiás","Maranhão","Mato Grosso",
                "Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná",
                "Pernambuco","Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul",
                "Rondônia","Roraima","Santa Catarina","São Paulo","Sergipe","Tocantins")
brazilProvAbbv <- c("AC","AL","AP","AM","BA","CE",
                "DF","ES","GO","MA","MT",
                "MS","MG","PA","PB","PR",
                "PE","PI","RJ","RN","RS",
                "RO","RR","SC","SP","SE","TO")

mexicoProv <- c("Aguascalientes","Baja California","Baja California Sur","Campeche",
                "Chiapas","Chihuahua","Coahuila","Colima","Federal District","Durango",
                "Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
                "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
                "San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamauliaps","Tlaxcala",
                "Veracruz","Yucatán","Zacatecas")
mexicoProvAbbv <- c("Ags","BC","BCS","Camp",
                "Chis","Chih","Coah","Col","CDMX","Dgo",
                "Gto","Gro","Hgo","Jal","Méx","Mich","Mor",
                "Nay","NL","Oax","Pue","Qro","QR",
                "SLP","Sub","Son","Tab","TAMPS","Tlax",
                "Ver","Yuc","Zac")
beerGraphs$temp <- NA
for(i in 1:nrow(beerGraphs)){
  if(!is.na(beerGraphs$stPlace3[i])){
    if(beerGraphs$stPlace3[i]=="United States"){
      beerGraphs$stPlace2[i] <- gsub("[[:digit:]]+","",beerGraphs$stPlace2[i] )
      if(nchar(beerGraphs$stPlace2[i])<=3){
        beerGraphs$stPlace2[i] <- gsub(" ","",beerGraphs$stPlace2[i] )
      }
      
      for(j in 1:length(states)){
        if(beerGraphs$stPlace2[i]==statesAbbv[j]){
          beerGraphs$stPlace2[i] <- states[j]
        }
        
      }
    }
    if(beerGraphs$stPlace3[i]=="Denmark"){
      beerGraphs$stPlace2[i] <- gsub("[[:digit:]]+","",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("K benhavn","København",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("N rre Aaby","Nørre Aaby",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Br ndby","Brøndby",beerGraphs$stPlace2[i] )
      if(substr(beerGraphs$stPlace2[i],1,1)==" "){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i] , 2, nchar(beerGraphs$stPlace2[i] ))
      }
      beerGraphs$stPlace2[i] <- gsub(" V","",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub(" N","",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub(" K","",beerGraphs$stPlace2[i] )
      
        
      
    }
    if(beerGraphs$stPlace3[i]=="Belgium"){
      beerGraphs$stPlace2[i] <- gsub("[[:digit:]]+","",beerGraphs$stPlace2[i] )
      if(substr(beerGraphs$stPlace2[i],1,1)==" "){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i] , 2, nchar(beerGraphs$stPlace2[i] ))
      }
      beerGraphs$stPlace2[i] <- gsub("B ","",beerGraphs$stPlace2[i] )
    }
    if(beerGraphs$stPlace3[i]=="United Kingdom"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][1]
      if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
        beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
      }
    }
    if(beerGraphs$stPlace3[i]=="New Zealand"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][1]
      if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
        beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
      }
    }
    if(beerGraphs$stPlace3[i]=="Norway"){
      beerGraphs$stPlace2[i] <- gsub("B nes","Bønes",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Lillestr m","Lillestrøm",beerGraphs$stPlace2[i] )
      
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][2]
      if(length(beerGraphs$temp[[i]])>2){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
        }
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Netherlands"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
        beerGraphs$stPlace2[i] <- beerGraphs$temp[[i]][2]
      }
      if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2]) &grepl("[[:lower:]]",beerGraphs$temp[[i]][3]) ){
        beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3] , sep = " ")
      }
      if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
        beerGraphs$stPlace2[i] <- beerGraphs$temp[[i]][3]
      }
      
      if(length(beerGraphs$temp[[i]])==4){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][4])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4], sep=" ")
        }
      }
      if(length(beerGraphs$temp[[i]])==5){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][5])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4],beerGraphs$temp[[i]][5], sep=" ")
        }
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Canada"){
      beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i], 1, 2)
      
      
      for(j in 1:length(canadaProv)){
        if(beerGraphs$stPlace2[i]==canadaProv[j]){
          beerGraphs$stPlace2[i] <- canadaProvFull[j]
        }
        
      }
    }
    if(beerGraphs$stPlace3[i]=="Australia"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      
      beerGraphs$stPlace1[i] <-beerGraphs$temp[[i]][1]
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][2]
      if(length(beerGraphs$temp[[i]])>=2){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
          beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
          beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][3]
          
        }
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
          beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][4]
          
        }
      }
      
      for(j in 1:length(ausProv)){
        if(!is.na(beerGraphs$stPlace2[i])){
          if(beerGraphs$stPlace2[i]==ausProvAbbv[j]){
            beerGraphs$stPlace2[i] <- ausProv[j]
          }
        }
        
        
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Switzerland"){
      beerGraphs$stPlace2[i] <- gsub("L gier","Légier",beerGraphs$stPlace2[i] )
      
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      
      
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][2]
      if(length(beerGraphs$temp[[i]])>2){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
        }
      }
      if(length(beerGraphs$temp[[i]])>3){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][4])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4], sep=" ")
        }
      }
      if(length(beerGraphs$temp[[i]])>4){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][5])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3],
                                          beerGraphs$temp[[i]][4],beerGraphs$temp[[i]][5], sep=" ")
        }
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Germany"){
      beerGraphs$stPlace2[i] <- gsub("N rnberg","Nürnberg",beerGraphs$stPlace2[i] )
      
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      
      if(!grepl("[[:lower:]]",beerGraphs$temp[[i]][1])){
        beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][2]
      }else {
        beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][1]
      }
      
      if(length(beerGraphs$temp[[i]])>2){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
        }
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][1]) &
           grepl("[[:lower:]]",beerGraphs$temp[[i]][2])&
           grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
        }
      }
      if(length(beerGraphs$temp[[i]])>3){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][4])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4], sep=" ")
        }
      }
      if(length(beerGraphs$temp[[i]])>4){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][5])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4],beerGraphs$temp[[i]][5], sep=" ")
        }
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Italy"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      
      beerGraphs$stPlace1[i] <-beerGraphs$temp[[i]][2]
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][3]
      
      if(length(beerGraphs$temp[[i]])>2){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
          beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
          beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][4]
          
        }
      }
      if(length(beerGraphs$temp[[i]])>3){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][4])){
          beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3],beerGraphs$temp[[i]][4], sep=" ")
          beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][5]
          
        }
      }
      
      for(j in 1:length(italyProv)){
        if(!is.na(beerGraphs$stPlace2[i])){
          if(beerGraphs$stPlace2[i]==italyProvAbbv[j]){
            beerGraphs$stPlace2[i] <- italyProv[j]
          }
        }
        
        
      }
      
    }
    # Some Japanese addresses start with country info..wtf?
    if(grepl("^Japan",beerGraphs$address[i])){
      beerGraphs$stPlace3[i] <- beerGraphs$stLoc[[i]][1]
      beerGraphs$stPlace2[i] <- beerGraphs$stLoc[[i]][2]
      beerGraphs$stPlace1[i] <- beerGraphs$stLoc[[i]][3]
    }
    
    if(beerGraphs$stPlace3[i]=="Japan"){
      beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
      
      beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][1]
      if(length(beerGraphs$temp[[i]])>1){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
          beerGraphs$stPlace2[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
          
        }
      }
      if(length(beerGraphs$temp[[i]])>3){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][4])){
          beerGraphs$stPlace2[i] <-beerGraphs$temp[[i]][4]
          
        }
      }
      if(length(beerGraphs$temp[[i]])>4){
        if(grepl("[[:lower:]]",beerGraphs$temp[[i]][5])){
          beerGraphs$stPlace2[i] <-paste(beerGraphs$temp[[i]][4],beerGraphs$temp[[i]][5], sep=" ")
          
        }
      }
      
      
    }
    if(beerGraphs$stPlace3[i]=="Brazil"){
      beerGraphs$stPlace1[i] <- gsub("C ndido Mota","Cândido Mota",beerGraphs$stPlace1[i] )
      beerGraphs$stPlace1[i] <- gsub("Nova Petr polis","Nova Petrópolis",beerGraphs$stPlace1[i] )
      beerGraphs$stPlace1[i] <- gsub("State of Minas Gerais","MG",beerGraphs$stPlace1[i] )
      beerGraphs$stPlace1[i] <- gsub("Paulo-SP","Paulo SP",beerGraphs$stPlace1[i] )
      beerGraphs$stPlace2[i] <- gsub("Macei    State of Alagoas","AL",beerGraphs$stPlace2[i] )
      
      if(!grepl("[[:lower:]]", beerGraphs$stPlace2[i]) & 
         grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace1[i], " ")
        
        beerGraphs$stPlace2[i] <- beerGraphs$temp[[i]][length(beerGraphs$temp[[i]])]
        beerGraphs$stPlace1[i] <- beerGraphs$temp[[i]][1]
        if(length(beerGraphs$temp[[i]])>1){
          if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
            beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
            
          }
        }
        if(length(beerGraphs$temp[[i]])>2){
          if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
            beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
            
          }
        }
        
        
      }
      if(grepl("[[:lower:]]", beerGraphs$stPlace2[i]) & 
         !grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$temp[i] <- strsplit( beerGraphs$stPlace2[i], " ")
        
        beerGraphs$stPlace2[i] <- beerGraphs$temp[[i]][length(beerGraphs$temp[[i]])]
        beerGraphs$stPlace1[i] <- beerGraphs$temp[[i]][1]
        
        if(length(beerGraphs$temp[[i]])>1){
          if(grepl("[[:lower:]]",beerGraphs$temp[[i]][2])){
            beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2], sep=" ")
            
          }
        }
        if(length(beerGraphs$temp[[i]])>2){
          if(grepl("[[:lower:]]",beerGraphs$temp[[i]][3])){
            beerGraphs$stPlace1[i] <- paste(beerGraphs$temp[[i]][1],beerGraphs$temp[[i]][2],beerGraphs$temp[[i]][3], sep=" ")
            
          }
        }
      }
      beerGraphs$stPlace1[i] <- gsub("Bel m","Belém",beerGraphs$stPlace1[i] )
      
      
      for(j in 1:length(brazilProv)){
        if(beerGraphs$stPlace2[i]==brazilProvAbbv[j]){
          beerGraphs$stPlace2[i] <- brazilProv[j]
        }
        
      }
    }
    if(beerGraphs$stPlace3[i]=="France"){
      beerGraphs$stPlace2[i] <- gsub("B nifontaine","Bénifontaine",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("La Chapelle d Armenti res","La Chapelle d'Armentières",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Villeneuve d Ascq","Villeneuve d'Ascq",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Saint P re","Saint Père",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("C ré la Ronde","Céré la Ronde",beerGraphs$stPlace2[i] )
      
      beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i] , 7, nchar(beerGraphs$stPlace2[i] ))
    }
    if(beerGraphs$stPlace3[i]=="Austria"){
      beerGraphs$stPlace2[i] <- gsub("^A-","",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i] , 6, nchar(beerGraphs$stPlace2[i] ))
    }
    if(!is.na(beerGraphs$stPlace2[i])){
      if(beerGraphs$stPlace2[i]=="Russia" & length(beerGraphs$stLoc[[i]])>3){ # Russia country info labeled 2nd last
        beerGraphs$stPlace3[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-1]
        beerGraphs$stPlace2[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-2]
        beerGraphs$stPlace1[i] <- beerGraphs$stLoc[[i]][length(beerGraphs$stLoc[[i]])-3]
        
        if(beerGraphs$stPlace2[i] %in% c("St. Petersburg","Saint Petersburg","Москва","Moscow",
                                         "Sankt Peterburg","Moskva")){
          beerGraphs$stPlace1[i] <- NA
        }
      }
    }
    if(beerGraphs$stPlace3[i]=="Ireland"){
      beerGraphs$stPlace2[i] <- gsub("Co ","County ",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("[[:digit:]]","",beerGraphs$stPlace2[i] )
      
      if(!grepl("[[:lower:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- beerGraphs$stPlace1[i]
      }
    }
    if(beerGraphs$stPlace3[i]=="Sweden"){
      beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i], 8, nchar(beerGraphs$stPlace2[i] ))
      
      beerGraphs$stPlace2[i] <- gsub("G teborg","Göteborg",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("rebro","Örebro",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("V stra Frölunda","Västra Frölunda",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("V sterås","Västerås",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Luxembourg"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i], 6, nchar(beerGraphs$stPlace2[i] ))
      }
      beerGraphs$stPlace2[i] <- gsub("L tzebuerg","Luxembourg",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Czech Republic"){
      beerGraphs$stPlace3[i] <- "Czechia"
    }
    if(beerGraphs$stPlace3[i] =="Czechia"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],8, nchar(beerGraphs$stPlace2[i]))
      }
      beerGraphs$stPlace2[i] <- gsub("Star Město","Staré Město",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Protiv n","Protivín",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("hotěboř","Chotěboř",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Poland"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],8, nchar(beerGraphs$stPlace2[i]))
      }
      beerGraphs$stPlace2[i] <- gsub("Namysł w","Namysłów",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("W lka Kosowska","Wólka Kosowska",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Ciechan w","Ciechanów",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Krak w","Kraków",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Piotrk w","Piotrków",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Spain"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],7, nchar(beerGraphs$stPlace2[i]))
      }
      beerGraphs$stPlace2[i] <- gsub("C rdoba","Córdoba",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("M laga","Málaga",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="South Africa"){
      beerGraphs$stPlace2[i] <- beerGraphs$stPlace1[i]
      beerGraphs$stPlace1[i] <- NA
    }
    if(beerGraphs$stPlace3[i]=="Puerto Rico"){
      beerGraphs$stPlace2[i] <- beerGraphs$stPlace1[i]
      beerGraphs$stPlace1[i] <- NA
    }
    if(beerGraphs$stPlace3[i]=="Philippines"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],6, nchar(beerGraphs$stPlace2[i]))
      }
     
    }
    if(beerGraphs$stPlace3[i]=="Mexico"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace1[i])){
        beerGraphs$stPlace1[i] <- substr(beerGraphs$stPlace1[i],7, nchar(beerGraphs$stPlace1[i]))
      }
      beerGraphs$stPlace2[i] <- gsub("B C S","BCS",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("BCS ","BCS",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("B C","BC",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("BC ","BC",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Pue ","Pue",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Qro ","Qro",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("QRO","Qro",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("Jal ","Jal",beerGraphs$stPlace2[i] )
      beerGraphs$stPlace2[i] <- gsub("D F ","CDMX",beerGraphs$stPlace2[i] )
      
      for(j in 1:length(mexicoProv)){
        if(beerGraphs$stPlace2[i]==mexicoProvAbbv[j]){
          beerGraphs$stPlace2[i] <- mexicoProv[j]
        }
        
      }
    }
    if(beerGraphs$stPlace3[i]=="Greece"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-7)
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Portugal"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],6, nchar(beerGraphs$stPlace2[i]))
      }
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],5, nchar(beerGraphs$stPlace2[i]))
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Nepal"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Thailand"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Estonia"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],7, nchar(beerGraphs$stPlace2[i]))
      }
      
    }
    if(grepl("Singapore",beerGraphs$stPlace3[i])){
      beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace3[i],nchar(beerGraphs$stPlace3[i])-5, nchar(beerGraphs$stPlace3[i]))
      beerGraphs$stPlace3[i] <- substr(beerGraphs$stPlace3[i],1, nchar(beerGraphs$stPlace3[i])-7)
      
    }
    if(beerGraphs$stPlace3[i]=="Lithuania"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Slovenia"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],6, nchar(beerGraphs$stPlace2[i]))
      }
    }
    if(beerGraphs$stPlace3[i]=="Finland"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],7, nchar(beerGraphs$stPlace2[i]))
      }
    }
    if(beerGraphs$stPlace3[i]=="Bosnia and Herzegovina"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
    }
    if(beerGraphs$stPlace3[i]=="Cyprus"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-5)
      }
    }
    if(beerGraphs$stPlace3[i]=="Iceland"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],5, nchar(beerGraphs$stPlace2[i]))
      }
      if(grepl("Reykjavik",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- "Reykjavik"
      }
      
    }
    if(beerGraphs$stPlace3[i]=="Serbia"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
    }
    if(beerGraphs$stPlace3[i]=="Turks and Caicos Islands"){
      
    }
    if(beerGraphs$stPlace3[i]=="Peru"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-6)
      }
    }
    if(beerGraphs$stPlace3[i]=="Chile"){
      beerGraphs$stPlace2[i] <- gsub("Regi n","Región",beerGraphs$stPlace2[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Argentina"){
      if(grepl("CABA",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- "CABA"
      }
      beerGraphs$stPlace2[i] <- gsub("R o Negro","Río Negro",beerGraphs$stPlace2[i] )
      
    }
    if(grepl("Macedonia",beerGraphs$stPlace3[i])){
      beerGraphs$stPlace3[i] <- "Macedonia"
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-5)
      }
    }
    if(grepl("Hungary",beerGraphs$stPlace3[i])){
      if(grepl("[[:digit:]]",beerGraphs$stPlace3[i])){
        beerGraphs$stPlace3[i] <- substr(beerGraphs$stPlace3[i],6, nchar(beerGraphs$stPlace3[i]))
      }
      if(grepl("Budapest",beerGraphs$address[i])){
        beerGraphs$stPlace1[i] <- "Budapest"
      }
      beerGraphs$stPlace1[i] <- gsub("Balatonvil gos","Balatonvilágos",beerGraphs$stPlace1[i] )
      
    }
    if(beerGraphs$stPlace3[i]=="Slovakia"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],8, nchar(beerGraphs$stPlace2[i]))
      }
      
    }
    if(beerGraphs$stPlace3[i]=="India"){
      if(grepl("[[:digit:]]",beerGraphs$stPlace2[i])){
        beerGraphs$stPlace2[i] <- substr(beerGraphs$stPlace2[i],1, nchar(beerGraphs$stPlace2[i])-7)
      }
      
    }
    if(grepl("Myanmar",beerGraphs$stPlace3[i])  ){
      beerGraphs$stPlace3[i] <- "Myanmar"
    }
    if(!is.na(beerGraphs$stPlace2[i])){
      if(beerGraphs$stPlace2[i]=="China"  ){
        beerGraphs$stPlace3[i]<- beerGraphs$stPlace2[i]
        beerGraphs$stPlace2[i]<- beerGraphs$stPlace1[i]
        
      }
    }
    if(grepl("^China",beerGraphs$address[i])){
      beerGraphs$stPlace3[i]<- "China"
      beerGraphs$stPlace2[i]<- beerGraphs$stLoc[[i]][2]
    }
    
    
  }
  
}

# If we haven't found a country in Place3 from street address, remove that street info (random shit)
countriesFull <- c("United States","Denmark","Belgium","United Kingdom","New Zealand",
                   "Norway","Netherlands","Canada","Australia","Switzerland","Germany","Italy",
                   "Japan","Brazil","France","Austria","Russia","Ireland","Sweden","Hong Kong",
                   "China","Luxembourg","Czechia","Czech Republic","Poland","Spain","South Africa",
                   "Puerto Rico","Philippines","Cayman Islands","Mexico","Mozambique",
                   "Vatican City","Greece","Venezuela","Costa Rica","Israel","Barbados",
                   "Armenia","Jamaica","Bosnia and Herzegovina","Portugal","Croatia",
                   "Mauritius","Zambia","Egypt","Nepal","Thailand","Latvia","Cyprus","Estonia",
                   "Albania","Singapore","Vietnam","Guatemala","Bolivia","Lithuania","Guam",
                   "Colombia","Virgin Islands","Turkey","Finland","Namibia","Aruba",
                   "Slovenia","Zimbabwe","El Salvador","Bermuda","Palestine","Taiwan","Azerbaijan",
                   "Belarus","Iceland","Serbia","Belize","Honduras","Nicaragua","Panama","Cuba","The Bahamas",
                   "Turks and Caicos Islands","Haiti","Barbados","Aruba","Suriname","French Guiana","Peru","Chile",
                   "Argentina","Madagascar","Angola","Tanzania","Kenya","Ethiopia","Macedonia","Pristina","Romania",
                   "Hungary","Slovakia","India","Myanmar","Cambodia","Indonesia","Fiji","South Korea")
for(i in 1:nrow(beerGraphs)){
  if(!(beerGraphs$stPlace3[i] %in% countriesFull)){
    beerGraphs$stPlace3[i]<-beerGraphs$stPlace2[i]<-beerGraphs$stPlace1[i]<-NA
    
  }
  if((beerGraphs$stPlace3[i] %in% countriesFull)){
    if(is.na(beerGraphs$stPlace3[i])){
      beerGraphs$stPlace3[i]<- ""
    }
    if(is.na(beerGraphs$stPlace2[i])){
      beerGraphs$stPlace2[i]<- ""
    }
    if(is.na(beerGraphs$stPlace1[i])){
      beerGraphs$stPlace1[i]<- ""
    }
    
    
  }
  
}

# Need to combine Street Address into full string slightly differently by country
beerGraphs$stLocationFull <-NA
for(i in 1:nrow(beerGraphs)){
  if(beerGraphs$stPlace3[i] %in% c("United States","Canada","Australia","Italy","Japan",
                                   "Brazil","Russia","Ireland","Philippines","Mexico","Venezuela",
                                   "Costa Rica","Colombia","Chile","Uruguay","South Korea","India")){
    beerGraphs$stLocationFull[i] <- paste(beerGraphs$stPlace1[i],
                                          beerGraphs$stPlace2[i],
                                          beerGraphs$stPlace3[i], sep = ", ")
  }
  if(beerGraphs$stPlace3[i] %in% c("Denmark","Belgium","United Kingdom","New Zealand",
                                   "Norway","Netherlands","Switzerland","Germany","France",
                                   "Austria","Sweden","Hong Kong","Luxembourg","Czechia",
                                   "Poland","Spain","South Africa","Puerto Rico","Cayman Islands",
                                   "Greece","Israel","Barbados","Armenia","Jamaica","Bosnia and Herzegovina",
                                   "Portugal","Croatia","Mauritius","Zambia","Egypt","Nepal","Thailand",
                                   "Latvia","Cyprus","Estonia","Albania","Singapore",
                                   "Lithuania","Finland","Namibia","Aruba","Slovenia","Belarus","Guatemala","Belize",
                                   "Honduras","Nicaragua","Panama","Cuba","The Bahamas","Turks and Caicos Islands","Haiti",
                                   "Barbados","Aruba","Suriname","French Guiana","Peru","Bolivia","Argentina","Madagascar","Zimbabwe",
                                   "Mozambique","Angola","Tanzania","Kenya","Ethiopia","Macedonia","Romania","Slovakia",
                                   "Myanmar","Cambodia","Indonesia","Fiji","China","Iceland","Serbia")){
    beerGraphs$stLocationFull[i] <- paste(beerGraphs$stPlace2[i],
                                          beerGraphs$stPlace3[i], sep = ", ")
  }
  if(beerGraphs$stPlace3[i] %in% c("Bermuda","Pristina")){
    beerGraphs$stLocationFull[i] <- beerGraphs$stPlace3[i]
  }
  if(beerGraphs$stPlace3[i] %in% c("Argentina")& beerGraphs$stPlace2[i]!="CABA"){
    beerGraphs$stLocationFull[i] <- paste(beerGraphs$stPlace1[i],
                                          beerGraphs$stPlace2[i],
                                          beerGraphs$stPlace3[i], sep = ", ")
  }
  if(beerGraphs$stPlace3[i] %in% c("China")& grepl("Sheng",beerGraphs$stPlace2[i])){
    beerGraphs$stLocationFull[i] <- paste(beerGraphs$stPlace1[i],
                                          beerGraphs$stPlace2[i],
                                          beerGraphs$stPlace3[i], sep = ", ")
  }
  if(beerGraphs$stPlace3[i] %in% c("Hungary","Vietnam")){
    beerGraphs$stLocationFull[i] <- paste(beerGraphs$stPlace1[i],
                                          beerGraphs$stPlace3[i], sep = ", ")
  }
}

# Fix ~60 locations where found Country != Country info from Beergraphs
for(i in 1:nrow(beerGraphs)){
  # Remove looked up street info
  if(beerGraphs$Brewery[i] %in% c("Evil Twin Brewing","BrewCult","Arbor Ales","Extraomnes","Fortitude Brewing Company",
                                  "Birrificio Menaresta","Brouwerij De Dochter van de Korenaar","Purity Brewing Co.",
                                  "Aspall","Innocente Brewing Company","Haven Brewing","Walkerville Brewery","Loch Ness Brewery","Okells","Wahoo Brewing Company",
                                  "Dungarvan Brewing Company","Buskers Beer","Island Brewing Co. (Fiji)","Cervecería Austral S.A.",
                                  "Bahamian Brewery & Beverage Co.","Ayr Brewing Company","Abbaye Mont des Cats","Free Lions","Tin Mill Brewery",
                                  "Taps Brewing Company","De Fontein","Scottish Borders Brewery","Brasserie BVM Inc. (Vieux-Montreal)",
                                  "Les 3 Brasseurs (The 3 Brewers)","Brasserie Artisanale Millevertus","Cervecería Cuauhtémoc Moctezuma (Heineken)")){
    beerGraphs$LocationFull[i] <- NA
  }
  
  # Remove BeerGraphs info
  if(beerGraphs$Brewery[i] %in% c("Townshend Brewery","Titanic Brewery","Rebellion Beer Co.","Beer Here","2 Brothers Brewery","Pioneer Brewing Company",
                                  "Port Brewing Company / Pizza Port","Grain Brewery","Westons","Grimor","Arundel","Beer By Cartel","Kensington Brewing Company","Beer Engineers",
                                  "Brew Brothers Brewery","Copper Dragon","De Bierderie","Galway Hooker","Craft Liquors","Abbey Ales","K's Brewing Company",
                                  "Phoenix Beverages","Pub Dog Brewing","Donn","De Pauw","Sterling Beer Company","Cumberland","B&T Brewery","Vanberg & DeWulf",
                                  "Churchkey Can Co.","Blue Sky Brewery","Bierbrouwerij de Leeuw (Haacht)","Bird's Brewery","Hepworth & Co","Great Deep Brewing Company",
                                  "Triangle Brewing Company","Chameleon Brewing","York Brewery Company Limited","Way Beer")){
    beerGraphs$stLocationFull[i] <- NA
  }
  if(beerGraphs$Brewery[i] %in% c("Trouble Brewing")){
    beerGraphs$stLocationFull[i] <- NA
    beerGraphs$LocationFull[i] <- NA
  }
}


# Move location rows to end to easier see
beerGraphs <- beerGraphs[ ,c(names(beerGraphs)[!(names(beerGraphs)%in% c("LocationFull","stLocationFull"))],"LocationFull","stLocationFull")]

# Must decide which location to geocode
beerGraphs$FinalLocationFull <- NA
for(i in 1:nrow(beerGraphs)){
  if(is.na(beerGraphs$LocationFull[i])){
    beerGraphs$FinalLocationFull[i] <- beerGraphs$stLocationFull[i]
  }
  if(is.na(beerGraphs$stLocationFull[i])){
    beerGraphs$FinalLocationFull[i] <- beerGraphs$LocationFull[i]
  }
  if(!is.na(beerGraphs$place3[i]) & !is.na(beerGraphs$place2[i])& !is.na(beerGraphs$stPlace3[i]) & !is.na(beerGraphs$stPlace2[i])){
    if(beerGraphs$place3[i]=="United States" & beerGraphs$stPlace3[i]=="United States" & 
       beerGraphs$place2[i]!=beerGraphs$stPlace2[i]){
      beerGraphs$FinalLocationFull[i] <- beerGraphs$LocationFull[i]
    }
  }
  if(!is.na(beerGraphs$stLocationFull[i])& !is.na(beerGraphs$LocationFull[i])){
    if(beerGraphs$stLocationFull[i]==beerGraphs$LocationFull[i]){
      beerGraphs$FinalLocationFull[i] <- beerGraphs$LocationFull[i]
    }
  }
  if(!is.na(beerGraphs$place3[i]) & !is.na(beerGraphs$stPlace3[i]) ){
    if(beerGraphs$stPlace3[i]==beerGraphs$place3[i] & is.na(beerGraphs$FinalLocationFull[i]) & 
       beerGraphs$stPlace3[i]!="United States"){
      if(nchar(beerGraphs$stLocationFull[i])>nchar(beerGraphs$LocationFull[i])){
        beerGraphs$FinalLocationFull[i] <- beerGraphs$stLocationFull[i]
      }else {
        beerGraphs$FinalLocationFull[i] <- beerGraphs$LocationFull[i]
      }
    }
  }
  if(!is.na(beerGraphs$place3[i]) & !is.na(beerGraphs$place2[i])& !is.na(beerGraphs$stPlace3[i]) & !is.na(beerGraphs$stPlace2[i])&
     beerGraphs$stPlace3[i]=="United States" & is.na(beerGraphs$FinalLocationFull[i])){
    beerGraphs$FinalLocationFull[i]<-beerGraphs$stLocationFull[i]
  }
  if(is.na(beerGraphs$FinalLocationFull[i]) & !is.na(beerGraphs$LocationFull[i])& !is.na(beerGraphs$stLocationFull[i])){
    beerGraphs$FinalLocationFull[i]<-beerGraphs$stLocationFull[i]
  }
  if(!is.na(beerGraphs$stPlace3[i])){
    if(beerGraphs$stPlace3[i]=="Cyprus"){
      beerGraphs$FinalLocationFull[i]<-beerGraphs$stLocationFull[i]
    }
    if(beerGraphs$stPlace3[i]=="Singapore"){
      beerGraphs$FinalLocationFull[i]<-beerGraphs$stLocationFull[i]
    }
  }
}

saveRDS(beerGraphs, "data/BeerGraphsBreweries(wStreets).rds")

