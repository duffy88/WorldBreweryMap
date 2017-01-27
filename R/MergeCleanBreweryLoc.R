# Merge and Clean Beer Graphs Brewery Info and Location

# Load Brewery Info and Location data
beerGraphs <- readRDS("data/BeerGraphsBreweries(wStreets).rds")
beerGraphsBeers <- readRDS("data/BeerGraphsBeers.rds")

logos <- readRDS("data/LogoLinks/BreweryLogoLinks.rds")

logos <- logos[ !duplicated(logos$Brewery), ]


temp1 <- readRDS("data/Locations2017Jan/BreweryLocations(Batch-1)FullLoc.rds")
names(temp1) <- c("Brewery","Location","lon","lat")
temp2 <- readRDS("data/Locations2017Jan/BreweryLocations(Batch-2)FullLoc.rds")
names(temp2) <- c("Brewery","Location","lon","lat")
temp3 <- readRDS("data/Locations2017Jan/BreweryLocations(Batch-3)FullLoc.rds")
names(temp3) <- c("Brewery","Location","lon","lat")
temp4 <- readRDS("data/Locations2017Jan/BreweryLocations(Batch-4)FullLoc.rds")
names(temp4) <- c("Brewery","Location","lon","lat")

temp <- rbind(temp1, temp2, temp3,temp4)

beerGraphs <- merge(beerGraphs, temp[, c("Brewery","lon","lat")], by=c("Brewery"), all.x=T)
beerGraphs <- merge(beerGraphs, logos[ ,c("Brewery","logoLink")], by=c("Brewery"), all.x=T)

beerGraphs <- beerGraphs[ order(beerGraphs$TotalBAR, decreasing=T),]
beerGraphs$logoLink <- as.character(beerGraphs$logoLink)
beerGraphs$link <- as.character(beerGraphs$link)

# Fix some random geocoding errors
for(i in 1:nrow(beerGraphs)){
  if(!is.na(beerGraphs$FinalLocationFull[i])){
    # Missed Geocodes
    if(beerGraphs$FinalLocationFull[i]=="Leeds, West Yorkshire, United Kingdom"){
      beerGraphs$lat[i] <- 53.8008
      beerGraphs$lon[i] <- -1.5491
      
    }
    if(beerGraphs$FinalLocationFull[i]=="Bellingham, Washington, United States"){
      beerGraphs$lat[i] <- 48.7519
      beerGraphs$lon[i] <- -122.4787
      
    }
    if(beerGraphs$FinalLocationFull[i]=="Camperdown, New South Wales, Australia"){
      beerGraphs$lat[i] <- -33.8901
      beerGraphs$lon[i] <- 151.1799
      
    }
    if(beerGraphs$FinalLocationFull[i]=="Gainesville, Florida, United States"){
      beerGraphs$lat[i] <- 29.6516
      beerGraphs$lon[i] <- -82.3248
      
    }
    
    
    # Weird addresses, most end at center of state/country
    if(beerGraphs$Brewery[i]=="Big Horn Brewery (RAM Restaurant & Brewery / C.B. & Potts)"){
      beerGraphs$FinalLocationFull[i] <- "Lakewood, Washington, United States"
      beerGraphs$lat[i] <- 47.1718
      beerGraphs$lon[i] <- -122.5185
      
    }
    if(beerGraphs$Brewery[i]=="Dennis Beer Co."){
      beerGraphs$FinalLocationFull[i] <- "Sydney, New South Wales, Australia"
      beerGraphs$lat[i] <- -33.8688
      beerGraphs$lon[i] <- 151.2093
      
    }
    if(beerGraphs$Brewery[i]=="Microbrasserie Charlevoix"){
      beerGraphs$FinalLocationFull[i] <- "Baie-Saint-Paul, Quebec, Canada"
      beerGraphs$lat[i] <- 47.443252
      beerGraphs$lon[i] <- -70.508662
      
    }
    if(beerGraphs$Brewery[i]=="Frampton Brasse"){
      beerGraphs$FinalLocationFull[i] <- "Frampton, Quebec, Canada"
      beerGraphs$lat[i] <- 46.4600
      beerGraphs$lon[i] <- -70.8064
      
    }
    if(beerGraphs$Brewery[i]=="Bronher"){
      beerGraphs$FinalLocationFull[i] <- "Villajoyosa, Alicante, Spain"
      beerGraphs$lat[i] <- 38.5080
      beerGraphs$lon[i] <- -0.2286
      
    }
    if(beerGraphs$Brewery[i]=="Szałpiw"){
      beerGraphs$FinalLocationFull[i] <- "Poznań, Poland"
      beerGraphs$lat[i] <- 52.4064
      beerGraphs$lon[i] <- 16.9252
      
    }
    if(beerGraphs$Brewery[i]=="Pryes Brewing"){
      beerGraphs$FinalLocationFull[i] <- "Minneapolis, Minnesota, United States"
      beerGraphs$lat[i] <- 44.9778
      beerGraphs$lon[i] <- -93.2650
      
    }
    if(beerGraphs$Brewery[i]=="Casella Family Brewers"){
      beerGraphs$FinalLocationFull[i] <- "Yenda, New South Wales, Australia"
      beerGraphs$lat[i] <- -34.2452
      beerGraphs$lon[i] <- 146.2027
      
    }
    if(beerGraphs$Brewery[i]=="Caleya"){
      beerGraphs$FinalLocationFull[i] <- "Madrid, Spain"
      beerGraphs$link[i] <- "http://www.cervezacaleya.com/"
      beerGraphs$lat[i] <- 40.4168
      beerGraphs$lon[i] <- -3.7038
      
    }
    if(beerGraphs$Brewery[i]=="Via Priula"){
      beerGraphs$FinalLocationFull[i] <- "San Pellegrino Terme, Bergamo, Italy"
      beerGraphs$link[i] <- "http://www.birrificioviapriula.it/en"
      beerGraphs$lat[i] <- 45.8416
      beerGraphs$lon[i] <- 9.6667
      
    }
    if(beerGraphs$Brewery[i]=="Malka (מלכה)"){
      beerGraphs$FinalLocationFull[i] <- "Yehiam, Israel"
      beerGraphs$lat[i] <- 32.9961
      beerGraphs$lon[i] <- 35.2211
      
    }
    if(beerGraphs$Brewery[i]=="Distillerie de Genievre Claeyssens"){
      beerGraphs$FinalLocationFull[i] <- "Wambrechies, France"
      beerGraphs$link[i] <- "http://www.wambrechies.com/index.php?lg=fpdb/wambr_fr&page1=a-visite.htm"
      beerGraphs$lat[i] <- 50.6875
      beerGraphs$lon[i] <- 3.0524
      
    }
    if(beerGraphs$Brewery[i]=="Prospectors Brewing Company"){
      beerGraphs$FinalLocationFull[i] <- "Mariposa, California, United States"
      beerGraphs$link[i] <- "http://www.prospectorsbrewingcompany.com/"
      beerGraphs$lat[i] <- 37.4849
      beerGraphs$lon[i] <- -119.9663
      
    }
    if(beerGraphs$Brewery[i]=="La Voie Maltee"){
      beerGraphs$FinalLocationFull[i] <- "Jonquière, Quebec, Canada"
      beerGraphs$lat[i] <- 48.4236
      beerGraphs$lon[i] <- -71.2395
      
    }
    if(beerGraphs$Brewery[i]=="Skebo Bruksbryggeri"){
      beerGraphs$FinalLocationFull[i] <- "Skebobruk, Sweden"
      beerGraphs$link[i] <- "http://www.skebobruksbryggeri.se/"
      beerGraphs$lat[i] <- 59.9628
      beerGraphs$lon[i] <- 18.6065
      
    }
    if(beerGraphs$Brewery[i]=="Browar Kasztelan (Carlsberg Polska)"){
      beerGraphs$FinalLocationFull[i] <- "Sierpc, Poland"
      beerGraphs$link[i] <- "http://www.browarkasztelan.pl/"
      beerGraphs$lat[i] <- 52.8568
      beerGraphs$lon[i] <- 19.6694
      
    }
    if(beerGraphs$Brewery[i]=="Rethymnian Brewery"){
      beerGraphs$FinalLocationFull[i] <- "Armeni,Greece"
      beerGraphs$link[i] <- "http://www.brinks-beer.gr/index.php/en/brinks"
      beerGraphs$lat[i] <- 35.4292
      beerGraphs$lon[i] <- 24.1589
      
    }
    if(beerGraphs$Brewery[i]=="Trollbryggeriet"){
      beerGraphs$FinalLocationFull[i] <- "Liabygda, Norway"
      beerGraphs$link[i] <- "http://www.trollbryggeriet.no/"
      beerGraphs$lat[i] <- 62.3119
      beerGraphs$lon[i] <- 7.0526
      
    }
    if(beerGraphs$Brewery[i]=="Carlsberg Sverige (Carlsberg Group)"){
      beerGraphs$FinalLocationFull[i] <- "Falkenberg, Sweden"
      beerGraphs$link[i] <- "http://www.carlsbergsverige.se/Sidor/default.aspx"
      beerGraphs$lat[i] <- 56.9027
      beerGraphs$lon[i] <- 12.4888
      
    }
    if(beerGraphs$Brewery[i]=="Allgäuer Brauhaus (Radeberger Gruppe)"){
      beerGraphs$FinalLocationFull[i] <- "Kempten, Bavaria, Germany"
      beerGraphs$link[i] <- "http://www.allgaeuer-brauhaus.de/"
      beerGraphs$lat[i] <- 47.7286
      beerGraphs$lon[i] <- 10.3158
      
    }
    if(beerGraphs$Brewery[i]=="Friesisches Brauhaus zu Jever (Radeberger Gruppe)"){
      beerGraphs$FinalLocationFull[i] <- "Jever, Lower Saxony, Germany"
      beerGraphs$link[i] <- "http://www.jever.de/"
      beerGraphs$lat[i] <- 53.5705
      beerGraphs$lon[i] <- 7.8970
      
    }
    if(beerGraphs$Brewery[i]=="Einbecker Brauhaus"){
      beerGraphs$FinalLocationFull[i] <- "Einbeck, Lower Saxony, Germany"
      beerGraphs$link[i] <- "http://www.einbecker.de/home-en.html"
      beerGraphs$lat[i] <- 51.8206
      beerGraphs$lon[i] <- 9.8683
      
    }
    if(beerGraphs$Brewery[i]=="Privatbrauerei Hoepfner"){
      beerGraphs$FinalLocationFull[i] <- "Karlsruhe, Baden-Württemberg, Germany"
      beerGraphs$link[i] <- "http://www.hoepfner.de/vorschaltseite.html"
      beerGraphs$lat[i] <- 49.0069
      beerGraphs$lon[i] <- 8.4037
      
    }
    if(beerGraphs$Brewery[i]=="Pfungstädter Privatbrauerei"){
      beerGraphs$FinalLocationFull[i] <- "Pfungstadt, Hesse, Germany"
      beerGraphs$lat[i] <- 49.8029
      beerGraphs$lon[i] <- 8.6035
      
    }
    if(beerGraphs$Brewery[i]=="Privatbrauerei Bosch"){
      beerGraphs$FinalLocationFull[i] <- "Bad Laasphe, North Rhine-Westphalia, Germany"
      beerGraphs$link[i] <- "http://www.brauerei-bosch.de/"
      beerGraphs$lat[i] <- 50.9279
      beerGraphs$lon[i] <- 8.4195
      
    }
    if(beerGraphs$Brewery[i]=="Schlossbrauerei Eichhofen"){
      beerGraphs$FinalLocationFull[i] <- "Nittendorf, Bavaria, Germany"
      beerGraphs$lat[i] <- 49.0243
      beerGraphs$lon[i] <- 11.9605
      
    }
    if(beerGraphs$Brewery[i]=="Brouwerij De Lelie"){
      beerGraphs$FinalLocationFull[i] <- "Vosselaar, Antwerp, Belgium"
      beerGraphs$lat[i] <- 51.3147
      beerGraphs$lon[i] <- 4.8877
      
    }
    if(beerGraphs$Brewery[i]=="Dockyard Brewing Company"){
      beerGraphs$FinalLocationFull[i] <- "Royal Naval Dockyard, Bermuda"
      beerGraphs$lat[i] <- 32.3266
      beerGraphs$lon[i] <- -64.8334
      
    }
    if(beerGraphs$Brewery[i]=="Bionade"){
      beerGraphs$FinalLocationFull[i] <- "Ostheim, Bavaria, Germany"
      beerGraphs$link[i] <- "http://www.bionade.de/en/"
      beerGraphs$lat[i] <- 50.4608
      beerGraphs$lon[i] <- 10.2305
      
    }
    if(beerGraphs$Brewery[i]=="Brauerei Diebels (ABInBev)"){
      beerGraphs$FinalLocationFull[i] <- "Issum, North Rhine-Westphalia, Germany"
      beerGraphs$link[i] <- "http://www.diebels.de/"
      beerGraphs$lat[i] <- 51.5381 
      beerGraphs$lon[i] <- 6.4237
      
    }
    if(beerGraphs$Brewery[i]=="Tyris"){
      beerGraphs$FinalLocationFull[i] <- "Riba-roja de Túria, Spain"
      beerGraphs$lat[i] <- 39.5456
      beerGraphs$lon[i] <- -0.5713
      
    }
    if(beerGraphs$Brewery[i]=="Feldschlößchen"){
      beerGraphs$FinalLocationFull[i] <- "Dresden, Saxony, Germany"
      beerGraphs$link[i] <- "http://www.feldschloesschen.com/en/Pages/default.aspx"
      beerGraphs$lat[i] <- 51.0504
      beerGraphs$lon[i] <- 13.7373
      
    }
    if(beerGraphs$Brewery[i]=="Olvi"){
      beerGraphs$FinalLocationFull[i] <- "Iisalmi, Finland"
      beerGraphs$link[i] <- "http://www.olvi.fi/web/en"
      beerGraphs$lat[i] <- 63.5694
      beerGraphs$lon[i] <- 27.1826
      
    }
    if(beerGraphs$Brewery[i]=="Oettinger Brauerei"){
      beerGraphs$FinalLocationFull[i] <- "Oettingen in Bayern, Bavaria, Germany"
      beerGraphs$lat[i] <- 48.9518
      beerGraphs$lon[i] <- 10.6021
      
    }
    if(beerGraphs$Brewery[i]=="Brewery John Martin & Brewery Timmermans"){
      beerGraphs$FinalLocationFull[i] <- "Dilbeek, Flanders, Belgium"
      beerGraphs$lat[i] <- 50.8465
      beerGraphs$lon[i] <- 4.2621
      
    }
    if(beerGraphs$Brewery[i]=="Royal Unibrew"){
      beerGraphs$FinalLocationFull[i] <- "Faxe, Denmark"
      beerGraphs$lat[i] <- 55.2551 
      beerGraphs$lon[i] <- 12.1138
      
    }
    if(beerGraphs$Brewery[i]=="Sabaja Craft Brewery"){
      beerGraphs$FinalLocationFull[i] <- "Pristina, Kosovo"
     
    }
    if(beerGraphs$Brewery[i]=="Kongsberg"){
      beerGraphs$FinalLocationFull[i] <- "Kongsberg, Norway"
      beerGraphs$lat[i] <- 59.6689
      beerGraphs$lon[i] <- 9.6502
      
    }
    if(beerGraphs$Brewery[i]=="Delta Breweries (SAB Miller)"){
      beerGraphs$FinalLocationFull[i] <- "Harare, Zimbabwe"
      beerGraphs$link[i] <- "http://www.delta.co.zw/"
      beerGraphs$lat[i] <- -17.8252
      beerGraphs$lon[i] <- 31.0335
      
    }
    if(beerGraphs$Brewery[i]=="Gotlands Bryggerier"){
      beerGraphs$FinalLocationFull[i] <- "Visby, Sweden"
      beerGraphs$lat[i] <- 57.6348
      beerGraphs$lon[i] <- 18.2948
      
    }
    if(beerGraphs$Brewery[i]=="Birrificio Artigianale Lariano"){
      beerGraphs$FinalLocationFull[i] <- "Sirone, Lecco, Italy"
      beerGraphs$lat[i] <- 45.7756
      beerGraphs$lon[i] <- 9.3262
      
    }
    if(beerGraphs$Brewery[i]=="Karksi Brewery"){
      beerGraphs$FinalLocationFull[i] <- "Viljandimaa, Estonia"
      beerGraphs$lat[i] <- 58.2822
      beerGraphs$lon[i] <- 25.5752
      
    }
    if(beerGraphs$Brewery[i]=="UAB Gubernija"){
      beerGraphs$FinalLocationFull[i] <- "Šiauliai, Lithuania"
      beerGraphs$lat[i] <- 55.9349
      beerGraphs$lon[i] <- 23.3137
      
    }
    if(beerGraphs$Brewery[i]=="Abbaye D'Aulne"){
      beerGraphs$FinalLocationFull[i] <- "Thuin, Hainaut, Belgium"
      beerGraphs$lat[i] <- 50.3408
      beerGraphs$lon[i] <- 4.2873
      
    }
    if(beerGraphs$Brewery[i]=="Dalgety Brewing Company"){
      beerGraphs$FinalLocationFull[i] <- "Dalgety, New South Wales, Australia"
      beerGraphs$lat[i] <- -36.5000 
      beerGraphs$lon[i] <- 148.8333
      
    }
    if(beerGraphs$Brewery[i]=="Hoa Vien"){
      beerGraphs$FinalLocationFull[i] <- "Hồ Chí Minh, Vietnam"
      beerGraphs$link[i] <- "http://www.hoavien.vn/"
      beerGraphs$lat[i] <- 10.8231
      beerGraphs$lon[i] <- 106.6297
      
    }
    if(beerGraphs$Brewery[i]=="Taybeh Brewing Company"){
      beerGraphs$FinalLocationFull[i] <- "Ramallah, Palestine"
      beerGraphs$lat[i] <- 31.9038 
      beerGraphs$lon[i] <- 35.2034
      
    }
    if(beerGraphs$Brewery[i]=="Brouwerij Smisje"){
      beerGraphs$FinalLocationFull[i] <- "Oudenaarde, East Flanders, Belgium"
      beerGraphs$lat[i] <- 50.8470  
      beerGraphs$lon[i] <- 3.6014
      
    }
    if(beerGraphs$Brewery[i]=="Brouwerij Smisje"){
      beerGraphs$FinalLocationFull[i] <- "Oudenaarde, East Flanders, Belgium"
      beerGraphs$lat[i] <- 50.8470  
      beerGraphs$lon[i] <- 3.6014
      
    }
    if(beerGraphs$Brewery[i]=="Brasserie des Fagnes"){
      beerGraphs$FinalLocationFull[i] <- "Couvin, Namur, Beglium"
      beerGraphs$lat[i] <- 50.0520  
      beerGraphs$lon[i] <- 4.4946
      
    }
    if(beerGraphs$Brewery[i]=="Golan Brewery (מבשלת הגולן)"){
      beerGraphs$FinalLocationFull[i] <- "Qatsrin, Golan Heights"
      beerGraphs$lat[i] <- 32.9910  
      beerGraphs$lon[i] <- 35.6899
      
    }
    if(beerGraphs$Brewery[i]=="Melo Abreu"){
      beerGraphs$FinalLocationFull[i] <- "Ilha De São Miguel, Portugal"
      beerGraphs$link[i] <- "http://www.hdgazores.com/melo-abreu/"
      beerGraphs$lat[i] <- 37.7804 
      beerGraphs$lon[i] <- -25.4970
      
    }
    if(beerGraphs$Brewery[i]=="Brugghús Steðja"){
      beerGraphs$FinalLocationFull[i] <- "Steðja, Iceland"
      beerGraphs$lat[i] <- 64.6152547 
      beerGraphs$lon[i] <- -21.4846874 
      
    }
    if(beerGraphs$Brewery[i]=="Muifelbrouwerij"){
      beerGraphs$FinalLocationFull[i] <- "Megen, North Brabant, Netherlands"
      beerGraphs$lat[i] <- 51.8214
      beerGraphs$lon[i] <- 5.5636  
      
    }
    if(beerGraphs$Brewery[i]=="Dommelsche Bierbrouwerij (ABInBev)"){ 
      beerGraphs$FinalLocationFull[i] <- "Dommelen, North Brabant, Netherlands"
      beerGraphs$link[i] <- "http://www.dommelsch.nl/"
      beerGraphs$lat[i] <- 51.3473
      beerGraphs$lon[i] <- 5.4317  
      
    }
    if(beerGraphs$Brewery[i]=="Brauerei Kapsreiter"){ 
      beerGraphs$FinalLocationFull[i] <- "Schärding, Upper Austria, Austria"
      beerGraphs$link[i] <- "https://www.beeradvocate.com/beer/profile/4459/"
      beerGraphs$lat[i] <- 48.4605 
      beerGraphs$lon[i] <- 13.4327  
      
    }
    if(beerGraphs$Brewery[i]=="Docqmans"){ 
      beerGraphs$FinalLocationFull[i] <- "Tiel, Netherlands"
      beerGraphs$link[i] <- "http://www.docqmans.com/"
      beerGraphs$lat[i] <- 51.8876  
      beerGraphs$lon[i] <- 5.4279  
      
    }
    if(beerGraphs$Brewery[i]=="Cerveceria La Constancia (SABMiller)"){ 
      beerGraphs$FinalLocationFull[i] <- "Santa Ana, El Salvador"
      beerGraphs$lat[i] <- 13.9778  
      beerGraphs$lon[i] <- -89.5639  
      
    }
    if(beerGraphs$Brewery[i]=="Rinkuškiai"){ 
      beerGraphs$FinalLocationFull[i] <- "Biržai, Lithuania"
      beerGraphs$link[i] <- "http://www.rinkuskiai.lt/"
      beerGraphs$lat[i] <- 56.2018 
      beerGraphs$lon[i] <- 24.7560  
      
    }
    if(beerGraphs$Brewery[i]=="Monteith's Brewing Co. (DB Breweries)"){ 
      beerGraphs$FinalLocationFull[i] <- "Greymouth, New Zealand"
      beerGraphs$link[i] <- "http://www.monteiths.co.nz/"
      beerGraphs$lat[i] <- -42.4504 
      beerGraphs$lon[i] <- 171.2108  
      
    }
    if(beerGraphs$Brewery[i]=="Al Ahram Beverages Company (Heineken)"){ 
      beerGraphs$FinalLocationFull[i] <- "Zamalek, Cairo, Egypt"
      beerGraphs$lat[i] <- 30.0609
      beerGraphs$lon[i] <- 31.2197  
      
    }
    if(beerGraphs$Brewery[i]=="Negev Brewery"){ 
      beerGraphs$FinalLocationFull[i] <- "Kiryat Gat, Israel"
      beerGraphs$lat[i] <- 31.6111
      beerGraphs$lon[i] <- 34.7685  
      
    }
    if(beerGraphs$Brewery[i]=="Gæðingur"){ 
      beerGraphs$FinalLocationFull[i] <- "Saudarkrokr, Iceland"
      beerGraphs$lat[i] <- 65.7409
      beerGraphs$lon[i] <- -19.6405  
      
    }
    if(beerGraphs$Brewery[i]=="Northern & Co"){ 
      beerGraphs$FinalLocationFull[i] <- "Fedje, Norway"
      beerGraphs$link[i] <- "https://www.facebook.com/Northern-Co-769261099794198/"
      beerGraphs$logoLink[i] <- "https://scontent-yyz1-1.xx.fbcdn.net/v/t1.0-1/11230843_828558590531115_8886645266244026824_n.jpg?oh=c27f5ff3989e2a222386e46e6e6a4cd4&oe=59008B7B"
      beerGraphs$lat[i] <- 60.7791076   
      beerGraphs$lon[i] <- 4.7209453  
      
    }
    if(beerGraphs$Brewery[i]=="Cotoya"){ 
      beerGraphs$FinalLocationFull[i] <- "Lugones, Asturias, Spain"
      beerGraphs$link[i] <- "http://cervezacotoya.blogspot.ca/"
      beerGraphs$logoLink[i] <- "https://scontent-yyz1-1.xx.fbcdn.net/v/t1.0-9/11987081_1668267606720569_7175396441811560636_n.jpg?oh=94b41595a319fd19aaa911234cb1b067&oe=590A5CDA"
      beerGraphs$lat[i] <- 43.4027 
      beerGraphs$lon[i] <- -5.8109  
      
    }
    if(beerGraphs$Brewery[i]=="Shapiro (שפירה)"){ 
      beerGraphs$FinalLocationFull[i] <- "Beit Shemesh, Israel"
      beerGraphs$lat[i] <- 31.7470  
      beerGraphs$lon[i] <- 34.9881  
      
    }
    if(beerGraphs$Brewery[i]=="Left Field Cider Co."){ 
      beerGraphs$FinalLocationFull[i] <- "Logan Lake, British Columbia, Canada"
      beerGraphs$lat[i] <- 50.4912   
      beerGraphs$lon[i] <- -120.8153  
      
    }
    if(beerGraphs$Brewery[i]=="Brasserie d'Ebly"){ 
      beerGraphs$FinalLocationFull[i] <- "Ebly, Walloon Region, Belgium"
      beerGraphs$lat[i] <- 49.8531   
      beerGraphs$lon[i] <- 5.5404  
      
    }
    if(beerGraphs$Brewery[i]=="Empresa de Cervejas da Madeira"){ 
      beerGraphs$FinalLocationFull[i] <- "Funchal, Madeira, Portugal"
      beerGraphs$lat[i] <- 32.6669   
      beerGraphs$lon[i] <- -16.9241  
      
    }
    if(beerGraphs$Brewery[i]=="Tya Bryggeri"){ 
      beerGraphs$FinalLocationFull[i] <- "Øvre Årdal, Norway"
      beerGraphs$link[i] <- "http://tya-bryggeri.no/"
      beerGraphs$lat[i] <- 61.3089   
      beerGraphs$lon[i] <- 7.8028  
      
    }
    if(beerGraphs$Brewery[i]=="Ruby Mountain Brewing Company"){ 
      beerGraphs$link[i] <- "http://www.rubymountainbrewing.com/"
      beerGraphs$lat[i] <- 41.012419   
      beerGraphs$lon[i] <- -115.007516  
      
    }
    if(beerGraphs$Brewery[i]=="Birra Korca Shpk Korce"){ 
      beerGraphs$FinalLocationFull[i] <- "Korçë, Albania"
      beerGraphs$lat[i] <- 40.6141
      beerGraphs$lon[i] <- 20.7778  
      
    }
    if(beerGraphs$Brewery[i]=="Brauerei Zipf"){ 
      beerGraphs$FinalLocationFull[i] <- "Neukirchen an der Vöckla, Austria"
      beerGraphs$link[i] <- "http://www.brauhaus-zipf.at/"
      beerGraphs$lat[i] <- 48.0396 
      beerGraphs$lon[i] <- 13.5401  
      
    }
    if(beerGraphs$Brewery[i]=="Lindesnes Brygghus"){ 
      beerGraphs$FinalLocationFull[i] <- "Spangereid, Norway"
      beerGraphs$lat[i] <- 58.0454
      beerGraphs$lon[i] <- 7.1441  
      
    }
    if(beerGraphs$Brewery[i]=="Broughton Ales"){ 
      beerGraphs$FinalLocationFull[i] <- "Biggar, United Kingdom"
      beerGraphs$lat[i] <- 55.6234
      beerGraphs$lon[i] <- -3.5240  
      
    }
    if(beerGraphs$Brewery[i]=="Brasserie Artisanale Millevertus"){ 
      beerGraphs$FinalLocationFull[i] <- "Tintigny, Luxembourg, Belgium"
      beerGraphs$lat[i] <- 49.6833 
      beerGraphs$lon[i] <- 5.5131  
      
    }
    
    
    
    # Weird logos
    if(beerGraphs$Brewery[i]=="Dad & Dude's Breweria"){
      beerGraphs$logoLink[i] <- "https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSOF5Lc20qbXu3kJqsT83yhiXegTKsQ6Qbi4bUaP50_jz_GbS3fh03ShHI"
    }
    if(beerGraphs$Brewery[i]=="Carlton & United Breweries (Fosters Group)"){
      beerGraphs$logoLink[i] <- "http://4.bp.blogspot.com/-JUcivdrVHbg/TjWx1kYzbtI/AAAAAAAAB1U/Ol-AvAta5o4/s500/Carlton+United+Brewers+logo+2011.png"
        }
    
    # Remove geocoded breweries that don't appear to be real (usually get some local address)
    if(beerGraphs$Brewery[i] %in% c("D&J Brewery","Bierhalle","Premium","Detroit Beer Company","Bad Attitude Craft Beer",
                                    "Professor Fritz Briem","Burgbrauerei Hessberg","Bayern Bräu","Beer Cat",
                                    "IKEA","Pepsico","Pepsi Cola","Coca-Cola","Zima","AMBEV","Millers Brewery",
                                    "Growler Brewery")){
      beerGraphs$FinalLocationFull[i] <- NA
      beerGraphs$lat[i] <- NA
      beerGraphs$lon[i] <- NA
    }
    
  }

}

# Jitter slightly so breweries in same town not exactly on top of each other
beerGraphs$lat <- jitter(beerGraphs$lat, factor = 0.5)
beerGraphs$lon <- jitter(beerGraphs$lon, factor = 0.5)


beerGraphs$AvgBARPtSize <- 3 + (beerGraphs$AvgBAR/9.1 * 20)
beerGraphs$AvgBARPtSize[beerGraphs$AvgBARPtSize < 3] <- 3

beerGraphs$BAR25PtSize <-   (log(beerGraphs$BAR25 ) * 2.5)
beerGraphs$BAR25PtSize[beerGraphs$BAR25PtSize < 3] <- 3
beerGraphs$BAR25PtSize[is.na(beerGraphs$BAR25PtSize) ] <- 3

beerGraphs$BAR25PtSize[ is.na(beerGraphs$BAR25)]<- NA

beerGraphs$TotalBARPtSize <-  (log(beerGraphs$TotalBAR)*2.5)
beerGraphs$TotalBARPtSize[beerGraphs$TotalBARPtSize < 3] <- 3
beerGraphs$TotalBARPtSize[is.na(beerGraphs$TotalBARPtSize) ] <- 3

# For each brewery find top 5 beers (name, style, BAR)
beerGraphs$beer1 <- beerGraphs$style1 <-beerGraphs$abv1 <- beerGraphs$bar1 <- NULL
beerGraphs$beer2 <- beerGraphs$style2 <-beerGraphs$abv2 <- beerGraphs$bar2 <- NULL
beerGraphs$beer3 <- beerGraphs$style3 <-beerGraphs$abv3 <- beerGraphs$bar3 <- NULL
beerGraphs$beer4 <- beerGraphs$style4 <-beerGraphs$abv4 <- beerGraphs$bar4 <- NULL
beerGraphs$beer5 <- beerGraphs$style5 <-beerGraphs$abv5 <- beerGraphs$bar5 <- NULL

beerGraphs$HTMLbeer1 <- beerGraphs$HTMLabv1 <- beerGraphs$HTMLbar1 <- NULL
beerGraphs$HTMLbeer2 <- beerGraphs$HTMLabv2 <- beerGraphs$HTMLbar2 <- NULL
beerGraphs$HTMLbeer3 <- beerGraphs$HTMLabv3 <- beerGraphs$HTMLbar3 <- NULL
beerGraphs$HTMLbeer4 <- beerGraphs$HTMLabv4 <- beerGraphs$HTMLbar4 <- NULL
beerGraphs$HTMLbeer5 <- beerGraphs$HTMLabv5 <- beerGraphs$HTMLbar5 <- NULL

beerGraphs$NumBeers <- NA

for(i in 1:nrow(beerGraphs)){ #nrow(beerGraphs)
  
  # Subset to country and order by decreasing career WAR
  temp3 <- subset(beerGraphsBeers, Brewery==beerGraphs$Brewery[i])
  temp3 <- temp3[ order(temp3$BAR, decreasing=T), ]
  
  beerGraphs$beer1[i] <- temp3$Beer[1]
  beerGraphs$style1[i] <- temp3$Style[1]
  beerGraphs$abv1[i] <- temp3$ABV[1]
  beerGraphs$bar1[i] <- temp3$BAR[1]
  
  beerGraphs$beer2[i] <- temp3$Beer[2]
  beerGraphs$style2[i] <- temp3$Style[2]
  beerGraphs$abv2[i] <- temp3$ABV[2]
  beerGraphs$bar2[i] <- temp3$BAR[2]
  
  beerGraphs$beer3[i] <- temp3$Beer[3]
  beerGraphs$style3[i] <- temp3$Style[3]
  beerGraphs$abv3[i] <- temp3$ABV[3]
  beerGraphs$bar3[i] <- temp3$BAR[3]
  
  beerGraphs$beer4[i] <- temp3$Beer[4]
  beerGraphs$style4[i] <- temp3$Style[4]
  beerGraphs$abv4[i] <- temp3$ABV[4]
  beerGraphs$bar4[i] <- temp3$BAR[4]
  
  beerGraphs$beer5[i] <- temp3$Beer[5]
  beerGraphs$style5[i] <- temp3$Style[5]
  beerGraphs$abv5[i] <- temp3$ABV[5]
  beerGraphs$bar5[i] <- temp3$BAR[5]
  
  
  
}


beerGraphs$styleColor1 <- NA
beerGraphs$styleColor1[grepl("Ale", beerGraphs$style1)] <- "Ale"
beerGraphs$styleColor1[grepl("IPA", beerGraphs$style1)] <- "IPA"
beerGraphs$styleColor1[grepl("Belgian", beerGraphs$style1)] <- "Belgian"
beerGraphs$styleColor1[grepl("Lambic", beerGraphs$style1)] <- "Belgian"
beerGraphs$styleColor1[grepl("Lager", beerGraphs$style1)] <- "Lager"
beerGraphs$styleColor1[grepl("Porter", beerGraphs$style1)] <- "Porter"
beerGraphs$styleColor1[grepl("Wheat", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("Witbier", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("Weisse", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("Weissbier", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("weizen", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("Weizen", beerGraphs$style1)] <- "Wheat"
beerGraphs$styleColor1[grepl("Stout", beerGraphs$style1)] <- "Stout"
beerGraphs$styleColor1[grepl("Cider", beerGraphs$style1)] <- "Cider"
beerGraphs$styleColor1[grepl("Pilsner", beerGraphs$style1)] <- "Pilsner"
beerGraphs$styleColor1[grepl("Pilsener", beerGraphs$style1)] <- "Pilsner"
beerGraphs$styleColor1[grepl("Fruit", beerGraphs$style1)] <- "Fruit"
beerGraphs$styleColor1[is.na(beerGraphs$styleColor1)] <- "Other"

beerGraphs$styleColor2 <- NA
beerGraphs$styleColor2[grepl("Ale", beerGraphs$style2)] <- "Ale"
beerGraphs$styleColor2[grepl("IPA", beerGraphs$style2)] <- "IPA"
beerGraphs$styleColor2[grepl("Belgian", beerGraphs$style2)] <- "Belgian"
beerGraphs$styleColor2[grepl("Lambic", beerGraphs$style2)] <- "Belgian"
beerGraphs$styleColor2[grepl("Lager", beerGraphs$style2)] <- "Lager"
beerGraphs$styleColor2[grepl("Porter", beerGraphs$style2)] <- "Porter"
beerGraphs$styleColor2[grepl("Wheat", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("Witbier", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("Weisse", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("Weissbier", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("weizen", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("Weizen", beerGraphs$style2)] <- "Wheat"
beerGraphs$styleColor2[grepl("Stout", beerGraphs$style2)] <- "Stout"
beerGraphs$styleColor2[grepl("Cider", beerGraphs$style2)] <- "Cider"
beerGraphs$styleColor2[grepl("Pilsner", beerGraphs$style2)] <- "Pilsner"
beerGraphs$styleColor2[grepl("Pilsener", beerGraphs$style2)] <- "Pilsner"
beerGraphs$styleColor2[grepl("Fruit", beerGraphs$style2)] <- "Fruit"
beerGraphs$styleColor2[is.na(beerGraphs$styleColor2)] <- "Other"

beerGraphs$styleColor3 <- NA
beerGraphs$styleColor3[grepl("Ale", beerGraphs$style3)] <- "Ale"
beerGraphs$styleColor3[grepl("IPA", beerGraphs$style3)] <- "IPA"
beerGraphs$styleColor3[grepl("Belgian", beerGraphs$style3)] <- "Belgian"
beerGraphs$styleColor3[grepl("Lambic", beerGraphs$style3)] <- "Belgian"
beerGraphs$styleColor3[grepl("Lager", beerGraphs$style3)] <- "Lager"
beerGraphs$styleColor3[grepl("Porter", beerGraphs$style3)] <- "Porter"
beerGraphs$styleColor3[grepl("Wheat", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("Witbier", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("Weisse", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("weizen", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("Weizen", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("Weissbier", beerGraphs$style3)] <- "Wheat"
beerGraphs$styleColor3[grepl("Stout", beerGraphs$style3)] <- "Stout"
beerGraphs$styleColor3[grepl("Cider", beerGraphs$style3)] <- "Cider"
beerGraphs$styleColor3[grepl("Pilsner", beerGraphs$style3)] <- "Pilsner"
beerGraphs$styleColor3[grepl("Pilsener", beerGraphs$style3)] <- "Pilsner"
beerGraphs$styleColor3[grepl("Fruit", beerGraphs$style3)] <- "Fruit"
beerGraphs$styleColor3[is.na(beerGraphs$styleColor3)] <- "Other"

beerGraphs$styleColor4 <- NA
beerGraphs$styleColor4[grepl("Ale", beerGraphs$style4)] <- "Ale"
beerGraphs$styleColor4[grepl("IPA", beerGraphs$style4)] <- "IPA"
beerGraphs$styleColor4[grepl("Belgian", beerGraphs$style4)] <- "Belgian"
beerGraphs$styleColor4[grepl("Lambic", beerGraphs$style4)] <- "Belgian"
beerGraphs$styleColor4[grepl("Lager", beerGraphs$style4)] <- "Lager"
beerGraphs$styleColor4[grepl("Porter", beerGraphs$style4)] <- "Porter"
beerGraphs$styleColor4[grepl("Wheat", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("Witbier", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("Weisse", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("Weissbier", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("weizen", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("Weizen", beerGraphs$style4)] <- "Wheat"
beerGraphs$styleColor4[grepl("Stout", beerGraphs$style4)] <- "Stout"
beerGraphs$styleColor4[grepl("Cider", beerGraphs$style4)] <- "Cider"
beerGraphs$styleColor4[grepl("Pilsner", beerGraphs$style4)] <- "Pilsner"
beerGraphs$styleColor4[grepl("Pilsener", beerGraphs$style4)] <- "Pilsner"
beerGraphs$styleColor4[grepl("Fruit", beerGraphs$style4)] <- "Fruit"
beerGraphs$styleColor4[is.na(beerGraphs$styleColor4)] <- "Other"

beerGraphs$styleColor5 <- NA
beerGraphs$styleColor5[grepl("Ale", beerGraphs$style5)] <- "Ale"
beerGraphs$styleColor5[grepl("IPA", beerGraphs$style5)] <- "IPA"
beerGraphs$styleColor5[grepl("Belgian", beerGraphs$style5)] <- "Belgian"
beerGraphs$styleColor5[grepl("Lambic", beerGraphs$style5)] <- "Belgian"
beerGraphs$styleColor5[grepl("Lager", beerGraphs$style5)] <- "Lager"
beerGraphs$styleColor5[grepl("Porter", beerGraphs$style5)] <- "Porter"
beerGraphs$styleColor5[grepl("Wheat", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("Witbier", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("Weisse", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("Weissbier", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("weizen", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("Weizen", beerGraphs$style5)] <- "Wheat"
beerGraphs$styleColor5[grepl("Stout", beerGraphs$style5)] <- "Stout"
beerGraphs$styleColor5[grepl("Cider", beerGraphs$style5)] <- "Cider"
beerGraphs$styleColor5[grepl("Pilsner", beerGraphs$style5)] <- "Pilsner"
beerGraphs$styleColor5[grepl("Pilsener", beerGraphs$style5)] <- "Pilsner"
beerGraphs$styleColor5[grepl("Fruit", beerGraphs$style5)] <- "Fruit"
beerGraphs$styleColor5[is.na(beerGraphs$styleColor5)] <- "Other"

# Define vector of styles
styles  <- c("Lager","IPA","Porter","Wheat","Ale","Stout","Cider","Pilsner","Belgian","Fruit","Other")
mycolors <- c()

# Define color for each beer style
for(i in 1:length(styles)){
  mycolors[i] <- switch(styles[i],
                        "Lager"= "#cc9900",
                        "IPA" = "#009933",
                        "Porter" = "#734d26",
                        "Wheat" = "#ff9900",
                        "Ale" = "#cccc00",
                        "Stout" = "#000000",
                        "Cider" = "#ff0000",
                        "Belgian" = "#9933ff",
                        "Pilsner" = "#00ccff", 
                        "Fruit" = "#ff4dd2",
                        "Other" = "#0000ff")
}

beerGraphs$color1 <- beerGraphs$color2 <-beerGraphs$color3 <-beerGraphs$color4 <-beerGraphs$color5 <-NA
# Define color for each beer style
for(i in 1:nrow(beerGraphs)){
  # Define color
  beerGraphs$color1[i] <- mycolors[grepl(beerGraphs$styleColor1[i], styles)]
  beerGraphs$color2[i] <- mycolors[grepl(beerGraphs$styleColor2[i], styles)]
  beerGraphs$color3[i] <- mycolors[grepl(beerGraphs$styleColor3[i], styles)]
  beerGraphs$color4[i] <- mycolors[grepl(beerGraphs$styleColor4[i], styles)]
  beerGraphs$color5[i] <- mycolors[grepl(beerGraphs$styleColor5[i], styles)]
  
  if(!is.na(beerGraphs$beer1[i])){
    if(is.na(beerGraphs$beer2[i])){
      beerGraphs$NumBeers[i] <- 1
    }else if(is.na(beerGraphs$beer3[i])){
      beerGraphs$NumBeers[i] <- 2
    }else if(is.na(beerGraphs$beer4[i])){
      beerGraphs$NumBeers[i] <- 3
    }else if(is.na(beerGraphs$beer5[i])){
      beerGraphs$NumBeers[i] <- 4
    }else
      beerGraphs$NumBeers[i] <- 5
  }else {
    beerGraphs$NumBeers[i] <- 0
  }
  
  
  if(beerGraphs$NumBeers[i]==5){
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
             beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
     # Values
       paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
     # Plotted Variable/Value
     paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
           beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>"), 
      
      # http://beergraphs.com/leaderboards/?brewery=46
      paste("<div style = 'text-align:left;margin-top: 0px;margin-bottom:0px; padding:0;font-size:10px'><p style = 'padding:0;margin-top: 3px;margin-bottom:0px'><a href='", 
            paste("http://beergraphs.com",beerGraphs$linkBG[i],sep=""), "' style = 'color: black;' target = '_blank'><span style = 'padding:0;font-weight:bold;text-decoration:underline'>Top Beers :</span> ", 
            "</a></p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>1. ",beerGraphs$beer1[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color1[i],"'>",beerGraphs$style1[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv1[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar1[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>2. ",beerGraphs$beer2[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color2[i],"'>",beerGraphs$style2[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv2[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar2[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>3. ",beerGraphs$beer3[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color3[i],"'>",beerGraphs$style3[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv3[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar3[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>4. ",beerGraphs$beer4[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color4[i],"'>",beerGraphs$style4[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv4[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar4[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>5. ",beerGraphs$beer5[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color5[i],"'>",beerGraphs$style5[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv5[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar5[i],
            "</div></p>")
    )
  }else if(beerGraphs$NumBeers[i]==4){
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
      # Values
      paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
      # Plotted Variable/Value
      paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
            beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>"), 
      
      # http://beergraphs.com/leaderboards/?brewery=46
      paste("<div style = 'text-align:left;margin-top: 0px;margin-bottom:0px; padding:0;font-size:10px'><p style = 'padding:0;margin-top: 3px;margin-bottom:0px'><a href='", 
            paste("http://beergraphs.com",beerGraphs$linkBG[i],sep=""), "' style = 'color: black;' target = '_blank'><span style = 'padding:0;font-weight:bold;text-decoration:underline'>Top Beers :</span> ", 
            "</a></p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>1. ",beerGraphs$beer1[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color1[i],"'>",beerGraphs$style1[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv1[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar1[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>2. ",beerGraphs$beer2[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color2[i],"'>",beerGraphs$style2[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv2[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar2[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>3. ",beerGraphs$beer3[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color3[i],"'>",beerGraphs$style3[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv3[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar3[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>4. ",beerGraphs$beer4[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color4[i],"'>",beerGraphs$style4[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv4[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar4[i],
            "</p>")
    )
  }else if(beerGraphs$NumBeers[i]==3){
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
      # Values
      paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
      # Plotted Variable/Value
      paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
            beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>"), 
      
      # http://beergraphs.com/leaderboards/?brewery=46
      paste("<div style = 'text-align:left;margin-top: 0px;margin-bottom:0px; padding:0;font-size:10px'><p style = 'padding:0;margin-top: 3px;margin-bottom:0px'><a href='", 
            paste("http://beergraphs.com",beerGraphs$linkBG[i],sep=""), "' style = 'color: black;' target = '_blank'><span style = 'padding:0;font-weight:bold;text-decoration:underline'>Top Beers :</span> ", 
            "</a></p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>1. ",beerGraphs$beer1[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color1[i],"'>",beerGraphs$style1[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv1[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar1[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>2. ",beerGraphs$beer2[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color2[i],"'>",beerGraphs$style2[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv2[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar2[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>3. ",beerGraphs$beer3[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color3[i],"'>",beerGraphs$style3[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv3[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar3[i],
            "</p>")
    )
  }else if(beerGraphs$NumBeers[i]==2){
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
      # Values
      paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
      # Plotted Variable/Value
      paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
            beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>"), 
      
      # http://beergraphs.com/leaderboards/?brewery=46
      paste("<div style = 'text-align:left;margin-top: 0px;margin-bottom:0px; padding:0;font-size:10px'><p style = 'padding:0;margin-top: 3px;margin-bottom:0px'><a href='", 
            paste("http://beergraphs.com",beerGraphs$linkBG[i],sep=""), "' style = 'color: black;' target = '_blank'><span style = 'padding:0;font-weight:bold;text-decoration:underline'>Top Beers :</span> ", 
            "</a></p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>1. ",beerGraphs$beer1[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color1[i],"'>",beerGraphs$style1[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv1[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar1[i],
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>2. ",beerGraphs$beer2[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color2[i],"'>",beerGraphs$style2[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv2[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar2[i],
            "</p>")
    )
  }else if(beerGraphs$NumBeers[i]==1){
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
      # Values
      paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
      # Plotted Variable/Value
      paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
            beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>"), 
      
      # http://beergraphs.com/leaderboards/?brewery=46
      paste("<div style = 'text-align:left;margin-top: 0px;margin-bottom:0px; padding:0;font-size:10px'><p style = 'padding:0;margin-top: 3px;margin-bottom:0px'><a href='", 
            paste("http://beergraphs.com",beerGraphs$linkBG[i],sep=""), "' style = 'color: black;' target = '_blank'><span style = 'padding:0;font-weight:bold;text-decoration:underline'>Top Beers :</span> ", 
            "</a></p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;'><span style = 'font-weight:bold'>1. ",beerGraphs$beer1[i],"</span>", 
            "</p>"),
      paste("<p style = 'margin-top: 0px;margin-bottom:0px;text-indent:13px'><span style = 'color:",beerGraphs$color1[i],"'>",beerGraphs$style1[i], "</span><span style = 'font-weight:bold'></span>ABV : ", beerGraphs$abv1[i],
            "<span style = 'font-weight:bold'></span>BAR : ",beerGraphs$bar1[i],
            "</p>")
    )
  }else {
    beerGraphs$HTMLtextTOT[i] <- paste(
      paste("<div style = 'text-align:center;margin:0'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><img src='",
            beerGraphs$logoLink[i],"' width = '100' /></a>"),
      paste("<div style = 'text-align:center;margin-top: 5px;margin-bottom:0px'><a href='", 
            beerGraphs$link[i], "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0;margin:0'>",
            beerGraphs$Brewery[i],"</div></a></h5>"),
      paste("<div style = 'text-align:center;margin:0'><p style = 'padding:0;margin:0'>", beerGraphs$FinalLocationFull[i],"</p>"),
      #Wrapper div
      paste( "<div style = 'width:250px;overflow:hidden'> "),
      # Labels
      paste("<div style = 'text-align:left;margin: 0;font-size:10px;width:80px;float:left'><p style = 'padding:0;margin: 0;'>",
            "<span style = 'padding:0;font-weight:bold;'>Beers :<br/>Avg. ABV :<br/>Avg. BAR :<br/>BAR Top 25% :</span> </div>"), 
      # Values
      paste("<div style = 'text-align:right;margin: 0;font-size:10px;width:50px;float:left;border-right: 1px solid gray'><p style = 'padding:0;margin: 0;position:relative;right:10px'>",
            beerGraphs$Beers[i],"<br/>",beerGraphs$AvgABV[i],"<br/>",beerGraphs$AvgBAR[i],"<br/>",beerGraphs$BAR25[i],"</p> </div>"), 
      # Plotted Variable/Value
      paste("<div style = 'text-align:center;margin: 0;width:120px;float:left;'><p style = 'padding:0;margin: 0;font-size:14px;position:relative;top:10px;'>",
            beerGraphs$TotalBAR[i],"<br/><span style = 'padding:0;font-weight:bold;'>Total BAR","</span></p> </div></div>")
      
    )
  }     
}

# Make colors an ordered factor by our vector list
beerGraphs$color1 <- factor(beerGraphs$color1)

saveRDS(beerGraphs, "data/BreweryMap.rds")
saveRDS(beerGraphs, "Brewery-Map/data/BreweryMap.rds")



