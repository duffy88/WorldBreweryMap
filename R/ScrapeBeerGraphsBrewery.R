library(XML)
library(rvest)

# Progress bar
pb <- txtProgressBar(min = 1, max = 313, style = 3)

for(i in 1:313){ #  
  
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  url <- paste("http://beergraphs.com/leaderboards/breweries/?page=",i, sep="")
  
  table <- readHTMLTable(url, stringsAsFactors=FALSE)
  table <- table[[1]]
  
  table <- table[ ,-1]
  names(table) <- c("Brewery","Location","Beers","AvgABV","Solid","SolidPct","AvgBAR","BAR25","TotalBAR")
  
  
  pg <- read_html(url)
  
  linkBG <- html_attr(html_nodes(pg, ".name a"), "href")
  
  if(i==1) beerGraphs <- cbind(table, linkBG) else
    beerGraphs <- rbind(beerGraphs,cbind(table, linkBG))
  
}
beerGraphs$SolidPct <- gsub("%","", beerGraphs$SolidPct)


for(j in 3:9){
  beerGraphs[,j] <- as.numeric(beerGraphs[,j])
}
# Close progress bar
close(pb)


beerGraphs$Location <- gsub("\n","", beerGraphs$Location)
beerGraphs$Location <- gsub("                                                        "," ", beerGraphs$Location)

beerGraphs$loc <- strsplit(beerGraphs$Location,", ")
beerGraphs$place1 <- beerGraphs$place2 <- beerGraphs$place3 <- NA 
for(i in 1:nrow(beerGraphs)){
  if(length(beerGraphs$loc[[i]])==3 | length(beerGraphs$loc[[i]])==4){
    beerGraphs$place1[i] <- beerGraphs$loc[[i]][1]
    beerGraphs$place2[i] <- beerGraphs$loc[[i]][2]
    beerGraphs$place3[i] <- beerGraphs$loc[[i]][3]
    
  }
  if(length(beerGraphs$loc[[i]])==2){
    beerGraphs$place1[i] <- beerGraphs$loc[[i]][1]
    beerGraphs$place2[i] <- beerGraphs$loc[[i]][2]
    
  }
}
 
for(i in 1:nrow(beerGraphs)){
  if(length(beerGraphs$loc[[i]])==1 ){
    
    beerGraphs$place3[i] <- beerGraphs$loc[[i]][1]
    
  }
}
# Correct country abbreviations
for(i in 1:nrow(beerGraphs)){
  if(!is.na(beerGraphs$place3[i]) & nchar(beerGraphs$place3[i])==2){
    beerGraphs$place3[i] <- switch(beerGraphs$place3[i],
                                  "US"= "United States",
                                  "DK"= "Denmark",
                                  "BE" = "Belgium",
                                  "GB" = "United Kingdom",
                                  "NZ" = "New Zealand",
                                  "NO" = "Norway",
                                  "NL" = "Netherlands",
                                  "CA" = "Canada",
                                  "AU" = "Australia",
                                  "CH" = "Switzerland",
                                  "DE"= "Germany",
                                  "IT" = "Italy",
                                  "JP" = "Japan",
                                  "BR" = "Brazil",
                                  "FR" = "France",
                                  "AT" = "Austria",
                                  "RU"= "Russia",
                                  "IE" = "Ireland",
                                  "SE" = "Sweden",
                                  "HK" = "Hong Kong",
                                  "CN" = "China",
                                  "LU" = "Luxembourg",
                                  "CZ"= "Czechia",
                                  "PL" = "Poland",
                                  "ES" ="Spain",
                                  "ZA" = "South Africa",
                                  "PR" = "Puerto Rico",
                                  "PH" = "Philippines",
                                  "KY" = "Cayman Islands",
                                  "MX" = "Mexico",
                                  "MZ" = "Mozambique",
                                  "VA" = "Vatican City",
                                  "GR" = "Greece",
                                  "VE" = "Venezuela",
                                  "CR" = "Costa Rica",
                                  "IL" = "Israel",
                                  "BB" = "Barbados",
                                  "AM" = "Armenia",
                                  "JM" = "Jamaica", 
                                  "BA" = "Bosnia and Herzegovina",
                                  "PT" = "Portugal",
                                  "HR" = "Croatia",
                                  "XX" = "United States",
                                  "MU" = "Mauritius",
                                  "ZM" = "Zambia",
                                  "EG" = "Egypt",
                                  "NP" = "Nepal",
                                  "TH" = "Thailand",
                                  "LV" = "Latvia",
                                  "CY" = "Cyprus",
                                  "EE" ="Estonia",
                                  "AL" = "Albania",
                                  "SG" = "Singapore",
                                  "VN" = "Vietnam",
                                  "GT" = "Guatemala",
                                  "BO" = "Bolivia",
                                  "LT" = "Lithuania",
                                  "GU" = "Guam",
                                  "CO" = "Colombia",
                                  "VI" = "Virgin Islands",
                                  "TR" = "Turkey",
                                  "FI" = "Finland",
                                  "NA" = "Namibia",
                                  "AW" ="Aruba",
                                  "SI"= "Slovenia",
                                  "ZW" = "Zimbabwe",
                                  "SV" = "El Salvador",
                                  "BM" = "Bermuda",
                                  "PS" = "Palestine",
                                  "TW"="Taiwan",
                                  "AZ"="Azerbaijan"
                                  )
  }
}
# Correct place2 abbreviations
for(i in 1:nrow(beerGraphs)){
  if(!is.na(beerGraphs$place2[i]) & !is.na(beerGraphs$place3[i])){
    if(beerGraphs$place3[i]=="Canada" & beerGraphs$place2[i]=="NY"& beerGraphs$place1[i]=="Buffalo" ){
      beerGraphs$place3[i] <- "United States"
      beerGraphs$place2[i] <- "New York"
    }
    if(beerGraphs$place3[i]=="United States" & nchar(beerGraphs$place2[i])==2){
      beerGraphs$place2[i] <- switch(beerGraphs$place2[i],
                                     "CA"= "California",
                                     "FL"= "Florida",
                                     "MI" = "Michigan",
                                     "IL" = "Illinois",
                                     "VT" = "Vermont",
                                     "IN" = "Indiana",
                                     "DE" = "Delaware",
                                     "CO" = "Colorado",
                                     "OR" = "Oregon",
                                     "WI" = "Wisconsin",
                                     "PA" = "Pennsylvania",
                                     "MO" = "Missouri",
                                     "NC" = "North Carolina",
                                     "TX" = "Texas",
                                     "MN" = "Minnesota",
                                     "OH" = "Ohio",
                                     "NY" = "New York",
                                     "ME" = "Maine",
                                     "MA" = "Massachusetts",
                                     "IA" = "Iowa",
                                     "CT" = "Connecticut",
                                     "SC" = "South Carolina",
                                     "NJ" = "New Jersey",
                                     "GA" = "Georgia",
                                     "UT" = "Utah",
                                     "WA" = "Washington",
                                     "VA" = "Virginia",
                                     "AZ" = "Arizona",
                                     "AK" = "Alaska",
                                     "MD" = "Maryland",
                                     "NM" = "New Mexico",
                                     "NH" = "New Hampshire",
                                     "HI" = "Hawaii",
                                     "KY" = "Kentucky",
                                     "LA" = "Louisiana",
                                     "TN" = "Tennessee",
                                     "NE" = "Nebraska",
                                     "OK" = "Oklahoma",
                                     "KS" = "Kansas",
                                     "MT" = "Montana",
                                     "AL" ="Alabama",
                                     "RI" = "Rhode Island",
                                     "SD" = "South Dakota",
                                     "MS" = "Mississippi",
                                     "NV" = "Nevada",
                                     "ID" = "Idaho",
                                     "AR"= "Arkansas",
                                     "WY" = "Wyoming",
                                     "WV" = "West Virginia",
                                     "ND" = "North Dakota",
                                     "DC" = "District of Columbia"
                                     
      )
    }
    if(beerGraphs$place3[i]=="Canada"){
      if( nchar(beerGraphs$place2[i])==2){
        beerGraphs$place2[i] <- switch(beerGraphs$place2[i],
                                       "BC"= "British Columbia",
                                       "Bc"= "British Columbia",
                                       "QC" = "Quebec",
                                       "Qc" = "Quebec",
                                       "ON" = "Ontario",
                                       "MB" = "Manitoba",
                                       "NB" = "New Brunswick",
                                       "SK" = "Saskatchewan",
                                       "PQ" = "Quebec",
                                       "AB" = "Alberta",
                                       "NS" = "Nova Scotia",
                                       "NL" = "Newfoundland and Labrador",
                                       "PE" = "Prince Edward Island",
                                       "YT" = "Yukon"
                                       
                                       
                                       
        )
      }
      
    }
  }
}


beerGraphs$LocationFull <- NA
for(i in 1:nrow(beerGraphs)){
  if(!is.na(beerGraphs$place1[i]) & !is.na(beerGraphs$place2[i]) & !is.na(beerGraphs$place3[i])){
    beerGraphs$LocationFull[i] <- paste(beerGraphs$place1[i], beerGraphs$place2[i], beerGraphs$place3[i], sep= ", ")
    
  }else if(!is.na(beerGraphs$place1[i]) & !is.na(beerGraphs$place2[i])){
    beerGraphs$LocationFull[i] <- paste(beerGraphs$place1[i], beerGraphs$place2[i],  sep= ", ")
  }else if(!is.na(beerGraphs$place3[i]) ){
    beerGraphs$LocationFull[i] <- beerGraphs$place3[i]
  }
}

# Make ID for breweries
beerGraphs$linkBG <- as.character(beerGraphs$linkBG)
beerGraphs$BrewID <- gsub("[[:alpha:]]", "", beerGraphs$linkBG)
beerGraphs$BrewID <- substr(beerGraphs$BrewID ,5,nchar(beerGraphs$BrewID ))
beerGraphs$BrewID <- as.integer(beerGraphs$BrewID)


saveRDS(beerGraphs, "data/BeerGraphsBreweries.rds" )
