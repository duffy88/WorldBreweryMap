# Server for BeerGraphs Brewery info interactive map 
# 
# By Doug Duffy 2016

# Load Packages
library(shiny)
library(leaflet)

# Load prepared dataset
brewMap <- readRDS("data/BreweryMap.rds")

brewMap <- brewMap[!(is.na(brewMap$lat)), ]

# Define vector of styles
styles  <- c("Lager","IPA","Porter","Wheat","Ale","Stout","Cider","Pilsner","Belgian","Fruit","Other")
mycolors <- c()

# Define color for each team
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

#
# Shiny Server Function
#

server <- function(input, output, session) {
  
  
  # Build main body of leaflet map, basic tiles only, points added later
  output$map <- renderLeaflet({
    
    leaflet(brewMap) %>% addProviderTiles("CartoDB.Positron")
  })
  
  # Reactive to set point size based on user input
  inputSize <- reactive({
    if(input$size == "BAR Top 25%"){
      return("BAR25PtSize")
    }else if(input$size == "Avg. BAR"){
      return("AvgBARPtSize")
    }else if(input$size == "Total BAR"){
      return("TotalBARPtSize")
    }
  })
  
  # Reactive to set the variable for point size based on user input
  inputVar <- reactive({
    if(input$size == "BAR Top 25%"){
      return("BAR25")
    }else if(input$size == "Avg. BAR"){
      return("AvgBAR")
    }else if(input$size == "Total BAR"){
      return("TotalBAR")
    }
  })
  
  # Reactive to set min num beers slider  based on user input size
  inputMinBeers <- reactive({
    if(input$size == "BAR Top 25%"){
      return(13)
    }else if(input$size == "Avg. BAR"){
      return(1)
    }else if(input$size == "Total BAR"){
      return(1)
    }
  })
  
  
  # Dynamically render the slider bars based on filtered data # 2
  # Yearly WAR
  output$bar2 <- renderUI({
    sliderInput("bar", input$size,
                round(min(brewMap[, inputVar()], na.rm=T), 1),
                round(max(brewMap[, inputVar()], na.rm=T),1),
                value = c(min(brewMap[, inputVar()], na.rm=T),max(brewMap[, inputVar()], na.rm=T)), step =0.1
    )
  })
  
  # Yearly WAR
  output$minbeer2 <- renderUI({
    sliderInput("minbeer", "Min. Beers : ",
                round(min(brewMap[, "Beers"], na.rm=T), 0),
                round(max(brewMap[, "Beers"], na.rm=T),0),
                value = c(inputMinBeers(), max(brewMap[, "Beers"], na.rm=T)), step =1
    )
  })
  
  # First filtering of data to only desired year
  filteredData <- reactive({
    if(input$size == "BAR Top 25%"){
      brewMap <- brewMap[!(is.na(brewMap$BAR25)), ]
    }
    df <- brewMap[brewMap[, inputVar()] >= input$bar[1] &
                    brewMap[, inputVar()] <= input$bar[2] &
                    brewMap[, "Beers"] >= input$minbeer[1] &
                    brewMap[, "Beers"] <= input$minbeer[2],]
    return(df)
    
    
    
  })
  
  # Observe reset view button
  observe({
    input$resetview
    if(!is.null(filteredData())){
      leafletProxy("map", data = filteredData())  %>%
        fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    }
    
  })
  
  # Observe legend
  observe({
    if(!is.null(filteredData())){
      proxy <- leafletProxy("map", data = filteredData())
      
      # Remove any existing legend, and only if the legend is
      # enabled and multiple teams plotted, create a new one.
      proxy %>% clearControls()
      if (input$legend & length(mycolors[mycolors %in% filteredData()$color1])>1) {
        
        
        proxy %>% addLegend("bottomleft", 
                            colors= ~mycolors[mycolors %in% filteredData()$color1], 
                            labels= ~styles[styles %in% filteredData()$styleColor1], 
                            title="Top Beer Style",
                            opacity =1) 
      }
    }
    
  })
  
  # Dynamically  the point size legend
  output$ptlegend <- renderUI({
    absolutePanel(left = ifelse(input$legend == FALSE | length(mycolors[mycolors %in% filteredData()$color1])<2,25,150), top =390, width = 135, class = "floater", style="opacity:0.7",
                  div(style="margin:0",
                      div(h5(style = "text-align:center;margin:0",
                             strong(input$size)),
                          div(style="",
                              img(src = if(input$size == "BAR Top 25%"){
                                "BAR25PtSizeLegend.png"
                              }else if(input$size == "Avg. BAR"){
                                "AvgBARPtSizeLegend.png"
                              }else if(input$size == "Total BAR"){
                                "TotBARPtSizeLegend.png"
                              }, width = "124px", height= "68px"))
                          
                          
                      )    
                  ))
     
   
  })
  
  
  
  
  
  
  
  # Observe the addition and removal of points based on filtered data #3
  observe({
    
    data <- filteredData()[order(filteredData()[,names(filteredData())== inputSize()], decreasing = T),]
    size <- filteredData()[order(filteredData()[,names(filteredData())== inputSize()], decreasing = T),names(filteredData())== inputSize()]
    
    
    
    
    # Remove points if null
    if(is.null(data)  ){ 
      leafletProxy("map", data = data) %>% clearMarkers()
      # If not null remove points and add new points
    }else {
      
      map2 <- leafletProxy("map", data = data) %>%
        clearMarkers() 
      
      if(nrow(data)>0){
        map2 %>% addCircleMarkers(radius = ~size,  stroke = NA, color = ~color1,
                                  fillOpacity = ifelse(nrow(data)>50, 0.5, 0.85), layerId = ~Brewery ,
                                  
                                  # HTML to display on click
                                  popup = ~ HTMLtextTOT) %>%
          fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
      }
      
      
    }
    
  })
  
  
  # Add highlighting around point being clicked
  observe({
    click <- input$map_marker_click
    
    
    if(!is.null(click)  ){
      # Identify point clicked
      hl <- subset(filteredData()[order(filteredData()[,names(filteredData())== inputSize()], decreasing = T),], Brewery==click$id)
      # Remove previous highlighting
      lp <- leafletProxy("map", data = hl) %>% 
        removeMarker( layerId = "add") 
      size2 <- hl[,names(hl)== inputSize()]
      # Add new highlighting only if there is a point to add
      if(nrow(hl) >0){
        lp %>% addCircleMarkers(radius = ~size2,  stroke = TRUE, weight=4,color = ~color1,
                                fillOpacity = 0,opacity=1, layerId = "add")
      }
      
    }
    
  })
  
}
