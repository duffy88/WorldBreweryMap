# UI for MLB Birthplace info interactive map 
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(leaflet)



#
# Shiny UI function
#

shinyUI(fluidPage(
  # Add a little CSS to modify floater aethetics
  tags$head(tags$style("
    .floater { background-color: rgb(255,255,255); padding: 8px; 
 border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }

")),
  
  # Leaflet Map output
  leafletOutput("map", width =1000, height = 500),
  
  # BAR Slider Inputs
  absolutePanel(top = 10, left = 810, width = 200,
                uiOutput("bar2"),uiOutput("minbeer2")),
  
  # Header, Logo and References
  absolutePanel(top = 432, left = 815, width = 200,
                div(style="background:white; opacity:0.7",
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px")),
                    # Div for main styling
                    
                    div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:5px", 
                        "Source :", 
                        a( href = "http://beergraphs.com/",
                           style ="text-decoration:none",target = "_blank",
                           "BeerGraphs"))
                )
  ),
  
  
  
  # Select Inputs for year and dynamically rendered team list
  absolutePanel(top=200, left =865, width=150,
                selectInput("size", "Point Size :", 
                            c("Avg. BAR","BAR Top 25%","Total BAR"),
                            selected = "Total BAR"
                )
  ),
  
  # Show team legend and reset view panel
  absolutePanel(top = 260, left = 910, width = 125,
                checkboxInput("legend", "Show Styles", TRUE),
                actionButton("resetview", "Reset View")
  ),
  
  uiOutput("ptlegend")
  
  
  
  
  
  ))





