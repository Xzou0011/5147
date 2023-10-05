library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(ggplot2)
library(scales)

## set relative path
setwd("./")
getwd()

## read in csv data about Victoria criminal data by suburb
vicCriminalData <- read.csv("./vicDataCriminalDataBySuburb.csv")

## read in csv data about Victoria criminal data by police region
vicCriminalDataByPoliceReg <- read.csv("./vicDataCriminalDataByPoliceRegion.csv")


## read in csv data about Victoria rental price for median one-bed flat data and crime data in 2021 by suburb
vicRentalPriceWithCrimeData <- read.csv("./vicRentalPriceIndicatorData.csv")

## read in shape file of Victoria
victoria <- readOGR("./vic_localities.shp")

## the maximum number of yearly criminal records from 2012 to 2021
maxCriminalIncidentsByPoliceReg <- 
  vicCriminalDataByPoliceReg %>% 
  group_by(year) %>% 
  summarise(incidents = sum(incidents)) %>%
  top_n(n = 1)

## purple color used in bar chart
colors <- c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f")

## bar chart
allYearCriminalChanges <- vicCriminalDataByPoliceReg %>%
  ggplot(aes(x = year, y = incidents)) +
  geom_col(
    aes(fill = police.region)
  ) + 
  ylim(0, maxCriminalIncidentsByPoliceReg$incidents * 1.02) +
  theme(
    legend.box      = "horizontal",
    legend.key      = element_blank(),
    legend.title    = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_manual(values = colors) +
  theme_classic() +
  guides(fill = guide_legend(title="Police Region"))

## function to calculate the renter choice coefficient
calculateRenterChoiceCoefficient <- function(numberA, numberB, numberC){
  vicRentalPriceIndicatorData <- vicRentalPriceWithCrimeData %>%
    group_by(X1.Bed.Flat) %>%
    mutate(renter.choice = (
      X1.Bed.Flat * numberA + 
        B.Property.and.deception.offences/1000 * numberB + 
        A.Crimes.against.the.person/1000 * numberB + 
        (
          C.Drug.offences + 
            D.Public.order.and.security.offences + 
            E.Justice.procedures.offences + 
            F.Other.offences
          )/1000 * numberC
      )
    )
  top_5_Recommond_Place <- 
    head(arrange(vicRentalPriceIndicatorData,renter.choice), n = 5)
  
  table <- data.frame("Top5"=top_5_Recommond_Place$Local.Government.Area)
    
  return(table)
}

## create function to generate the Victoria criminal map in specific year
vicMaps <- function(year){
  
  victoria$LOC_NAME = toupper(victoria$LOC_NAME)
  
  df_sum <- vicCriminalData %>% 
    filter(Year == year) %>%
    filter(Suburb %in% victoria$LOC_NAME) %>%
    group_by(Suburb) %>%
    summarise(Incidents.Recorded = sum(Incidents.Recorded))
  victoria$Incidents.Recorded = 2851
  victoria$Incidents.Recorded <- df_sum$Incidents.Recorded[match(victoria$LOC_NAME, df_sum$Suburb)]
  
  bins <- c(1, 100, 101, 15000, 200000000)
  
  pal <- colorBin("Reds", domain = victoria$Incidents.Recorded , bins = bins, na.color = NA)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g cases",
    victoria$LOC_NAME, victoria$Incidents.Recorded
  ) %>% lapply(htmltools::HTML)
  
  m <- leaflet() %>%
    setView(144,-37,6.5) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = victoria,
      weight = 1,
      color = "white",
      fillOpacity = 0.8,
      fillColor = pal(victoria$Incidents.Recorded),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend("bottomright",
              pal = pal,
              values = victoria$Incidents.Recorded,
              title = "Incidents numbers",
              opacity = 1)
  return(m)
}

## generate pie chart indicate the proportion of crime types in specific year
vicPieChart <- function(year){
  vic_By_Year <- vicCriminalData %>%
    filter(Year == year) %>%
    group_by(Offence.Division) %>%
    summarise(Incidents.Recorded = sum(Incidents.Recorded)) %>%
    mutate(percent = Incidents.Recorded/sum(Incidents.Recorded))
  
  
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.title= element_text(size=24, face="bold")
    )
  
  pieChart <- ggplot(vic_By_Year, aes(x="", y=Incidents.Recorded, fill=Offence.Division))+
    geom_bar(width = 1, stat = "identity") + 
    coord_polar("y", start=0) +
    scale_fill_brewer("Blues") + 
    blank_theme +
    theme(axis.text.x=element_blank()) + 
    geom_text(aes(label = percent(round(percent, digits = 2),accuracy = 1)), 
              size=5, position = position_stack(vjust = 0.5))
  
  return(pieChart)
}

## define the ui
ui <- fluidPage(
  
  titlePanel(
    div(
      "Most Affordable and Safest Place To Live in Victoria", style = "font-size:65px;"
      )
    ),
  
  fluidPage(
    fluidRow(
      column(4, 
             selectInput("Year",
                         label = "Choose the year you want to see:",
                         choices = list("2012" = "2012",
                                        "2013" = "2013",
                                        "2014" = "2014",
                                        "2015" = "2015",
                                        "2016" = "2016",
                                        "2017" = "2017",
                                        "2018" = "2018",
                                        "2019" = "2019",
                                        "2020" = "2020",
                                        "2021" = "2021"), 
                         selected = "2015"),
             div("Crime Type Proportion in Selected Year", style = "font-size:25px;"),
             plotOutput('VIS2')
      ),
      column(8, 
             div("Victoria Criminal Map by Suburb in Selected Year", style = "font-size:25px;"),
             leafletOutput("distPlot")
      )
    ),
    fluidRow(
      column(
        4, 
        div("Criminal Incidents from 2012 to 2021", style = "font-size:25px;"),
        plotOutput('VIS1')
      ),
      column(
        5,
        div("Choose the importance of the following from 0-1 and the top 5 recommond rental suburbs will generate for you", 
            style = "font-size:25px;"),
        div("The build-in renter choice coefficient equation is the finding from the previous data exploration project, 
            the renter choice coefficient and the predicted satisfaction the user would be when moving into a one-bed flat is negative correlated.", 
            style = "font-size:15px;"),
        splitLayout(
          numericInput("numberA", label = "Importance of rental price", value = 0.5,
                       min = 0,
                       max = 1
          ),
          numericInput("numberB", label = "Crimes related", value = 0.5,
                       min = 0,
                       max = 1
          ),
          numericInput("numberC", label = "Other crimes", value = 0.5,
                       min = 0,
                       max = 1
          )
        )
      ),
      column(
        8,
        tableOutput('TABLE1'))
    ),
    fluidRow(
      'data source 1: https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data, 
      this data is victoria criminal data from 2012 to 2021;'
    ),
    fluidRow(
      'data source 2: https://www.dhhs.vic.gov.au/past-rental-reports,
      this data is victoria one bed flat median rental price by local government area;'
    ),
    fluidRow(
      'data source 3: https://data.gov.au/dataset/ds-dga-af33dd8c-0534-4e18-9245-fc64440f742e/details,
      this data is victoria shapefile by suburb'
    )
  )
)

# Define the server
server = function(input, output) {
  
  output$distPlot <- renderLeaflet(
    vicMaps(input$Year)
  )
  
  output$VIS1 <- renderPlot(
    allYearCriminalChanges
  )
  
  output$VIS2 <- renderPlot(
    vicPieChart(input$Year)
  )
  output$TABLE1 <- renderTable(
    calculateRenterChoiceCoefficient(input$numberA, input$numberB, input$numberC) 
  )
}

# run the app
shinyApp(ui, server)
