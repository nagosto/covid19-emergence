library(lubridate)
library(plotly)
library(RColorBrewer)
library(shiny)
library(tidyverse)
library(viridis)

# Load data
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                      check.names = FALSE, stringsAsFactors = FALSE)
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", 
                      check.names = FALSE, stringsAsFactors = FALSE)
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)
confirmed_us <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", 
                         check.names = FALSE, stringsAsFactors = FALSE) # US Covid19 data
deaths_us <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", 
                      check.names = FALSE, stringsAsFactors = FALSE)

world_pop <- read.csv("pop_per_country.csv", check.names = FALSE, 
                      stringsAsFactors = FALSE) # world population data
world_pop <- rename(world_pop, COUNTRY = "name")
world_pop <- select(world_pop, COUNTRY, Density)
world_pop[world_pop$COUNTRY == "DR Congo", ]$COUNTRY = "Congo, Democratic Republic of the"
world_pop[world_pop$COUNTRY == "Republic of the Congo", ]$COUNTRY = "Congo, Republic of the"
df <- read.csv(
  'https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv') # country codes

# Cleaning, prepping---------------------------------------------------------------------------

country_codes <- select(df,-GDP..BILLIONS.)
confirmed <- rename(confirmed, COUNTRY = "Country/Region")
confirmed_us <- rename(confirmed_us, COUNTRY = "Country_Region", Long = "Long_", 
                       CODE = "iso3")
deaths <- rename(deaths, COUNTRY = "Country/Region")
deaths_us <- rename(deaths_us, COUNTRY = "Country_Region", Long = "Long_", CODE = "iso3")
recovered <- rename(recovered, COUNTRY = "Country/Region")

confirmed[confirmed$COUNTRY == "US", ]$COUNTRY = "United States" # rename US 
confirmed_us[which(confirmed_us$`Province_State` == "Puerto Rico"), ]$COUNTRY = "Puerto Rico"
deaths_us[which(deaths_us$`Province_State` == "Puerto Rico"), ]$COUNTRY = "Puerto Rico"

confirmed <- confirmed[, -1] # recode country names 
confirmed <- confirmed %>% group_by(COUNTRY) %>% summarise_all(sum)
recovered <- recovered[, -1]
recovered <- recovered %>% group_by(COUNTRY) %>% summarise_all(sum)
deaths <- deaths[, -1]
deaths <- deaths %>% group_by(COUNTRY) %>% summarise_all(sum)

confirmed_us <- confirmed_us[,-c(1:2,4:6)] # eliminate unwanted variables 
deaths_us <- deaths_us[,-c(1:2,4:6)]

prdf <- filter(confirmed_us, Province_State=="Puerto Rico")
dates <- prdf[,7:NCOL(prdf)]
dates_tot <- apply(dates,2,sum)
prdf <- cbind(prdf[1,1:5],t(dates_tot))
prdf <- prdf[,-c(1:2)]
confirmed <- rbind(confirmed, prdf)
deaths_pr <- filter(deaths_us, Province_State=="Puerto Rico")
deaths_pr <- deaths_pr[73,]
deaths_pr <- deaths_pr[,-c(1:2,6:7)]
deaths <- rbind(deaths, deaths_pr)

state_codes <- data.frame(Province_State = state.name, Abb = state.abb, 
                          stringsAsFactors = FALSE) # recode US states
deaths[which(deaths$COUNTRY=="US"),1]="United States"
confirmed_us <- confirmed_us %>% group_by(Province_State) %>% 
  summarise_if(., is.numeric, funs(sum))
confirmed_us <- right_join(state_codes, confirmed_us, by = "Province_State")
deaths_us <- deaths_us %>% group_by(Province_State) %>% 
  summarise_if(., is.numeric, funs(sum))
deaths_us <- right_join(state_codes, deaths_us, by = "Province_State")

confirmed_countrycodes <- left_join(country_codes, confirmed, by = "COUNTRY")
confirmed_countrycodes <- left_join(world_pop, confirmed_countrycodes, by = "COUNTRY")
recovered_countrycodes <- left_join(country_codes, recovered, by = "COUNTRY")
recovered_countrycodes <- left_join(world_pop, recovered_countrycodes, by = "COUNTRY")
deaths_countrycodes <- left_join(country_codes, deaths, by = "COUNTRY")
deaths_countrycodes <- left_join(world_pop, deaths_countrycodes, by = "COUNTRY")

colors <- c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu",
            "PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")

# UI----------------------------------------------------------------------------
ui <- 
  fluidPage(
    titlePanel("Covid19 Cases"),
    fluidRow(
      tabsetPanel(
        tabPanel("World Map", 
                 fluidRow(align = "center", 
                          selectInput("view",
                                      "Select data",
                                      choices = c("Confirmed","Recovered","Deaths"),
                                      selected = "Confirmed",
                                      multiple = FALSE)),
                 fluidRow(align = "center", 
                          selectInput(
                            "col",
                            "Color palette",
                            choices = colors,
                            selected = "Blues",
                            multiple = FALSE)),
                 fluidRow(align= "center", uiOutput("slider")),
                 fluidRow(align = "center"),
                 fluidRow(align="center", plotlyOutput("world1")),
                 plotlyOutput("usa")),
        tabPanel("Selected Countries",
                 fluidRow(align = "center", 
                          selectInput("view2",
                                      "Select data",
                                      choices = c("Confirmed", "Recovered", "Deaths"),
                                      selected = "Confirmed",
                                      multiple = FALSE)),
                 fluidRow(align = "center",
                          checkboxInput("density",
                                        paste("Divided by population density"),
                                        value = FALSE)),
                 fluidRow(align = "center",
                          checkboxInput("scale",
                                        paste("Log scale"),
                                        value = FALSE)),
                 fluidRow(align= "center",
                          selectInput("country",
                                      "Select country",
                                      choices= df$COUNTRY,
                                      selected = sample(df$COUNTRY,1),
                                      multiple = TRUE)),
                 fluidRow(align= "center", uiOutput("slider2")),
                 fluidRow(align="center", plotlyOutput("world2")),
                 fluidRow(align="center", plotlyOutput("scatterp"))
        )
      )
    )
  )

# Server-----------------------------
server <- function(input, output) {
  
  # First tab
  output$slider <- renderUI({ # to select date in data
    sliderInput("date",
                "Dates:",
                min =  mdy(names(confirmed_countrycodes)[6]),
                max = mdy(names(confirmed_countrycodes)[NCOL(confirmed_countrycodes)]),
                value = mdy(names(confirmed_countrycodes)[6]),
                timeFormat="%m/%d/%Y",
                animate = animationOptions())
  })
  
  output$usa <- renderPlotly({   # US map and its daily cases in selected dataset
    req(input$date)
    if (!is.null(input$view) & input$view == "Confirmed") {
      mapdata <- confirmed_us
    } else if (input$view == "Recovered") {
      cat("No available recovered data")
    } else {
      mapdata <- deaths_us
    }
    date <- gsub('\\b0+','',format(input$date,'%m/%d/%20'))
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    mapdata[,6:NCOL(mapdata)] %>% replace(., is.na(.), 0) -> mapdata[,6:NCOL(mapdata)]
    usmap <- plot_geo(mapdata,locationmode = 'USA-states')
    usmap <- usmap %>% add_trace(
      z = ~ mapdata[, which(names(mapdata) == date)],
      color = ~ mapdata[, which(names(mapdata) == date)],
      reversescale = TRUE,
      text= ~Province_State,
      locations = ~Abb
    )
    usmap <- usmap %>% colorbar(title = input$view)
    usmap <- usmap %>% layout(geo = g)
    usmap
  })
  
  output$world1 <- renderPlotly({   # World map with daily cases 
    req(input$view)
    date <- gsub('\\b0+','',format(input$date,'%m/%d/%20'))
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    if(!is.null(input$view) & input$view == "Confirmed"){
      mapdata <- confirmed_countrycodes
    } else if(input$view == "Recovered"){
      mapdata <- recovered_countrycodes}
    else {mapdata <- deaths_countrycodes}
    
    mapdata[,6:NCOL(mapdata)] %>% replace(., is.na(.), 0) -> mapdata[,6:NCOL(mapdata)]
    mapworld1 <- plot_geo(mapdata)
    mapworld1 <- mapworld1 %>% add_trace(
      z = ~ mapdata[, which(names(mapdata) == date)],
      colors = input$col,
      text = ~ COUNTRY,
      locations = ~ CODE,
      marker = list(line = l)
    )
    mapworld1 <- mapworld1 %>% colorbar(title = input$view)
    mapworld1 <- mapworld1 %>% layout(geo = g)
    mapworld1
  })
  
  # Second tab
  output$slider2 <- renderUI({
    sliderInput("date2",
                "Dates:",
                min =  mdy(names(confirmed_countrycodes)[6]),
                max = mdy(names(confirmed_countrycodes)[NCOL(confirmed_countrycodes)]),
                value = mdy(names(confirmed_countrycodes)[6]),
                timeFormat="%m/%d/%Y",
                animate = animationOptions()
    )
  })
  
  output$scatterp <- renderPlotly({
    require(input$date2)
    require(input$country)
    date <- gsub("\\b0+", "", format(input$date2, "%m/%d/%20"))
    
    if (!is.null(input$view2) & input$view2 == "Confirmed") {
      mapdata <- confirmed_countrycodes
    } else if (input$view2 == "Recovered") {
      mapdata <- recovered_countrycodes
    } else {
      mapdata <- deaths_countrycodes
    }
    
    if (input$density == TRUE) {
      mapdata[, 6:ncol(mapdata)] <- mapdata %>% transmute_at(6:NCOL(.), funs(./Density))
      mapdata[, 6:ncol(mapdata)] <- mapdata %>% transmute_at(6:NCOL(.), funs(round(.)))
    }
    
    if (input$scale == TRUE) {
      mapdata[, 6:ncol(mapdata)] <- mapdata %>% transmute_at(6:NCOL(.), funs(log(., 10)))
    }
    
    uptocol <- which(names(mapdata) == date)
    x <- names(mapdata)[6:uptocol]
    selectedcountry <- input$country
    selectedcountry <- c(selectedcountry, 
                         mapdata[unlist(event_data("plotly_click")[2]) + 1, ]$COUNTRY)
    y <- mapdata[mapdata[, 1] %in% selectedcountry, ]
    y <- t(unname(y[6:uptocol]))
    df2 <- data.frame(cbind(x, y))
    colnames(df2) <- c("date", mapdata[mapdata[, 1] %in% selectedcountry, 1])
    colNames <- colnames(df2)
    colNames <- colNames[-which(colNames == "date")]
    
    p <- plot_ly(type = "scatter", mode = "markers", marker = list(size = 14))
    for (trace in colNames) {
      p <- p %>% add_trace(data = df2, x = ~date, y = as.formula(paste0("~`", trace, "`")), 
                           name = trace)
      p <- p %>% layout(xaxis = list(title = "Day", categoryarray = ~df2[, 1], 
                                     categoryorder = "array"), 
                        yaxis = list(title = input$view2, categoryarray = ~trace, 
                                     categoryorder = "array"))
    }
    p
  })
  
  output$world2 <- renderPlotly({
    req(input$date2)
    req(input$country)
    date <- gsub("\\b0+", "", format(input$date2, "%m/%d/%20"))
    
    if (!is.null(input$view2) & input$view2 == "Confirmed") {
      mapdata <- confirmed_countrycodes
    } else if (input$view2 == "Recovered") {
      mapdata <- recovered_countrycodes
    } else {
      mapdata <- deaths_countrycodes
    }
    
    if (input$density == TRUE) {
      mapdata[, 6:ncol(mapdata)] <- mapdata %>% transmute_at(6:NCOL(.), funs(./Density))
      mapdata[, 6:ncol(mapdata)] <- mapdata %>% transmute_at(6:NCOL(.), funs(round(.)))
    }
    
    selectedcountry <- input$country
    selectedcountry <- c(selectedcountry, mapdata[unlist(event_data("plotly_click")[2]) + 1, ]$COUNTRY)
    mapdata <- add_column(mapdata, id = 1:NROW(mapdata), .before = 1)
    
    mapdata2 <- mapdata
    for (i in 1:NROW(mapdata2)) {
      mapdata2[i, 7:NCOL(mapdata2)] = NA
    }
    
    for (i in 1:length(selectedcountry)) {
      selectedcountry_row <- which(mapdata$COUNTRY == selectedcountry[i])
      mapdata2[selectedcountry_row, 7:NCOL(mapdata2)] = mapdata[selectedcountry_row, 7:NCOL(mapdata)]
    }
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(showframe = FALSE, scope = "world", landcolor = "lightgray", 
              showcoastlines = FALSE, projection = list(type = "Mercator"))
    
    mapdata2[, 7:NCOL(mapdata2)] <- mapdata2[, 7:NCOL(mapdata2)] %>% 
      replace(., is.na(.), 0)
    mapworld2 <- plot_geo(mapdata2)
    mapworld2 <- mapworld2 %>% 
      add_trace(z = ~mapdata2[, which(names(mapdata2) == date)], 
                color = "viridis", locations = ~CODE, 
                marker = list(line = l), hoverinfo = "text", 
                text = ~ifelse(mapdata2[, which(names(mapdata2) == date)] == 0, 
                COUNTRY, paste(COUNTRY, mapdata2[, which(names(mapdata2) == date)])))
    mapworld2 <- mapworld2 %>% layout(geo = g)
    mapworld2
  })
}

shinyApp(ui = ui, server = server)
