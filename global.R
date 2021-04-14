# Load packages--- packgs <- c( 'colourpicker', 'dplyr', 'ggplot2', 'lubridate',
# 'tidyr', 'raster', 'shiny', 'shinyWidgets' )

# for (i in packgs) { if (!(i %in% installed.packages())) { install.packages(i) }
# library(i, character.only = TRUE) }

library(broom)
library(colourpicker)
library(dplyr)
library(ggplot2)
library(gpclib)
library(lubridate)
library(maptools)
library(tidyr)
library(raster)
library(rgeos)
library(shiny)
library(shinyWidgets)

# Variables---

dir.create("data")
# PR map data(no municipal divisios)
pr <- map_data("world", "puerto rico")

# Data with coordinates of municipality centroids
munis_center <- read.csv("centroidesmunicipioslatlong.csv")
munis_center <- munis_center[order(munis_center$municipio), ]

# Sp PR data with municipal divisions. From GADM
munis_gcoords <- getData(name = "GADM", country = "PR", path = "data", level = 1)

# PR municipalities
munis <- munis_gcoords$NAME_1

# Isolated data by municipalities
munis_gcoords@data <- munis_gcoords@data %>% mutate(id = munis)
munis_gcoords.df <- tidy(munis_gcoords, region = "id")


# Functions---

# Finds the municipality's name after it is clicked.
find_muni <- function(point) {
    # A click's location will likely have differences to the municipality centers
    dif <- abs(point[[1]] - munis_center$x_long) + abs(point[[2]] - munis_center$y_lat)
    # The answer is the municipality that has minimum difference
    return(munis[which.min(dif)])
}

# Displays earthquakes summary according to user specified criteria
get_summary <- function(summary, data, reps) {
    summary = as.character(eval(summary))
    if (summary != "Total" && summary != "Average" && summary != "Max") {
        return("valid inputs: tot, max, average")
    }

    summary_tot <- function() {
        summary_per_muni <- c()
        for (m in 1:length(munis)) {
            if (sum(data$muni == munis[m]) == 0) {
                summary_per_muni[m] <- 0
            } else {
                summary_per_muni[m] <- sum(data$muni == munis[m])
            }
        }
        new_col <- c()
        for (i in 1:length(munis)) {
            new_col <- c(new_col, rep(summary_per_muni[i], reps[i, ]))
        }
        munis_gcoords.df <- cbind(munis_gcoords.df, tot_amount_eartq = new_col)
        return(munis_gcoords.df)
    }

    summary_average <- function() {
        summary_per_muni <- c()
        for (m in 1:length(munis)) {
            if (sum(data$muni == munis[m]) == 0) {
                summary_per_muni[m] <- 0
            } else {
                summary_per_muni[m] <- mean(data[(which(data$muni == munis[m])), 11])
            }
        }
        new_col <- c()
        for (i in 1:length(munis)) {
            new_col <- c(new_col, rep(summary_per_muni[i], reps[i, ]))
        }
        munis_gcoords.df <- cbind(munis_gcoords.df, average_amount_eartq = new_col)
        return(munis_gcoords.df)
    }

    summary_max <- function() {
        summary_per_muni <- c()
        for (m in 1:length(munis)) {
            if (sum(data$muni == munis[m]) == 0) {
                summary_per_muni[m] <- 0
            } else {
                summary_per_muni[m] <- max(data[(which(data$muni == munis[m])), 11])
            }
        }
        new_col <- c()
        for (i in 1:length(munis)) {
            new_col <- c(new_col, rep(summary_per_muni[i], reps[i, ]))
        }
        munis_gcoords.df <- cbind(munis_gcoords.df, max_mag_eartq = new_col)
        return(munis_gcoords.df)
    }

    switch(summary, Total = summary_tot(), Average = summary_average(), Max = summary_max())
}


# UI settings---
colors <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", 
    "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
titles <- c(`Maximum Magnitudes of Earthquakes` = "Max", `Average Amount of Earthquakes` = "Average", 
    `Total Amount of Earthquakes` = "Total")
titles2 <- c(`Maximum Magnitudes of Earthquakes Satifying Chosen Criteria` = "Max", `Average Amount of Earthquakes Satifying Chosen Criteria` = "Average", 
    `Total Amount of Earthquakes Satifying Chosen Criteria` = "Total")
legends <- c(`Max Magnitude` = "Max", `Average Amount` = "Average", `Total Amount` = "Total")
options <- c(`Maximum Magnitude` = "Max", `Average Amount` = "Average", `Total Amount` = "Total")

radioLab <- list(tags$div(align = "left", class = "multicol", radioButtons(inputId = "crit", 
    label = "Criteria", choices = list(`<=` = 1, `>=` = 2), selected = 1)))

multicolLab <- list(tags$head(tags$style(HTML("
    .shiny-options-group {
    height: auto;
    width: 250px;
    -webkit-column-count: 2; /* Chrome, Safari, Opera */
    -moz-column-count: 2;    /* Firefox */
    column-count: 2;
    -webkit-column-fill: balance;
    -moz-column-fill: balance;
    column-fill: balance;
    margin-top: 0px;
    }
    
    .control-label {
    padding-bottom: 10px;
    }
    
    div.radio {
    margin-top: 0px;
    margin-bottom: 0px;
    padding-bottom: 10px;
    }"))))
