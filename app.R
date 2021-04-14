ui <- fluidPage(
  titlePanel("Earthquake Summary 30 Days from Selected Date"),
  sidebarLayout(sidebarPanel(
    selectInput(
      "viz",
      "Mode",
      choices = c("General Summary", "Summary Following Criteria"),
      selected = "General Summary",
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.viz == 'General Summary'",
      dateInput(
        "date",
        "Starting from",
        value = "2020-01-01",
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "en",
        width = NULL,
        autoclose = TRUE,
        datesdisabled = NULL,
        daysofweekdisabled = NULL
      ),
      selectInput(
        "summary",
        "Earthquake summary per municipality:",
        choices = options,
        selected = options[1],
        multiple = FALSE
      ),
      selectInput(
        "col",
        "Color palette",
        choices = colors,
        selected = "Oranges",
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.viz == 'Summary Following Criteria'",
      dateInput(
        "date2",
        "Starting from",
        value = "2020-01-01",
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "en",
        width = NULL,
        autoclose = TRUE,
        datesdisabled = NULL,
        daysofweekdisabled = NULL
      ),
      selectInput(
        "summary2",
        "Earthquake summary per municipality:",
        choices = options,
        selected = options[1],
        multiple = FALSE
      ),
      multicolLab,
      fluidRow(column(
        width = 6, radioLab, align = "center"
      ),
      column(6)),
      uiOutput("numval"),
      selectInput(
        "col2",
        "Color palette",
        choices = colors,
        selected = "Oranges",
        multiple = FALSE
      ),
      width = 4
    )
  ),
    mainPanel(
      conditionalPanel(
        condition = "input.viz == 'General Summary'",
        plotOutput("plot1", click = "plot_click"),
        htmlOutput("info"),
        tags$head(tags$style(
          HTML(
            " #info {
            font-size: 15px;
            border: 2.5px solid grey;
            border-radius: 5px ;
            }"
            )
          ))
        ), 
      conditionalPanel(
        condition = "input.viz == 'Summary Following Criteria'",
        plotOutput("plot2", click = "plot_click2"),
        htmlOutput("info2"),
        tags$head(tags$style(
          HTML(
            "#info2 {
            font-size: 15px;
            border: 2.5px solid grey;
            border-radius: 5px ;
            }"
          )
          ))
          ),
      width = 8
          )
        )
  )

server <- function(input, output, session) {
  
  # To retrieve data. It will contain data 30 days from specified date
  timeframe <- reactive({ # data 
    from <- as.Date(input$date)
    thru <- from + 1
    url <- paste0(
      "https://earthquake.usgs.gov/fdsnws/event/1/query?format=text&starttime=",
      from,
      "&endtime=",
      thru
    )
    earthq_data <- read.delim(url, sep = "|")
    pr_earthq <- earthq_data[grep("Puerto Rico", earthq_data$EventLocationName), ]
    point <- data.frame(x = pr_earthq$Longitude, y = pr_earthq$Latitude)
    pr_earthq$muni <- apply(point, 1, find_muni)
  
    times <- munis_gcoords.df %>% group_by(id) %>%  
      summarise(n = n()) %>% dplyr::select(n) # epicenter count by municipality
    return(list(Data = as.data.frame(pr_earthq),  Times = as.data.frame(times)))
  })
  
  # To adjust values in slider depending on summary obtained with get_summary()
  data_slider <- reactive({
    req(input$summary2)
    sliderdat <- get_summary(input$summary2, timeframe()$Data, timeframe()$Times)
    return(sliderdat)
  })
  
  data <- reactive({   # data for plot1
    req(input$summary)
    munis_gcoords.df2 <- get_summary(input$summary, timeframe()$Data, timeframe()$Times)
    return(munis_gcoords.df2)
  })
  
  output$numval <- renderUI({
    input$summary2
    sliderInput(
      "numval",
      label = "slider input",
      min = min(data_slider()[, 8]),
      max = max(data_slider()[, 8]),
      value = 0
    )
  })
  
  data_sub <- reactive({   # data for plot2
    req(input$summary2)
    if (input$crit == 1) {
      munis_gcoords.df2 <- get_summary(input$summary2, timeframe()$Data,
                                       timeframe()$Times)
      munis_gcoords.df2 <- subset(munis_gcoords.df2,
                                  munis_gcoords.df2[, 8] <= input$numval)
    } else {
      munis_gcoords.df2 <- get_summary(input$summary2, timeframe()$Data, timeframe()$Times)
      munis_gcoords.df2 <- subset(munis_gcoords.df2, munis_gcoords.df2[, 8] >= input$numval)
    }
    return(munis_gcoords.df2)
  })
  
  #  To get the name of the clicked municipality
  assess_click <- function(click) {
    if (is.null(click))
      return("Click on the map \n")
    # Valid clicks will be inside maximum and minimum of latitude and longitude 
    # coordinates in munis_center dataset
    w_lim <- min(munis_center$x_long)
    e_lim <- max(munis_center$x_long)
    n_lim <- max(munis_center$y_lat)
    s_lim <- min(munis_center$y_lat)
    if (e_lim + 0.05 < click$x || click$x < w_lim - 0.05)
      return("Invalid point \n")
    if (n_lim + 0.05 < click$y || click$y < s_lim - 0.05)
      return("Invalid point \n")
    else
      return(find_muni(click))
  }
  
  observe({  
    output$plot1 <- renderPlot({
      p <- ggplot() +
        geom_polygon(
          data = data(),
          aes(
            x = long,
            y = lat,
            group = group,
            fill = data()[, 8]
          ),
          color = "black"
        ) +
        theme_void() +
        scale_fill_distiller(palette = input$col, direction = 1)
      p + geom_polygon(
        data = subset(data(), id == input$plot_click),
        aes(x = long, y = lat, group = group),
        fill = NA
      ) +
        theme_void() +
        labs(fill = names(legends[which(legends == input$summary)]), 
             title = names(titles[which(titles == input$summary)])) +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$info <- renderText({ 
      paste(assess_click(input$plot_click),
            "<b>",
            unique(subset(
              data(), id == assess_click(input$plot_click) # clicked municipality name
            )[[8]]),
            "</b>")
    })
  })
  
  observe({
    output$plot2 <- renderPlot({
      p <-
        ggplot() + geom_polygon(
          data = data(),
          aes(x = long, y = lat, group = group),
          fill = NA,
          colour = "black"
        ) +
        theme_void()
      p + geom_polygon(
        data = data_sub(),
        aes(
          x = long,
          y = lat,
          group = group,
          fill = data_sub()[,8]
        ),
        colour = "black"
      ) +
        theme_void() + scale_fill_distiller(palette = input$col2 , direction = 1) +
        labs(fill = names(legends[which(legends == input$summary2)]), 
             title = names(titles[which(titles == input$summary2)])) +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$info2 <- renderText({
      paste(assess_click(input$plot_click2),
            "<b>",
            unique(subset(
              data_sub(), id == assess_click(input$plot_click2)
            )[[8]]),
            "</b>")
    })
  })
}

shinyApp(ui = ui, server = server)