# loading packages
library("shiny")
library("ggplot2")
library("dplyr")
library("countrycode")
library("plotly")

source("geo_map.R")

data <- read.csv("master.csv",stringsAsFactors = FALSE)


shinyServer(function(input, output) {
  output$age_total_suicide <- renderPlot({
    # filter data
    dat_age_total_suicide <- data %>%
      filter(year >= input$year_first[1], year <= input$year_first[2]) %>%
      group_by(age) %>% 
      summarise(n = sum(suicides_no))
    # create ggplot
    ggplot(data = dat_age_total_suicide) +
      geom_bar(
        mapping = aes(x = age, y = n),
        stat = "identity", fill = input$color1
      ) +
      ggtitle(paste0("Total suicide number from ",
                     input$year_first[1], " to ", input$year_first[2])) +
      labs(
        x = "Age Group",
        y = "Total number of suicides"
      )
  })
  
  filtered_data <- reactive({
    filter_table <- filter(data, data[, 1] == input$select)
    filter_table <- filter(filter_table, year >= input$year_second[1] & year <= input$year_second[2])
    filter_table
  })
  output$plot2 <- renderPlot({
    scatter <- ggplot(filtered_data(), aes( year,  suicides_no)) + 
      geom_bar(stat = "identity")
    scatter
  })

  # Select widgit
# output$value <- renderPrint({ input$select })
  # Slider widgit
# output$value <- renderPrint({ input$slider })
# output$range <- renderPrint({ input$slider2 })
  # Filter data based on page 1 input
  
  output$plot3 <- renderPlotly({
    get_plot(data, input$year_third)
    return(get_plot(data, input$year_third))
  })
})

