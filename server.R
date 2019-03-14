# loading packages
library("shiny")
library("ggplot2")
library("dplyr")
library("countrycode")
library("plotly")

source("geo_map.R")

data <- read.csv("master.csv",stringsAsFactors = FALSE)
c_data <- read.csv("country.csv",stringsAsFactors = FALSE)



shinyServer(function(input, output) {
  output$lineChart <- renderPlot({
    chartData <- data %>% 
      filter(age == input$radio) %>%
      group_by(sex, year) %>%
      summarise(n = sum(suicides.100k.pop)) %>%
      select(year, n, sex)
    chartData_female <- chartData %>% 
      filter(sex == "female")
    chartData_male <- chartData %>%
      filter(sex == "male")
    charTitle <- switch(input$radio,
                        "5-14 years" = "0-14",
                        "15-24 years" = "15-24",
                        "25-34 years" = "25-34",
                        "35-54 years" = "35-54",
                        "55-74 years" = "55-74",
                        "75+ years" = "75+")
    yrange <- range(chartData$n)
    xrange <- range(chartData$year)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "",cex.lab=1.5, 
         main = paste("title"), sub="")
    lines(chartData_female$year, chartData_female$n, col = "aquamarine4", lwd=3)
    lines(chartData_male$year, chartData_male$n, col = "firebrick3", lwd=3)
    abline(v = input$vertical, lty=2)
    legend("topright", legend = c("female", "male"), col = c("aquamarine4", "firebrick3"), pch=15,ncol=1,bty="n", cex=1.1)
    if (input$hor) {
      selected_year <- input$vertical
      height <- chartData %>% filter(year == selected_year)
      abline(h=height$n[1])
      abline(h=height$n[2])
    }
  })
  
  output$scartter_plot <- renderPlot({
    dat <- data %>% 
      filter(year == input$input_year) %>%
      group_by(country, year, gdp_per_capita....) %>%
      summarise(n = sum(suicides.100k.pop))
    
    ggplot(data = dat, aes(x = gdp_per_capita...., y = n)) +
      geom_point(size = 1,
                 color = "purple") +
      geom_smooth(method = "lm", formula=y~x) +
      labs(x = "GDP per capita in each country", y = "suicides per 100k population", title = "title")
  })
  
  
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
  
  output$plot3 <- renderPlotly({
    return(get_plot(data, input$year_third, input$gdp_third, c_data))
  })
})

