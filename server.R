# loading packages and sources
library("shiny")
library("ggplot2")
library("dplyr")
library("countrycode")
library("plotly")
library("grid")
# source files to be used
source("ui.R")
source("./scripts/geo_map.R")
source("./scripts/country_plot.R")
source("./scripts/gdp_plot.R")
source("./scripts/comparison_plot.R")

# load in the country code data
c_data <- read.csv("./data/country.csv", stringsAsFactors = FALSE)
#shiny server for the project
shinyServer(function(input, output) {
  # create the line chart to be outputed
  output$line_chart <- renderPlot({
    # filter out to get necessary data
    chart_data <- data %>%
      filter(age == input$radio) %>%
      group_by(sex, year) %>%
      summarise(n = sum(suicides.100k.pop)) %>%
      select(year, n, sex)
    # data for all age group
    chart_data_all <- data %>%
      group_by(sex, year) %>%
      summarise(n = sum(suicides.100k.pop)) %>%
      select(year, n, sex)
    # see if it is for all age group or not
    if (input$radio == "all age group") {
      chart_data <- chart_data_all
    }
    # see the data for female
    chart_data_female <- chart_data %>%
      filter(sex == "female")
    # see the data for male
    chart_data_male <- chart_data %>%
      filter(sex == "male")
    # change the chart_title variable which represent the age group
    chart_title <- switch(input$radio,
                        "5-14 years" = "5 - 14 age group",
                        "15-24 years" = "15-24 age group",
                        "25-34 years" = "25-34 age group",
                        "35-54 years" = "35-54 age group",
                        "55-74 years" = "55-74 age group",
                        "75+ years" = "75+ age group",
                        "all age" = "all age group")
    # suicide number range
    yrange <- range(chart_data$n)
    # year range
    xrange <- range(chart_data$year)
    # create the plot to be shown with clear explanation
    plot(xrange, yrange, type = "n", xlab = "Year",
         ylab = "Suicides number(per 100k population)", cex.lab = 1.5,
         main = paste("Suicides per 100k population for female and male: ",
                      chart_title), sub = "Data: Kaggle.com")
    # the line represent the female data
    lines(chart_data_female$year, chart_data_female$n,
          col = "aquamarine4", lwd = 3)
    # the line represents the male data
    lines(chart_data_male$year, chart_data_male$n, col = "firebrick3", lwd = 3)
    # the vertical line of the interested year to make the graph more clear
    abline(v = input$vertical, lty = 2)
    # specify the color of the line representations
    legend("topright", legend = c("female", "male"),
           col = c("aquamarine4", "firebrick3"),
           pch = 15, ncol = 1, bty = "n", cex = 1.1)
    # to show the horizontal line or not based on the user input
    if (input$hor) {
      selected_year <- input$vertical
      height <- chart_data %>%
        filter(year == selected_year)
      abline(h = height$n[1])
      abline(h = height$n[2])
    }
  })

  # the scartter plot and the regression line of gdp and year
  output$scartter_plot <- renderPlot({
    return(get_gdp_plot(data, input$input_year))
  })

  # comparison scartter plot to show the relationship between 
  # the correlation change of gdp versus year over time
  output$comparison <- renderPlot({
    # get the comparison plot from a source file
    return(get_comparison_plot(data))
  })
  
  output$plot2 <- renderPlot({
    # get the country plot from a source file
    return(get_country_plot(data, input$select, input$year_second))
  })

  output$plot3 <- renderPlotly({
    # get the graph from a source file
    return(get_plot(data, input$year_third, input$gdp_third, c_data))
  })
})
