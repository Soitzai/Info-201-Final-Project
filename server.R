# loading packages and sources
library("shiny")
library("ggplot2")
library("dplyr")
library("countrycode")
library("plotly")
library("grid")
source("ui.R")
source("./scripts/geo_map.R")
source("./scripts/country_plot.R")
source("./scripts/gdp_plot.R")
source("./scripts/comparison_plot.R")

c_data <- read.csv("./data/country.csv", stringsAsFactors = FALSE)
shinyServer(function(input, output) {
  output$line_chart <- renderPlot({
    chart_data <- data %>%
      filter(age == input$radio) %>%
      group_by(sex, year) %>%
      summarise(n = sum(suicides.100k.pop)) %>%
      select(year, n, sex)
    chart_data_all <- data %>%
      group_by(sex, year) %>%
      summarise(n = sum(suicides.100k.pop)) %>%
      select(year, n, sex)
    if (input$radio == "all age group") {
      chart_data <- chart_data_all
    }
    chart_data_female <- chart_data %>%
      filter(sex == "female")
    chart_data_male <- chart_data %>%
      filter(sex == "male")
    chart_title <- switch(input$radio,
                        "5-14 years" = "5 - 14 age group",
                        "15-24 years" = "15-24 age group",
                        "25-34 years" = "25-34 age group",
                        "35-54 years" = "35-54 age group",
                        "55-74 years" = "55-74 age group",
                        "75+ years" = "75+ age group",
                        "all age" = "all age group")
    yrange <- range(chart_data$n)
    xrange <- range(chart_data$year)
    plot(xrange, yrange, type = "n", xlab = "Year",
         ylab = "Suicides number(per 100k population)", cex.lab = 1.5,
         main = paste("Suicides per 100k population for female and male: ",
                      chart_title), sub = "Data: Kaggle.com")
    lines(chart_data_female$year, chart_data_female$n,
          col = "aquamarine4", lwd = 3)
    lines(chart_data_male$year, chart_data_male$n, col = "firebrick3", lwd = 3)
    abline(v = input$vertical, lty = 2)
    legend("topright", legend = c("female", "male"),
           col = c("aquamarine4", "firebrick3"),
           pch = 15, ncol = 1, bty = "n", cex = 1.1)
    if (input$hor) {
      selected_year <- input$vertical
      height <- chart_data %>%
        filter(year == selected_year)
      abline(h = height$n[1])
      abline(h = height$n[2])
    }
  })

  output$scartter_plot <- renderPlot({
    return(get_gdp_plot(data, input$input_year))
  })

  output$comparison <- renderPlot({
    return(get_comparison_plot(data))
  })

  output$plot2 <- renderPlot({
    return(get_country_plot(data, input$select, input$year_second))
  })


  output$plot3 <- renderPlotly({
    return(get_plot(data, input$year_third, input$gdp_third, c_data))
  })
})
