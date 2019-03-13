# load packages
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")

data <- read.csv("master.csv")
country_list <- as.character(data[,1])
year_range <- range(data$year)

ui <- fluidPage(
  theme = "style.css",
  # title
  titlePanel("Suicide Rates Overview 1985 to 2016"),
  tabsetPanel(
    #first page
    tabPanel(
      "first page",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            "year_first",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range,
            round = T,
            format = "####"
          ),
          selectInput(
            "color1",
            label = "Color",
            choices = list(
              "Pink" = "pink",
              "Blue" = "blue",
              "Green" = "green",
              "Purple" = "purple",
              "Red" = "red"
            )
          )
        ),
        mainPanel(
          h2("Total suicide in each Age group in a given period"),
          plotOutput("age_total_suicide")
        )
      )
    ), tabPanel( #second page
      "Number of suicdes based on country and years",
      sidebarLayout(
        sidebarPanel(
          # selection
          selectInput(
            "select", 
            label = ("Country"), 
            choices = country_list
          ), 
          sliderInput(
            "year_second",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range,
            round = T,
            format = "####"
          )
        ), mainPanel(
          plotOutput("plot2")
        )
      )
    ),
    tabPanel( #second page
      "Map",
      sidebarLayout(
        sidebarPanel(
          # selection
          sliderInput(
            "year_third",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range,
            step = 1
          ),
          sliderInput("animation", "Looping Animation:",
                      min = year_range[1], max = year_range[2],
                      value = 1, step = 1,
                      animate =
                        animationOptions(interval = 300, loop = TRUE))
        ), mainPanel(
          plotlyOutput("plot3")
        )
      )
    )
  )
)
shinyUI(ui)
