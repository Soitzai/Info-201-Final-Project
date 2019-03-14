# load packages
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")

data <- read.csv("master.csv")
country_list <- as.character(data[,1])
year_range <- range(data$year)
gdp_range <- range(data$gdp_per_capita....)

ui <- fluidPage(
  theme = "style.css",
  # title
  titlePanel("Suicide Rates Overview 1985 to 2015"),
  tabsetPanel(
    #first page
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          
        ),
        mainPanel(
          
        )
      )
    ),
    tabPanel(
      "first page",
      sidebarLayout(
        sidebarPanel(
          class = "side",
          radioButtons(
            "radio", 
            label = h3("Select the age group"),
            choices = list("5-14 years",
                           "15-24 years",
                           "25-34 years",
                           "35-54 years",
                           "55-74 years",
                           "75+ years"
            ),
            selected = "25-34 years"
          ),
          selectInput(
            "vertical",
            label = "Show vertical line in year(s):",
            choices = 1985:2015,
            multiple = FALSE
          ),
          checkboxInput("hor", "Show horizontal axis", TRUE)
        ),
        mainPanel(
          h2("Main Panel"),
          textOutput("text1"),
          plotOutput("lineChart")
        )
      )
    ), 
    tabPanel( #second page
      "Scartter plot",
      sidebarLayout(
        sidebarPanel(
          class = 'side',
          # selection
          selectInput(
            "input_year",
            label = "which year:",
            choices = 1985:2015,
            multiple = FALSE
          )
        ), mainPanel(
          plotOutput("scartter_plot")
        )
      )
    ),
    tabPanel( #second page
      "Number of suicdes based on country and years",
      sidebarLayout(
        sidebarPanel(
          class = "side",
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
          )
        ), mainPanel(
          plotOutput("plot2")
        )
      )
    ),
    tabPanel( #Third page
      "Map",
      sidebarLayout(
        sidebarPanel(
          class = "side",
          # selection
          sliderInput(
            "year_third",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range,
            step = 1
          ),
          sliderInput("gdp_third",
                      "GDP per Capital",
                      min = gdp_range[1], max = gdp_range[2],
                      value = c(gdp_range[1], gdp_range[1] + 1000), step = 1000,
                      animate =
                        animationOptions(interval = 600, loop = TRUE))
        ), mainPanel(
          plotlyOutput("plot3"),
          tags$div(checked = NA, class = "discription",
            tags$p("This plot gives an overview of the suicide rate worldwide.
                   The suicide rate is the suicide number
                   dividing by the population/100 in the region."
                   ),
            tags$p("Viewers can further narrow down the years of interest
                   and further investigate the distribution in the selected range.
                   The animation shows a general trend over suicide rate
                   as the GDP per capital progress.")
          )
        )
      )
    )
  )
)
shinyUI(ui)
