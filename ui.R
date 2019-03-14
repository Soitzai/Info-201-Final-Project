# load packages
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")

data <- read.csv("master.csv")
data <- data %>% filter(year != 2016)
country_list <- unique(as.character(data[,1]))
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
          tags$div(h3("Intro"),
                   tags$b("Suicide is one of the top ten common causes of death
                          in the world. People may consider suicide when they 
                          are hopeless and can't see any other solution to 
                          their problems. Often it's related to serious 
                          depression, alcohol or substance abuse, or a major 
                          stressful event.")
          )
        ),
        mainPanel(
          tags$blockquote("Every suicide is a tragedy. According to estimates 
                          from the World Health Organisation (WHO), over 800,000 
                          people die due to suicide every year. This corresponds 
                          to an age-standardized suicide rate of around 11.5 per 
                          100,000 people – a figure equivalent to someone dying 
                          of suicide every 40 seconds. Yet suicides are 
                          preventable with timely, evidence-based 
                          interventions."),
          tags$blockquote("Understanding the issues concerning suicide and mental 
                          health is an important way to take part in suicide 
                          prevention. Our analysis is dedicated to understand the suicide 
                          condition and trend around the globe from 1985 to 2015 in order to 
                          better prevent suicides."),
          tags$p(""),
          tags$i("For more information about suicide in WA."),
          tags$a(href="http://depts.washington.edu/hiprc/suicide/stats/", 
                 "Click Here!"),
          tags$p(""),
          tags$i("If you are concerning about your mental health?"), 
          tags$a(href="https://suicidepreventionlifeline.org", "GET HELP NOW!")
        )
      )
    ),tabPanel(
      "By Age-Group & Sex",
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
                           "75+ years",
                           "all age group"
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
          h2(""),
          plotOutput("lineChart"),
          tags$div(checked = NA, class = "discription",
                   tags$p("This plot shows the numbers of suicides per 100k population from 1985 to 2015, either for 
                          one certain age group or for all age group.It also gives the comparison of the numbers of suicides
                          among female and male."
                   ),
                   tags$p("From the graph above, we see that the red line, which represents the number of 
                          suicides per 100k population for male, is always above the green line, which represents 
                          the number of suicides per 100k population for female. From that we can conclude that, male
                          has a higher amount of suicides numbers over women. That tells the government/organizations 
                          that male's suicide situation definitely needs to be taken seriously."),
                   tags$p("According to " ,
                          tags$a(href="https://afsp.org/about-suicide/suicide-statistics/","American Foundation for Suicide Prevention"), 
                          ", in 2017, men died by suicide 3.54x more often than women.")
                   )
        )
      )
    ), tabPanel( #second page
      "By GDP",
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
          h2(""),
          plotOutput("scartter_plot"),
          tags$div(checked = NA, class = "discription",
                   tags$p("This plot shows the comparison of GDP per capita and the number 
                          of suicides per 100k population in the selected year. It shows how 
                          the suicides condition is affected by GDP."
                   ),
                   tags$p("With the default values in 1985, we see that the regression line is
                          slightly upward, which indicates that in 1985, a country with higher GDP per capita
                          tends to have a higher suicides number per 100k population.")
                   ),
          plotOutput("comparison"),
          tags$div(checked = NA, class = "discription",
                   tags$p("To see how the relationship between GDP per capita and suicides per 100k population 
                          changes from 1985 to 2015, we plot the slope of each regression line for each year. According to the graph, 
                          we see that GDP per capita has less affect on suicide number per 100k population over time, especially after the
                          year of 2000, GDP per capita has little affect on suicide number per 100k population.
                          Since all the slopes have a positive value, that means country with a higher GDP per capita 
                          tends to have a higher suicides number per 100k population. 
                          "
                   )
          )
        )
      )
    ),tabPanel( #third page
      "By Country",
      sidebarLayout(
        sidebarPanel(
          class = "side",
          # selection
          selectInput(
            "select", 
            label = "Country", 
            choices = country_list,
            selected = "United States"
          ), 
          sliderInput(
            "year_second",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range
          )
        ), mainPanel(
          h2(""),
          plotOutput("plot2"),
          tags$div(checked = NA, class = "discription",
                   tags$p("This plot shows the number of suicides in selected country during the selected year period.
                          It helps the users to understand the suicide trend in a certain country. By understanding the 
                          trend, the governments/organizations can better understand the suicide situation in
                          the country and therefore use appropriate strategies to reduce the suicides."
                   ),
                   tags$p("With the default values, suicide numbers in the United States from 1985 to 2015,
                          we get the graph above, which shows the trend of the number of suicides. The graph 
                          shows that there is an incresing amount of suicides in the United States, especially 
                          after the year of 2000. It alerts the governments/organizations to pay more attentions 
                          about suicides situations in the United States.")
          )
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
          h2(""),
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
