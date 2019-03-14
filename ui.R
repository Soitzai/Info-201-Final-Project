# load packages to the file
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")

# load in the data
data <- read.csv("./data/master.csv")
# filter out data from 2016 since the data for 2016 is not complete
# we will only use data from 1985 to 2015 for this project
data <- data %>% filter(year != 2016)
# country names in the data
country_list <- unique(as.character(data[, 1]))
# year range in the data
# 1985 to 2015
year_range <- range(data$year)
# gdp per capita range in the data
gdp_range <- range(data$gdp_per_capita....)

# create the ui for the shiny app
ui <- fluidPage(
  # link the css file
  theme = "style.css",
  titlePanel("Suicide Rates Overview 1985 to 2015"),
  tabsetPanel(
    #Overall page
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          class = "side",
          # introduction to the project
          tags$div(h3("Intro"),
                   tags$b("Suicide is one of the top ten common causes of death
                          in the world. People may consider suicide when they
                          are hopeless and can't see any other solution to
                          their problems. Often it's related to serious
                          depression, alcohol or substance abuse, or a major
                          stressful event.")
                   ),
          tags$div(h3("Questions"),
                   tags$ol(
                     tags$li(tags$i("What's the comparison of suicide numbers
                                    per 100k population of females and males
                                    in a certain age group or overall age
                                    groups?"),
                             tags$br(),
                             tags$b("-- Resolved by By Age-Group & Sex")),
                     tags$p(""),
                     tags$li(tags$i("How to find the correlation between GDP
                                    and suicide numbers?"),
                             tags$br(), tags$b("-- Resolved by By GDP")),
                     tags$p(""),
                     tags$li(tags$i("What's the comparison of suicide numbers
                                    in selected country and year period?"),
                             tags$br(), tags$b("-- Resolved by By Country")),
                     tags$p(""),
                     tags$li(tags$i("What's the overview of the worldwide
                                    suicide rate?"),
                             tags$br(), tags$b("-- Resolved by Map"))
                     )
                     )
                     ),
        mainPanel(
          class = "main",
          tags$div(h3("Insights"),
                   tags$i("Our analysis shows that the number of male
                          suicides per 100k population is higher than
                          female's. The age group with the highest number of
                          suicides is 75+. From our analysis of GDP, we
                          conclude that GDP had a greater correlation with
                          suicide rates before 2000. The higher GDP, the
                          higher suicide case. In US, the number of suicides
                          was on the rise from 1985 to 2015. In 2015,
                          Lithuania and south korea are outliers of worldwide
                          suicide rate.")
                   ),
          tags$p(""),
          tags$blockquote("Understanding the issues concerning suicide and
                          mental health is an important way to take part in
                          suicide prevention. Our analysis is dedicated to
                          understand the suicide condition and trend around
                          the globe from 1985 to 2015 in order to
                          better prevent suicides."),
          tags$p(""),
          tags$i("For more information about suicide in WA, Go to ",
                 tags$a(href =
                          "http://depts.washington.edu/hiprc/suicide/stats/",
                        "Pacific Northwest Suicide Prevention Resource Center."
                 )),
          tags$p(""),
          tags$i("If you are concerning about your mental health?"),
          tags$a(href = "https://suicidepreventionlifeline.org",
                 "GET HELP NOW!")
          )
                   )
        ), tabPanel( #first page
          "By Age-Group & Sex",
          sidebarLayout(
            sidebarPanel(
              class = "side",
              # radio select button
              radioButtons(
                "radio",
                label = "Select the age group",
                choices = list("5-14 years",
                               "15-24 years",
                               "25-34 years",
                               "35-54 years",
                               "55-74 years",
                               "75+ years",
                               "all age group"
                ),
                # set the default value
                selected = "25-34 years"
              ),
              # select to show which year's data
              selectInput(
                "vertical",
                label = "Show vertical line in year(s):",
                choices = 1985:2015,
                multiple = FALSE
              ),
              # select to add a horizontal line to better see the data or not
              checkboxInput("hor", "Show horizontal axis", TRUE)
            ),
            # show the result of the page
            mainPanel(
              h3(""),
              class = "main",
              plotOutput("line_chart"),
              tags$div(checked = NA, class = "discription",
                       tags$p("This plot shows the numbers of suicides per
                              100k population from 1985 to 2015, either for
                              one certain age group or for all age group.
                              It also gives the comparison of the numbers of
                              suicides among female and male."
                       ),
                       tags$p("From the graph above, we see that the red line,
                              which represents the number of suicides per 100k
                              population for male, is always above the green
                              line, which represents the number of suicides per
                              100k population for female. From that we can
                              conclude that, male has a higher amount of
                              suicides numbers over women. That tells the
                              government/organizations that male's suicide
                              situation definitely needs to be taken seriously.
                              "),
                       tags$p(
                         "According to ",
                         tags$a(href =
                                  "https://afsp.org/about-suicide/suicide-statistics/",
                                "American Foundation for Suicide Prevention"),
                         ", in 2017, men died by suicide 3.54x more often
                         than women.")
                       )
                       )
              )
          ), tabPanel( #page analysis by GDP
            "By GDP",
            sidebarLayout(
              sidebarPanel(
                class = "side",
                # selection of the interested year
                selectInput(
                  "input_year",
                  label = "Which year:",
                  choices = 1985:2015,
                  multiple = FALSE
                )
              ), mainPanel(
                h3(""),
                class = "main",
                # show the graph of the scartter plot and regression line
                plotOutput("scartter_plot"),
                # description of the graph
                tags$div(checked = NA, class = "discription",
                         tags$p("This plot shows the comparison of GDP per capita
                                and the number of suicides per 100k population in
                                the selected year. It shows how the suicides
                                condition is affected by GDP."
                         ),
                         tags$p("With the default values in 1985, we see that
                                the regression line is slightly upward, which
                                indicates that in 1985, a country with higher
                                GDP per capita tends to have a higher suicides
                                number per 100k population.")
                         ),
                # show the comparison between year
                plotOutput("comparison"),
                tags$div(checked = NA, class = "discription",
                         tags$p("To see how the relationship between GDP per
                                capita and suicides per 100k population changes
                                from 1985 to 2015, we plot the slope of each
                                regression line for each year. According to the
                                graph, we see that GDP per capita has less affect
                                on suicide number per 100k population over time,
                                especially after the year of 2000, GDP per capita
                                has little affect on suicide number per 100k
                                population. Since all the slopes have a positive
                                value, that means country with a higher GDP per
                                capita tends to have a higher suicides number per
                                100k population."
                         )
                         )
                )
              )
            ), tabPanel( #third page
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
                    h3(""),
                    class = "main",
                    plotOutput("plot2"),
                    tags$div(checked = NA, class = "discription",
                    tags$p("This plot shows the number of suicides
                            in selected country during the selected
                            year period. It helps the users to
                            understand the suicide trend in a certain
                            country. By understanding the trend, the
                            governments/organizations can better
                            understand the suicide situation in
                            the country and therefore use appropriate
                            strategies to reduce the suicides."
                           ),
                    tags$p("With the default values, suicide numbers
                            in the United States from 1985 to 2015,
                            we get the graph above, which shows the
                            trend of the number of suicides. The graph
                            shows that there is an incresing amount of
                            suicides in the United States, especially
                            after the year of 2000. It alerts the
                            governments/organizations to pay more
                            attentions about suicides situations
                            in the United States.")
                    )
                    )
                )
              ),
    tabPanel( #Third page
      "Map",
      sidebarLayout(
        sidebarPanel(
          class = "side",
          # selection of the year range of interest
          sliderInput(
            "year_third",
            label = "Year",
            min = year_range[1],
            max = year_range[2],
            value = year_range,
            step = 1
          ),
          # seelct the gdp range of interest
          sliderInput("gdp_third",
                      "GDP per capital",
                      min = gdp_range[1], max = gdp_range[2],
                      value = c(gdp_range[1], gdp_range[1] + 1000), step = 1000,
                      animate =
                        animationOptions(interval = 600, loop = TRUE))
        ), mainPanel(
          h3(""),
          class = "main",
          plotlyOutput("plot3"),
          tags$div(checked = NA, class = "discription",
                   tags$p("This plot gives an overview of the suicide rate
                          worldwide.
                          The suicide rate is the suicide number
                          dividing by the population/100 in the region."
                   ),
                   tags$p("Viewers can further narrow down the years of
                          interest and further investigate the distribution
                          in the selected range. The animation shows a general
                          trend over suicide rate as the GDP per capital
                          progress.")
                   )
          )
        )
      )
    )
  )
shinyUI(ui)
