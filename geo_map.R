#library(shiny)
#library(dplyr)
#library(plotly)
#library(countrycode)

get_plot <- function(data, year_selected) {
  df <- data %>%
    filter(year %in% year_selected) %>%
    group_by(country) %>%
    summarise(total = sum(suicides_no), 
              total_pop = sum(population)) %>%
    mutate(country_code = countrycode(country, "country.name", "iso3c"),
           suicides_rate = total / total_pop * 100000)
  View(df)
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  p <- plot_geo(df) %>%
    add_trace(
      z = ~suicides_rate, color = ~suicides_rate, colors = 'Blues',
      text = ~country, locations = ~country_code, marker = list(line = l)
    ) %>%
    colorbar(title = 'Suicide per 100K Population', tickprefix = '%') %>%
    layout(
      title = 'Suicide Distribution Across Countries',
      geo = g
    )
  return(p)
}


