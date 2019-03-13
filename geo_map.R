#library(shiny)
#library(dplyr)
#library(plotly)
#library(countrycode)

get_plot <- function(data, year_selected) {
  df <- data %>%
    group_by(country) %>%
    filter(year == year_selected) %>%
    summarise(total = sum(suicides_no)) %>%
    mutate(country_code = countrycode(country, "country.name", "iso3c"))
  
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
      z = ~total, color = ~total, colors = 'Blues',
      text = ~country, locations = ~country_code, marker = list(line = l)
    ) %>%
    colorbar(title = 'Total Suicide Population since 1985', tickprefix = '%') %>%
    layout(
      title = 'Suicide Distribution Across Countries<br>
      Source:<a href="https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016">Suicide Rates Overview 1985 to 2016</a>',
      geo = g
    )
  return(p)
}


