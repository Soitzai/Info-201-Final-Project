get_plot <- function(data, year_selected, gdp_selected, c_data) {
  new_data <- data %>%
    filter(year >= min(year_selected),
           year <= max(year_selected),
           gdp_per_capita.... >= min(gdp_selected),
           gdp_per_capita.... <= max(gdp_selected)) %>%
    group_by(country) %>%
    summarise(total = sum(suicides_no),
              total_pop = sum(as.numeric(population))) %>%
    mutate(country_code = countrycode(country, "country.name", "iso3c"),
           suicides_rate = total / total_pop * 100000)
  colnames(c_data)[colnames(c_data) == "ISO.alpha3.Code"] <- "country_code"
  
  df <- left_join(c_data, new_data, by = "country_code")
  df[is.na(df)] <- 0
  

  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)

  # specify map projection/options
  g <- list(
    scope = "world",
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = "Mercator")
  )

  m <- list(
    t = 100,
    pad = 4
  )

  t <- list(
    family = "Helvetica Neue",
    size = 18,
    weight = 3)

  p <- plot_geo(df) %>%
    add_trace(
      z = ~suicides_rate, color = ~suicides_rate, colors = "Purples",
      locations = ~country_code, marker = list(line = l)
    ) %>%
    colorbar(title = "Suicide / 100K Population", tickprefix = "%") %>%
    layout(
      margin = m,
      title = "Suicide Rate Distribution Across Countries",
      titlefont = t,
      geo = g
    )
  return(p)
}