get_gdp_plot <- function(data,  y_selected) {
  dat <- data %>% 
    filter(year == y_selected) %>%
    group_by(country, year, gdp_per_capita....) %>%
    summarise(n = sum(suicides.100k.pop))
  
  ggplot(data = dat, aes(x = gdp_per_capita...., y = n)) +
    geom_point(size = 1.5,
               color = "purple") +
    geom_smooth(method = "lm", formula=y~x) +
    ggtitle(paste("Correlation between Suicides per 100k population and GDP per capita in ", 
                  y_selected)) +
    labs(x = "GDP per capital", y = "suicides number(per 100k population)") +
    theme(plot.title = element_text(face="bold", 
                                    margin = margin(0, 0, 20, 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm"))
}