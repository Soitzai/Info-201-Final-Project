get_comparison_plot <- function(data) {
  datset <- c()
  for (i in 1985:2015) {
    dat <- data %>%
      filter(year == i) %>%
      group_by(country, year, gdp_per_capita....) %>%
      summarise(n = sum(suicides.100k.pop))
    res <- lm(n~gdp_per_capita...., data = dat)
    datset[i] <- as.numeric(res$coefficients[2])
  }
  com_plot <- plot(datset, xlim = c(1985, 2015), xlab = "Year",
       ylab = "slope", main = "slope of regression line in each year",
       pch = 15, col = "blue")
  return(com_plot)
}