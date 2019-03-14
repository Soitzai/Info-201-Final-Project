get_comparison_plot <- function(data) {
    # save all the slope representing the correlation over time in a
    # datastructure
    datset <- c()
    # loop over the data based on year
    for (i in 1985:2015) {
      dat <- data %>% 
        filter(year == i) %>%
        group_by(country, year, gdp_per_capita....) %>%
        summarise(n = sum(suicides.100k.pop))
      res <- lm(n~gdp_per_capita...., data = dat)
      datset[i] <- as.numeric(res$coefficients[2])
    }
    # plot the data and well labeled the graph
    plot(datset, xlim = c(1985,2015), xlab = "Year", 
         ylab = "slope", main = "slope of regression line in each year", pch = 15, col = "blue")
}
