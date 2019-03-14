get_country_plot <- function(data, country_selected, y_selected) {
  dat <- data %>%
    filter(data[, 1] == country_selected) %>%
    filter(year >= y_selected[1], year <= y_selected[2]) %>%
    group_by(year) %>%
    summarise(n = sum(suicides_no))
  country_plot <- ggplot(dat, aes(year, n)) +
      geom_point(stat = "identity", color = "blue") +
      geom_line(color = "purple") +
      labs(x = "Year",
          y = "number of suicides",
          title = paste("Number of Suicides from ", y_selected[1],
                        " to ", y_selected[2], " in ", country_selected)) +
      theme(plot.title = element_text(face = "bold",
                                      margin = margin(0, 0, 20, 0))) +
     theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  return(country_plot)
}