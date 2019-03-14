# plot the input country data with the suicide number
get_country_plot <- function(data, country_selected, y_selected) {
  # filter out the unnecessary data
  dat <- data %>%
    filter(data[, 1] == country_selected) %>%
    filter(year >= y_selected[1], year <= y_selected[2]) %>%
    group_by(year) %>%
    summarise(n = sum(suicides_no))
  # plot the graph to see the relationship
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
  # return the graph
  return(country_plot)
}