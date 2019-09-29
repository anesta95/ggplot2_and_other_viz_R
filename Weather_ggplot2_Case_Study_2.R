library(dplyr)
library(ggplo2)

# Finish the clean_weather function
clean_weather <- function(file) {
  weather <- read.fwf(file,
                      header = FALSE,
                      col.names = c("month", "day", "year", "temp"),
                      widths = c(14, 14, 13, 4))
  weather %>%
    filter(!(month == 2 & day == 29)) %>%
    group_by(year) %>%
    mutate(yearday = 1:length(day)) %>%
    ungroup() %>%
    filter(temp != -99)
}

# Import NYNEWYOR.txt: my_data
my_data <- clean_weather("NYNEWYOR.txt")

# Create the stats object
StatHistorical <- ggproto("StatHistorical", Stat,
                          compute_group = function(data, scales, params) {
                            data <- data %>%
                              filter(year != max(year)) %>%
                              group_by(x) %>%
                              mutate(ymin = Hmisc::smean.cl.normal(y)[3],
                                     ymax = Hmisc::smean.cl.normal(y)[2]) %>%
                              ungroup()
                          },
                          required_aes = c("x", "y", "year"))

# Create the layer
stat_historical <- function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  list(
    layer(
      stat = "identity", data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, col = "#EED8AE", alpha = 0.3, shape = 16, ...)
    ),
    layer(
      stat = StatHistorical, data = data, mapping = mapping, geom = "linerange",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, col = "#8B7E66", ...)
    )
  )
}

# Build the plot
my_data <- clean_weather("NYNEWYOR.txt")
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical()

# Create the stats object
StatPresent <- ggproto("StatPresent", Stat,
                       compute_group = function(data, scales, params) {
                         data <- filter(data, year == max(year))
                       },
                       required_aes = c("x", "y", "year"))

# Create the layer
stat_present <- function(mapping = NULL, data = NULL, geom = "line",
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ...) {
  layer(
    stat = StatPresent, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Build the plot
my_data <- clean_weather("NYNEWYOR.txt")
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present()

# Create the stats object
StatExtremes <- ggproto("StatExtremes", Stat,
                        compute_group = function(data, scales, params) {
                          
                          present <- data %>%
                            filter(year == max(year)) 
                          
                          past <- data %>%
                            filter(year != max(year)) 
                          
                          past_extremes <- past %>%
                            group_by(x) %>%
                            summarize(past_low = min(y),
                                      past_high = max(y))
                          
                          # transform data to contain extremes
                          data <- present %>%
                            left_join(past_extremes) %>%
                            mutate(record = ifelse(y < past_low, 
                                                   "#0000CD", 
                                                   ifelse(y > past_high, 
                                                          "#CD2626", 
                                                          "#00000000")))
                        },
                        required_aes = c("x", "y", "year"))

# Create the layer
stat_extremes <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatExtremes, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Build the plot
my_data <- clean_weather("NYNEWYOR.txt")
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present() +
  stat_extremes(aes(col = ..record..)) +
  scale_color_identity() # Color specification


# File paths of all datasets
my_files <- c("NYNEWYOR.txt","FRPARIS.txt", "ILREYKJV.txt", "UKLONDON.txt")

# Build my_data with a for loop
my_data <- NULL
for (file in my_files) {
  temp <- clean_weather(file)
  temp$id <- sub(".txt", "", file)
  my_data <- rbind(my_data, temp)
}

# Build the final plot, from scratch!
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present() +
  stat_extremes(aes(col = ..record..)) +
  scale_color_identity() +  # specify color here
  facet_wrap(~id, ncol = 2)