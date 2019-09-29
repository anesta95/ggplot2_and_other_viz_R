library(dplyr)
library(ggplot2)
# Import weather data
weather <- read.fwf("NYNEWYOR.txt",
                    header = FALSE,
                    col.names = c("month", "day", "year", "temp"),
                    widths = c(14, 14, 13 ,4))

# Check structure of weather
str(weather)

# Create past with two filter() calls
past <- weather %>%
  filter(!(month == 2 & day == 29)) %>%
  filter(year != max(year))

# Check structure of past
str(past)

# Create new version of past
past_summ <- past %>%
  group_by(year) %>%
  mutate(yearday = 1:length(day)) %>%
  ungroup() %>%
  filter(!(temp == -99)) %>%
  group_by(yearday) %>%
  mutate(max = max(temp),
         min = min(temp),
         avg = mean(temp),
         CI_lower = Hmisc::smean.cl.normal(temp)[2],
         CI_upper = Hmisc::smean.cl.normal(temp)[3]) %>%
  ungroup()

# Structure of past_summ
str(past_summ)

# Adapt historical plot
ggplot(past_summ, aes(x = yearday, y = temp)) +
  geom_point(col = "#EED8AE", alpha = 0.3, shape = 16) +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), col = "#8B7E66")

# weather and past are available in your workspace

# Create present
present <- weather %>%
  filter(!(month == 2 & day == 29)) %>%
  filter(year == max(year)) %>%
  group_by(year) %>%
  mutate(yearday = 1:length(day)) %>%
  ungroup() %>%
  filter(temp != -99)

# Add geom_line to ggplot command
ggplot(past_summ, aes(x = yearday, y = temp)) + 
  geom_point(col = "#EED8AE", alpha = 0.3, shape = 16) +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), col = "#8B7E66") + 
  geom_line(data = present, aes(x = yearday, y = temp), col = "#EE204D")

# Create past_highs
past_highs <- past_summ %>%
  group_by(yearday) %>%
  summarize(past_high = max(temp))

# Create record_high
record_high <- present %>%
  left_join(past_highs) %>%
  filter(temp > past_high)

# Add record_high information to plot
ggplot(past_summ, aes(x = yearday, y = temp)) + 
  geom_point(col = "#EED8AE", alpha = 0.3, shape = 16) +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), col = "#8B7E66") +
  geom_line(data = present) +
  geom_point(data = record_high, col = "#CD2626")

# Create past_extremes
past_extremes <- past_summ %>%
  group_by(yearday) %>%
  summarize(past_low = min(temp),
            past_high = max(temp))

# Create record_high_low
record_high_low <- present %>%
  left_join(past_extremes) %>%
  mutate(record = ifelse(temp < past_low, 
                         "#0000CD",
                         ifelse(temp > past_high, 
                                "#CD2626", 
                                "#00000000")))

# Structure of record_high_low
str(record_high_low)

# Add point layer of record_high_low
p <- ggplot(past_summ, aes(x = yearday, y = temp)) + 
  geom_point(col = "#EED8AE", alpha = 0.3, shape = 16) +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), col = "#8B7E66") +
  geom_line(data = present) +
  geom_point(data = record_high_low, aes(col = record)) +
  scale_color_identity()

# Finish the function draw_pop_legend
draw_pop_legend <- function(x = 0.6, y = 0.2, width = 0.2, height = 0.2, fontsize = 10) {
  
  # Finish viewport() function
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = "center"))
  
  legend_labels <- c("Past record high",
                     "95% CI range",
                     "Current year",
                     "Past years",
                     "Past record low")
  
  legend_position <- c(0.9, 0.7, 0.5, 0.2, 0.1)
  
  # Finish grid.text() function
  grid.text(label = legend_labels, x = 0.12, y = legend_position, 
            just = "left", 
            gp = gpar(fontsize = fontsize, col = "grey20"))
  
  # Position dots, rectangle and line
  point_position_y <- c(0.1, 0.2, 0.9)
  point_position_x <- rep(0.06, length(point_position_y))
  grid.points(x = point_position_x, y = point_position_y, pch = 16,
              gp = gpar(col = c("#0000CD", "#EED8AE", "#CD2626")))
  grid.rect(x = 0.06, y = 0.5, width = 0.06, height = 0.4,
            gp = gpar(col = NA, fill = "#8B7E66"))
  grid.lines(x = c(0.03, 0.09), y = c(0.5, 0.5),
             gp = gpar(col = "black", lwd = 3))
  
  # Add popViewport() for bookkeeping
  popViewport()
}

# Plotting object p, from previous exercise
p

# Call draw_pop_legend()
draw_pop_legend()
