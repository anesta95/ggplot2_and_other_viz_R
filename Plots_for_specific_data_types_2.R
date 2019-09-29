install.packages("maps")
install.packages("ggmap")
install.packages("mapproj")
install.packages("ggthemes")
install.packages("viridis")
install.packages("rgdal")
install.packages("animation")
install.packages("car")
install.packages("devtools")
library(devtools)
install_github("dgrtwo/gganimate")
library(car)
library(gganimate)
library(animation)
library(rgdal)
library(dplyr)
library(mapproj)
library(ggplot2)
library(maps)
library(ggmap)
library(ggthemes)
library(RColorBrewer)
library(viridis)
# maps, ggplot2, and ggmap are pre-loaded
# Use map_data() to create usa and inspect
usa <- map_data("usa")
str(usa)

# Build the map
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map() +
  theme_nothing()

cities_large <- read.csv('cities.csv')
cities_large <- cities_large %>% filter(State != 'Alaska' | State != 'Hawaii')

# usa, cities, and all required packages are available

# Finish plot 1
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_point(data = cities_large, aes(group = State, size = Pop_est),
             col = "red", shape = 16, alpha = 0.6) +
  coord_map() +
  theme_map()

# Arrange cities
cities_arr <- arrange(cities_large, Pop_est)

# Copy-paste plot 1 and adapt
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'grey90') +
  geom_point(data = cities_arr, aes(group = State, col = Pop_est), size = 2, shape = 16, alpha = 0.6) +
  coord_map() +
  theme_map() +
  scale_color_viridis()

pop <- read.csv('states.csv')

# pop and all required packages are available

# Use map_data() to create state
state <- map_data("state")

# Map of states
ggplot(state, aes(x = long, y = lat, fill = region, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_nothing()
names(pop) <- c("region", "Pop_est")
pop$region <- tolower(pop$region)
# Merge state and pop: state2
state2 <- merge(state, pop, all.x = T)
# Map of states with populations
ggplot(state2, aes(x = long, y = lat, fill = Pop_est, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_map()

# All required packages are available
# Import shape information: germany
germany <- readOGR(dsn = "shapes", layer = "DEU_adm1")

# fortify germany: bundes
bundes <- fortify(germany)

# Plot map of germany
ggplot(bundes, aes(x = long, y = lat, group = group)) +
  geom_polygon(col = "white", fill = "blue") +
  coord_map() +
  theme_nothing()

unemp <- read.csv("unemp.csv")

# germany, bundes and unemp are available

# re-add state names to bundes
bundes$state <- factor(as.numeric(bundes$id))
levels(bundes$state) <- germany$NAME_1

# Merge bundes and unemp: bundes_unemp
bundes_unemp <- merge(bundes, unemp)

# Update the ggplot call
ggplot(bundes_unemp, aes(x = long, y = lat, group = group, fill = unemployment)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_map()

?register_google
register_google(key = "Your key here")

london_map_13 <- get_map(location = "London, England", zoom = 13)
london_ton_13 <- get_map(location = "London, England", zoom = 13, source = "stamen", maptype = "toner")

# Create the map of London
ggmap(london_map_13)
# Create the second map of London
ggmap(london_ton_13)

london_sites <- c("Tower of London, London", "Buckingham Palace, London","Tower Bridge, London", "Westminster Abbey, London")

xx <- geocode(london_sites)

# Add a location column to xx
xx$location <- sub(", London", "", london_sites)

# Add a geom_points layer
ggmap(london_ton_13) +
  geom_point(data = xx, aes(col = location), size = 6)

# bundes and germany_06 are available, as are all required packages

# Plot map and polygon on top:
germany_06 <- get_map(location = "Germany", zoom = 6)
ggmap(germany_06)
ggmap(germany_06) +
  geom_polygon(data = bundes,
               aes(x = long, y = lat, group = group),
               fill = NA, col = "red") +
  coord_map()

japan <- read.table(file = 'japan.tsv', sep = '\t', header = TRUE)

# Inspect structure of japan
str(japan)

# Finish the code inside saveGIF
saveGIF({
  
  # Loop through all time points
  for (i in unique(japan$time)) {
    
    # Subset japan: data
    data <- subset(japan, time == i)
    
    # Finish the ggplot command
    p <- ggplot(data, aes(x = AGE, y = POP, fill = SEX, width = 1)) +
      coord_flip() +
      geom_bar(data = data[data$SEX == "Female",], stat = "identity") +
      geom_bar(data = data[data$SEX == "Male",], stat = "identity") +
      ggtitle(i)
    
    print(p)
    
  }
  
}, movie.name = "pyramid.gif", interval = 0.1)

data("Vocab")
# Vocab, gganimate and ggplot2 are available

# Update the static plot
p <- ggplot(Vocab, aes(x = education, y = vocabulary,
                       color = year, group = year,
                       frame = year, cumulative = T)) +
  stat_smooth(method = "lm", se = FALSE, size = 3)

# Call gg_animate on p
gg_animate(p, filename = "vocab.gif", interval = 1.0)