install.packages("GGally")
library(GGally)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(lattice)
data("mtcars")

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Base layers
m <- ggplot(mtcars, aes(x = cyl, y = wt))

# Draw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)


# Base layers
m2 <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am))


# Plot 1: Draw dynamite plot
m2 +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Plot 2: Set position dodge in each stat function
m2 +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.1, position = "dodge")

# Set your dodge posn manually
posn.d <- position_dodge(0.9)

# Plot 3: Redraw dynamite plot
m2 +
  stat_summary(fun.y = mean, geom = "bar", position = posn.d) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = posn.d)

mtcars.cyl <- data.frame("cyl" = c(4, 6, 8), "wt.avg" = c(2.285727, 3.117143, 3.999214), "sd" = c(0.5695637, 0.3563455, 0.7594047), n = c(11, 7, 14), "prop" = c(0.34375, 0.24875, 0.43750))


# Base layers
m3 <- ggplot(mtcars.cyl, aes(x = cyl, y = wt.avg))

# Plot 1: Draw bar plot with geom_bar
m3 + geom_bar(stat = "identity", fill = "skyblue")

# Plot 2: Draw bar plot with geom_col
m3 + geom_col(fill = "skyblue")

# Plot 3: geom_col with variable widths.
m3 + geom_col(width = mtcars.cyl$prop, fill = "skyblue")

# Plot 4: Add error bars
m3 + 
  geom_col(width = mtcars.cyl$prop, fill = "skyblue") +
  geom_errorbar(aes(ymin = wt.avg - sd, ymax = wt.avg + sd), width = 0.1)



# Bar chart
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill")

# Convert bar chart to pie chart
ggplot(mtcars, aes(x = factor(1), fill = am)) +
  geom_bar(position = "fill", width = 1) +
  facet_grid(. ~ cyl) + # Facets
  coord_polar(theta = "y") + # Coordinates
  theme_void() # theme

# All columns except am
group_by_am <- 9
my_names_am <- (1:11)[-group_by_am]

# Basic parallel plot - each variable plotted as a z-score transformation
ggparcoord(mtcars, my_names_am, groupColumn = group_by_am, alpha = 0.8)

data("barley")
str(barley)

# Create color palette
myColors <- brewer.pal(9, "Reds")

# Build the heat map from scratch
ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() + # Geom layer
  facet_wrap( ~ site, ncol = 1) + # Facet layer
  scale_fill_gradientn(colors = myColors) # Adjust colors

# Line plot; set the aes, geom and facet
ggplot(barley, aes(x = year, y = yield, col = variety, group = variety))
+ geom_line() + facet_wrap(~ site, nrow = 1)

# Create overlapping ribbon plot from scratch
ggplot(barley, aes(x = year, y = yield, col = site, group = site, fill = site)) + stat_summary(fun.y = mean, geom = "line") + stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", col = NA, alpha = 0.1)
