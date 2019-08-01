library(ggplot2)
library(RColorBrewer)
data("mtcars")
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Basic ggplot() command, coded for you
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + geom_point() + geom_smooth()

# Add scale_x_continuous()
p + scale_x_continuous(limits = c(3, 6), expand = c(0, 0))

# Add coord_cartesian(): the proper way to zoom in
p + coord_cartesian(xlim = c(3, 6))

data("iris")

# Complete basic scatter plot function
base.plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

# Plot base.plot: default aspect ratio
base.plot

# Fix aspect ratio (1:1) of base.plot
base.plot + coord_equal()

# Create a stacked bar plot: wide.bar
wide.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) + geom_bar()
wide.bar
# Convert wide.bar to pie chart
wide.bar +
  coord_polar(theta = "y")

# Create stacked bar plot: thin.bar
thin.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar(width = 0.1) +
  scale_x_continuous(limits = c(0.5, 1.5))

# Convert thin.bar to "ring" type pie chart
thin.bar + 
  coord_polar(theta = "y")

# Basic scatter plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# 1 - Separate rows according to transmission type, am
p +
  facet_grid(am ~ .)

# 2 - Separate columns according to cylinders, cyl
p +
  facet_grid(. ~ cyl)

# 3 - Separate by both columns and rows 
p +
  facet_grid(am ~ cyl)

# Code to create the cyl_am col and myCol vector
mtcars$cyl_am <- paste(mtcars$cyl, mtcars$am, sep = "_")
myCol <- rbind(brewer.pal(9, "Blues")[c(3,6,8)],
               brewer.pal(9, "Reds")[c(3,6,8)])

# Map cyl_am onto col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() +
  # Add a manual colour scale
  scale_color_manual(values = myCol)


# Grid facet on gear vs. vs
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() + scale_color_manual(values = myCol) + facet_grid(gear ~ vs)





# Also map disp to size
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am, size = disp)) +
  geom_point() + scale_color_manual(values = myCol) + facet_grid(gear ~ vs)

data("msleep")
str(msleep)

# Basic scatter plot
p2 <- ggplot(msleep, aes(x = time, y = name, col = sleep_total)) +
  geom_point()

# Execute to display plot
p2

# Facet rows accoding to vore
p2 +
  facet_grid(vore ~ .)

# Specify scale and space arguments to free up rows
p2 +
  facet_grid(vore ~ ., scale = "free_y", space = "free_y")

