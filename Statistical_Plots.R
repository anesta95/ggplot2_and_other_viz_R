install.packages("ggplot2")
install.packages("ggplot2movies")
install.packages("Hmisc")
install.packages("RColorBrewer")
install.packages("tidyverse")
install.packages("viridis")
install.packages("ggtern")
library(tidyverse)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(ggplot2movies)
data('movies')
data('diamonds')
# Create movies_small
set.seed(123)
movies_small <- movies[sample(nrow(movies), 1000), ]
movies_small$rating <- factor(round(movies_small$rating))

# Explore movies_small with str()
str(movies_small)

# Build a scatter plot with mean and 95% CI
ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red") +
  scale_y_log10()


# Reproduce the plot
ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  geom_point(alpha = 0.5, size = 0.5, shape = 16) +
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1, 10)) +
  scale_y_log10(expression(log[10](Price)), limits = c(100, 100000)) +
  scale_color_brewer(palette = "YlOrRd") +
  coord_equal() +
  theme_classic()

# Add smooth layer and facet the plot
ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  stat_smooth(method = "lm") +
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1,10)) +
  scale_y_log10(expression(log[10](Price)), limits = c(100,100000)) +
  scale_color_brewer(palette = "YlOrRd") +
  coord_equal() +
  theme_classic()


# movies_small is available

# Add a boxplot geom
d <- ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red")

# Untransformed plot
d
# Transform the scale
d + scale_y_log10()
# Transform the coordinates
d + coord_trans(y = "log10")

small_data <- data.frame("x" = c(-3.5, 0, 0.5, 6.0))
small_data
typeof(small_data)


# small_data is available

# Get the bandwith
get_bw <- density(small_data$x)$bw

# Basic plotting object
p <- ggplot(small_data, aes(x = x)) +
  geom_rug() +
  coord_cartesian(ylim = c(0,0.5))

# Create three plots
p + geom_density()
p + geom_density(adjust = 0.25)
p + geom_density(bw = (0.25 * get_bw))

# Create two plots
p + geom_density(kernel = "r")
p + geom_density(kernel = "e")

# Box plot with varying width
# Finish the plot
ggplot(diamonds, aes(x = cut, y = price, col = color)) + geom_boxplot(varwidth = TRUE) + facet_grid(. ~ color)

data("msleep")
head(msleep) 
mammals <- msleep[c(3, 6)]


# Individual densities
ggplot(mammals[mammals$vore == "Insectivore", ], aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

# With faceting
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3)) +
  facet_wrap( ~ vore, nrow = 2)

# Note that by default, the x ranges fill the scale
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

# Trim each density plot individually
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35, trim = TRUE) +
  scale_x_continuous(limits=c(0,24)) +
  coord_cartesian(ylim = c(0, 0.3))


# Unweighted density plot from before
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

# Unweighted violin plot
ggplot(mammals, aes(x = vore, y = sleep_total, fill = vore)) +
  geom_violin()

# Calculate weighting measure
library(dplyr)
mammals2 <- mammals %>%
  group_by(vore) %>%
  mutate(n = n() / nrow(mammals)) -> mammals

# Weighted density plot
ggplot(mammals2, aes(x = sleep_total, fill = vore)) +
  geom_density(aes(weight = n), col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

# Weighted violin plot
ggplot(mammals2, aes(x = vore, y = sleep_total, fill = vore)) +
  geom_violin(aes(weight = n), col = NA)

data("faithful")

# Base layers
p2 <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0, 0)) +
  coord_fixed(60 / 4.5)

# 1 - Use geom_density_2d()
p2 + geom_density_2d()

# 2 - Use stat_density_2d() with arguments
p2 + stat_density_2d(aes(col = ..level..), h = c(5, 0.5))

# Load in the viridis package
library(viridis)

# Add viridis color scale
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0,0)) +
  coord_fixed(60/4.5) +
  stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(5,.5), contour = FALSE) + scale_fill_viridis()






