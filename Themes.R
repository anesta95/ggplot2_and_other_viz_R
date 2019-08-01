install.packages("ggthemes")
library(ggplot2)
library(RColorBrewer)
data("mtcars")

str(mtcars)
my_blue = brewer.pal(n = 9, "Blues")[4:9]
z <- ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point() + facet_grid(. ~ cyl) + geom_smooth(method = "lm", se = FALSE) + xlab("Weight (lb/1000)") + ylab("Miles/(US) gallon") + scale_color_manual(values = my_blue) + labs(color = "Cylinders")
?scale_color_brewer
?labs
?rescale()
myPink <- "#FEE0D2"

# Starting point
z

# Plot 1: Change the plot background fill to myPink
z +
  theme(plot.background = element_rect(fill = myPink))

# Plot 2: Adjust the border to be a black line of size 3
z +
  theme(plot.background = element_rect(fill = myPink, color = "black", size = 3)) # expanded from plot 1

# Theme to remove all rectangles
no_panels <- theme(rect = element_blank())

# Plot 3: Combine custom themes
z2 <- z +
  no_panels +
  theme(plot.background = element_rect(fill = myPink, color = "black", size = 3)) # from plot 2

# Extend z using theme() function and 3 args
z3 <- z2 + theme(panel.grid = element_blank(), axis.line = element_line(color = "red"), axis.ticks = element_line(color = "red"))
myRed <- "#de2d26"

# Extend z with theme() function and 3 args
z3 +
  theme(strip.text = element_text(size = 16, color = myRed),
        axis.title = element_text(color = myRed, hjust = 0, face = "italic"),
        axis.text = element_text(color = "black"))


# Move legend by position
z3 +
  theme(legend.position = c(0.85, 0.85))

# Change direction
z3 +
  theme(legend.direction = "horizontal")

# Change location by name
z3 +
  theme(legend.position = "bottom")

# Remove legend entirely
z3 +
  theme(legend.position = "none")


library(grid)
# Increase spacing between facets
z3 + theme(panel.spacing.x = unit(2, "cm"))


# Adjust the plot margin
z3 + theme(panel.spacing.x = unit(2, "cm"), plot.margin = unit(c(1, 2, 1, 1), "cm"))

# Original plot
z

# Theme layer saved as an object, theme_pink
theme_pink <- theme(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = myPink, color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "red"),
                    axis.ticks = element_line(color = "red"),
                    strip.text = element_text(size = 16, color = myRed),
                    axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")

# 1 - Apply theme_pink to z
z +
  theme_pink

# 2 - Update the default theme, and at the same time
# assign the old theme to the object old.
old <- theme_update(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = myPink, color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "red"),
                    axis.ticks = element_line(color = "red"),
                    strip.text = element_text(size = 16, color = myRed),
                    axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")

# 3 - Display the plot z - new default theme used
z

# 4 - Restore the old default theme
theme_set(old)

# Display the plot z - old theme restored
z

# Original plot
z

# Load ggthemes
library(ggthemes)

# Apply theme_tufte(), plot additional modifications
custom_theme <- theme_tufte() +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_text(face = "italic", size = 12),
        axis.title = element_text(face = "bold", size = 14))

# Draw the customized plot
z + custom_theme

# Use theme set to set custom theme as default
theme_set(custom_theme)

# Plot z again
z

