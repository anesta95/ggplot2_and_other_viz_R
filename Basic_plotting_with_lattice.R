install.packages("lattice")
install.packages("latticeExtra")
install.packages("dplyr")
library(latticeExtra)
data("airquality")
# The 'airquality' dataset is already available
str(airquality)

# Load the lattice package
library(lattice)

# Create the histogram 
histogram(~ Ozone, data = airquality)

# Create the scatter plot
xyplot(Ozone ~ Solar.R, data = airquality)

# Use the 'airquality' dataset
str(airquality)

# Create the histogram
histogram(~ Ozone, data = airquality, 
          # Specify number of bins
          nint = 15,
          # Specify quantity displayed on y-axis
          type = "count")

# Create scatterplot
xyplot(Ozone ~ Temp, data = airquality,
       # Add main label
       main = "Environmental conditions in New York City (1973)", 
       # Add axis labels
       xlab = "Temperature (Fahrenheit)",
       ylab = "Ozone (ppb)")

# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = "jitter")

data()
# Create reordered variable
library(dplyr)
USCancerRates <-
  mutate(USCancerRates, 
         state.ordered = reorder(state, rate.female, median, na.rm = TRUE))

# Create box and whisker plot
bwplot(state.ordered ~ rate.female, data = USCancerRates)

# Create box and whisker plot
bwplot(state.ordered ~ rate.female, data = USCancerRates, 
       # Change whiskers extent
       coef = 0)