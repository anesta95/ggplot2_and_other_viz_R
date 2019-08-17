library(lattice)
library(latticeExtra)

data("AirPassengers")

# The airquality dataset has been pre-loaded
str(airquality)

# Create a histogram
histogram(~ Ozone | factor(Month),
          data = airquality, 
          # Define the layout
          layout = c(2, 3),
          # Change the x-axis label
          xlab = "Ozone (ppb)")

data("USCancerRates")

# USCancerRates has been pre-loaded
str(USCancerRates)

# Create a density plot
densityplot(~ rate.male + rate.female,
            data = USCancerRates, 
            outer = TRUE,
            # Suppress data points
            plot.points = FALSE,
            # Add a reference line
            ref = TRUE)

# Create a density plot
densityplot(~ rate.male + rate.female,
            data = USCancerRates,
            # Set value of 'outer' 
            outer = F,
            # Add x-axis label
            xlab = "Rate (per 100,000)",
            # Add a legend
            auto.key = T,
            plot.points = FALSE,
            ref = TRUE)

#Use the built-in dataset state.division
data.frame(state.name, state.division)
index <- match(USCancerRates$state, state.name)
USCancerRates$division <- state.division[index]
summary(USCancerRates$division)
# Create 'division.ordered' by reordering levels
library(dplyr)
USCancerRates <- 
  mutate(USCancerRates, 
         division.ordered = reorder(division, 
                                    rate.male + rate.female, 
                                    mean, na.rm = TRUE))

# Create conditioned scatter plot
xyplot(rate.female ~ rate.male | division.ordered,
       data = USCancerRates, 
       # Add reference grid
       grid = T, 
       # Add reference line
       abline = c(0, 1))

# Levels of division.ordered
levels(USCancerRates$division.ordered)

# Specify the as.table argument 
xyplot(rate.female ~ rate.male | division.ordered,
       data = USCancerRates, 
       grid = TRUE, abline = c(0, 1),
       as.table = T)

# Create box-and-whisker plot
bwplot(division.ordered ~ rate.male + rate.female,
       data = USCancerRates, 
       outer = T, 
       # Add a label for the x-axis
       xlab = "Rate (per 100,000)",
       # Add strip labels
       strip = strip.custom(factor.levels = c("Male", "Female")))

# Create "trellis" object
tplot <-
  densityplot(~ rate.male + rate.female | division.ordered, 
              data = USCancerRates, outer = TRUE, 
              plot.points = FALSE, as.table = TRUE)

# Change names for the second dimension
dimnames(tplot)[[2]] <- c("Male", "Female")

# Update x-axis label and plot
update(tplot, xlab = "Rate")

# Create "trellis" object
tplot <-
  densityplot(~ rate.male + rate.female | division.ordered, 
              data = USCancerRates, outer = TRUE, 
              plot.points = FALSE, as.table = TRUE)

# Inspect dimension
dim(tplot)
dimnames(tplot)

# Select subset retaining only last three divisions
tplot[7:9, ]
