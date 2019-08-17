install.packages("maps")
install.packages("mapproj")
install.packages("hexbin")
install.packages("directlabels")
library(directlabels)
library(hexbin)
library(mapproj)
library(maps)
library(lattice)
library(latticeExtra)

data("EuStockMarkets")
# Use 'EuStockMarkets' time series data
str(EuStockMarkets)

# Create time series plot
xyplot(EuStockMarkets, 
       # Plot all series together
       superpose = T,
       # Split up the time axis into parts
       cut = list(number = 3, overlap = 0.25))

# Create time series plot
xyplot(EuStockMarkets,
       # Specify panel function
       panel = panel.horizonplot,
       # Specify prepanel function
       prepanel = prepanel.horizonplot)

data("USCancerRates")

# Create map object for US counties
county.map <- map("county", plot = FALSE, fill = T, 
                  # Specify projection
                  projection = "sinusoidal")

# Create choropleth map
mapplot(rownames(USCancerRates) ~ log10(rate.male) + log10(rate.female), 
        data = USCancerRates, 
        xlab = "", scales = list(draw = FALSE),
        # Specify map
        map = county.map)

# Create subset for Louisiana
LACancerRates1 <- filter(USCancerRates, state == "Louisiana")
str(LACancerRates1)

# Reorder levels of county
LACancerRates2 <- 
  mutate(LACancerRates1, 
         county = reorder(county, rate.male))

# Draw confidence intervals
segplot(county ~ LCL95.male + UCL95.male,
        data = LACancerRates2,
        # Add point estimates
        centers = rate.male,
        # Draw segments rather than bands
        draw.bands = F)

# Create hexbin plot
hexbinplot(rate.female ~ rate.male, 
           data = USCancerRates, 
           # Add a regression line
           type = "r",
           # function to transform counts
           trans = sqrt,
           # function to invert transformed counts
           inv = function(x) x^2)
data("airquality")

# Use the 'airquality' dataset
str(airquality)

# Create factor variable
airquality$Month.Name <- 
  factor(month.name[airquality$Month], levels = month.name)

# Create density plot object
tplot2 <- 
  densityplot(~ Ozone + Temp, data = airquality, 
              # Variables should go in different panels
              outer = T,
              # Specify grouping variable
              groups = Month.Name,
              # Suppress display of data points
              plot.points = F, 
              # Add reference line
              ref = T,
              # Specify layout
              layout = c(2, 1),
              # Omit strip labels
              strip = F,
              # Provide column-specific x-axis labels
              xlab = c("Ozone (ppb)", "Temperature (F)"),
              # Let panels have independent scales 
              scales = list(relation = "free"))

# Produce plot with direct labels
direct.label(tplot2)

# 'USCancerRates' is pre-loaded
str(USCancerRates)

# Create scatter plot
p <- xyplot(rate.female ~ rate.male, data = USCancerRates, 
            # Change plotting character
            pch = 16, 
            # Make points semi-transparent
            alpha = 0.25)

# Create layer with reference grid
l0 <- layer_(panel.grid())

# Create layer with reference line
l1 <- layer(panel.abline(0, 1))

# Create layer with regression fit
l2 <- layer(panel.smoother(x, y, method = "lm"))

# Combine and plot
p + l0 + l1 + l2