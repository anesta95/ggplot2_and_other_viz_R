library(lattice)
library(dplyr)
library(latticeExtra)
library(RColorBrewer)
data("USMortality")

USMortality %>%
  group_by(Cause) %>%
  summarize(min = min(Rate)) %>%
  arrange(desc(min))

# Specify upper bound to exclude Heart disease and Cancer
x_limits <- c(0, 130)

# Draw the plot
dotplot(Cause ~ Rate | Sex + Status, data = USMortality, as.table = TRUE, 
        xlim = x_limits)

dotplot(Cause ~ Rate | Sex + Status, data = USMortality,
        as.table = TRUE,
        scales = list(x = list(relation = "free",
                               # Specify limits for each panel
                               limits = list(c(0, 50), c(0, 80), 
                                             c(0, 50), c(0, 80)))))
dotplot(Cause ~ Rate | Sex + Status, data = USMortality, 
        as.table = TRUE,
        # Change the number of tick marks
        scales = list(x = list(tick.number = 10, 
                               # Show `Rate` labels on both bottom and top
                               alternating = 3, 
                               # Rotate `Rate` labels by 90 degrees
                               rot = 90),
                      # Rotate `Cause` labels by 45 degrees
                      y = list(rot = 45)))

# Define at as 2^3 up to 2^8
x_ticks_at <- c(2^3, 2^4, 2^5, 2^6, 2^7, 2^8)

dotplot(Cause ~ Rate | Sex, data = USMortality,
        groups = Status, auto.key = list(columns = 2),
        scales = list(x = list(log = 2, 
                               # A numeric vector with 
                               # values 2^3, 2^4, ..., 2^8
                               at = x_ticks_at, 
                               # A character vector, 
                               # "8" for 2^3, "16" for 2^4, etc.
                               labels = x_ticks_at)))

data("WorldPhones")
WorldPhones

names(dimnames(WorldPhones)) <- c("Year", "Region")

# Transform matrix data to data frame
WorldPhonesDF <- as.data.frame(
  # Intermediate step: convert to table
  as.table(WorldPhones), 
  responseName = "Phones")

# Create the dot plot
dotplot(Year ~ Phones | Region, 
        data = WorldPhonesDF, 
        as.table = TRUE,
        # Log-transform the x-axis
        scales = list(x = list(log = T,
                               equispaced.log = FALSE, 
                               # Set x-axis relation to "sliced"
                               relation = "sliced")))

# Transform matrix data to data frame
names(dimnames(WorldPhones)) <- c("Year", "Region")
WorldPhonesDF <- 
  as.data.frame(as.table(WorldPhones[-1, ]), 
                responseName = "Phones")

# Create the dot plot
dotplot(Year ~ Phones | Region,
        data = WorldPhonesDF, 
        as.table = TRUE,
        scales = list(x = list(log = TRUE,
                               equispaced.log = FALSE, 
                               relation = "sliced")),
        # Fill in suitable value of par.settings
        par.settings = ggplot2like(),
        # Fill in suitable value of lattice.options
        lattice.options = ggplot2like.opts())

data("airquality")

# Create factor variable
airquality$Month.Name <- 
  factor(airquality$Month, levels = 1:12, 
         labels = month.name)

# Create histogram of Ozone, conditioning on Month
histogram(~ Ozone | Month.Name,
          data = airquality, as.table = TRUE,
          # Set border to be transparent
          border = "transparent", 
          # Set fill color to be mid-gray
          col = "grey50")

# Create the dot plot
dotplot(Cause ~ Rate | Status, data = USMortality,
        groups = Sex, auto.key = list(columns = 2),
        scales = list(x = list(log = TRUE, 
                               equispaced.log = FALSE)), 
        # Provide pch values for the two groups
        pch = c(3, 1))

dotplot(Cause ~ Rate | Status, data = USMortality,
        groups = Sex, auto.key = list(columns = 2),
        par.settings = simpleTheme(pch = c(3, 1)),
        scales = list(x = list(log = 2, equispaced.log = FALSE)))


# Create factor variable
airquality$Month.Name <- 
  factor(airquality$Month, levels = 1:12, 
         labels = month.name)
levels(airquality$Month.Name)

# Drop empty levels
airquality$Month.Name <- droplevels(airquality$Month.Name)
levels(airquality$Month.Name)

# Obtain colors from RColorBrewer
my.colors <- brewer.pal(n = 5, name = "Set1")

# Density plot of ozone concentration grouped by month
densityplot(~ Ozone, data = airquality, groups = Month.Name,
            plot.points = FALSE,
            auto.key = list(space = "right"),
            # Fill in value of col
            par.settings = simpleTheme(col = my.colors, 
                                       # Fill in value of lwd
                                       lwd = 2))