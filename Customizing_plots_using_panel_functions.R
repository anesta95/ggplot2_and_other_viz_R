library(lattice)
library(latticeExtra)

data("airquality")

panel.xyrug <- function(x, y, ...)
{
  # Reproduce standard scatter plot
  panel.xyplot(x, y, ...)
  
  # Identify observations with x-value missing
  x.missing <- is.na(x)
  
  # Identify observations with y-value missing
  y.missing <- is.na(y)
  
  # Draw rugs along axes
  panel.rug(x = x[y.missing], y = y[x.missing])
}

airquality$Month.Name <- 
  factor(month.name[airquality$Month], levels = month.name)

xyplot(Ozone ~ Solar.R | Month.Name, data = airquality,
       panel = panel.xyrug, as.table = TRUE)

# Create factor variable with month names
airquality$Month.Name <- 
  factor(month.name[airquality$Month], levels = month.name)

# Create box-and-whisker plot
bwplot(Month.Name ~ Ozone + Temp, airquality, 
       # Specify outer
       outer = T, 
       # Specify x-axis relation
       scales = list(x = list(relation = "free")),
       # Specify layout
       layout = c(2, 1),
       # Specify x-axis label
       xlab = "Measured value")

# Create violin plot
bwplot(Month.Name ~ Ozone + Temp, airquality, 
       # Specify outer
       outer = TRUE, 
       # Specify x-axis relation
       scales = list(x = list(relation = "free")),
       # Specify layout
       layout = c(2, 1),
       # Specify x-axis label
       xlab = "Measured value",
       # Replace default panel function
       panel = panel.violin)

data("USCancerRates")

# Create panel function
panel.ss <- function(x, y, ...) {
  # Call panel.smoothScatter()
  panel.smoothScatter(x, y, ...)
  # Call panel.loess()
  panel.loess(x, y, col = "red")
  # Call panel.abline()
  panel.abline(0, 1)
}

# Create plot
xyplot(rate.female ~ rate.male, data = USCancerRates,
       panel = panel.ss,
       main = "County-wise deaths due to cancer")

panel.histdens <- function(x, ...) {
  panel.histogram(x, ...)
  panel.lines(density(x, na.rm = TRUE))
}

# Define prepanel function
prepanel.histdens.2 <- function(x, ...) {
  h <- prepanel.default.histogram(x, ...)
  d <- density(x, na.rm = TRUE)
  list(xlim = quantile(x, c(0.005, 0.995), na.rm = TRUE),
       # Calculate upper y-limit
       ylim = c(0, max(d$y, h$ylim[2])))
}

# Create a histogram of rate.male and rate.female
histogram(~ rate.male + rate.female,
          data = USCancerRates, outer = TRUE,
          type = "density", nint = 50,
          border = "transparent", col = "lightblue",
          # The panel function: panel.histdens
          panel = panel.histdens, 
          # The prepanel function: prepanel.histdens.2
          prepanel = prepanel.histdens.2,
          # Ensure that the x-axis is log-transformed
          # and has relation "sliced"
          scales = list(x = list(log = T,
                                 equispaced.log = FALSE,
                                 relation = "sliced")),
          xlab = "Rate (per 100,000)")

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

# Create the box and whisker plot
bwplot(division.ordered ~ rate.male, 
       data = USCancerRates,
       # Indicate median by line instead of dot
       pch = "|", 
       # Include notches for confidence interval
       notch = T,
       # The x-axis should plot log-transformed values
       scales = list(x = list(log = T, equispaced.log = F)),
       xlab = "Death Rate in Males (per 100,000)")

# Create summary dataset
USCancerRates.state <- 
  with(USCancerRates, {
    rmale <- tapply(rate.male, state, median, na.rm = TRUE)
    rfemale <- tapply(rate.female, state, median, na.rm = TRUE)
    data.frame(Rate = c(rmale, rfemale),
               State = rep(names(rmale), 2),
               Gender = rep(c("Male", "Female"), 
                            each = length(rmale)))
  })

# Create summary dataset
USCancerRates.state <- 
  with(USCancerRates, {
    rmale <- tapply(rate.male, state, median, na.rm = TRUE)
    rfemale <- tapply(rate.female, state, median, na.rm = TRUE)
    data.frame(Rate = c(rmale, rfemale),
               State = rep(names(rmale), 2),
               Gender = rep(c("Male", "Female"), 
                            each = length(rmale)))
  })

# Reorder levels
library(dplyr)
USCancerRates.state <- 
  mutate(USCancerRates.state, State = reorder(State, Rate))
head(USCancerRates.state)

panel.xyimage <- function(x, y, 
         subscripts,
         groups = NULL,
         pch = NULL,
         cex = 1,
         ...,
         grid = FALSE, abline = NULL)
{
  if (all(is.na(x) | is.na(y))) return()
  if (!is.character(pch))
    stop("'pch' must be a character vector giving path(s) or URL(s) of PNG or JPEG files.")
  if (!identical(grid, FALSE))
  {
    if (!is.list(grid))
      grid <- switch(as.character(grid),
                     "TRUE" = list(h = -1, v = -1, x = x, y = y),
                     "h" = list(h = -1, v = 0, y = y),
                     "v" = list(h = 0, v = -1, x = x),
                     list(h = 0, v = 0))
    do.call(panel.grid, grid)
  }
  if (!is.null(abline))
  {
    if (is.numeric(abline)) abline <- as.list(abline)
    do.call(panel.abline, abline)
  }
  if (is.null(groups))
  {
    pch.raster <- url2raster(pch[1])
    grid.raster(x, y, image = pch.raster,
                width = unit(cex * 10, "mm"),
                height = unit(cex * 10, "mm"),
                default.units = "native")
  }
  else
  {
    groups <- as.factor(groups)
    cex <- rep(cex, length = nlevels(groups))
    pch <- rep(pch, length = nlevels(groups))
    pch.raster <- lapply(pch, url2raster)
    g <- as.numeric(groups)[subscripts]
    ug <- unique(g)
    for (i in ug)
    {
      w <- (g == i)
      grid.raster(x[w], y[w], image = pch.raster[[i]],
                  width = unit(cex[i] * 8, "mm"),
                  height = unit(cex[i] * 8, "mm"),
                  default.units = "native")
    }
  }
}

# URLs for emojis
emoji.man <- "https://twemoji.maxcdn.com/72x72/1f468.png"
emoji.woman <- "https://twemoji.maxcdn.com/72x72/1f469.png"

# Create dotplot
dotplot(State ~ Rate, data = USCancerRates.state, 
        # Specify grouping variable
        groups = Gender, 
        # Specify panel function
        panel = panel.xyimage, 
        # Specify emoji URLs
        pch = c(emoji.woman, emoji.man),
        # Make symbols smaller
        cex = 0.75)


