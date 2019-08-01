library(ggplot2)
install.packages("ggbeeswarm")
install.packages("ggridges")
library(ggridges)
library(ggbeeswarm)
md_speeding <- read.csv('Traffic_Violations.csv')

#A simple boxplot
md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  # Map x and y to gender and speed columns respectively
  ggplot(aes(x = gender, y = speed)) + 
  # add a boxplot geometry
  geom_boxplot() +
  # give plot supplied title
  labs(title = 'Speed of red cars by gender of driver')

#Adding some jitter
md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  ggplot(aes(x = gender, y = speed)) + 
  # add jittered points with alpha of 0.3 and color 'steelblue'
  geom_jitter(alpha = 0.3, color = 'steelblue') +
  # make boxplot transparent with alpha = 0
  geom_boxplot(alpha = 0) +
  labs(title = 'Speed of red cars by gender of driver')

#Faceting to show all colors
# remove color filter
md_speeding %>%
  ggplot(aes(x = gender, y = speed)) + 
  geom_jitter(alpha = 0.3, color = 'steelblue') +
  geom_boxplot(alpha = 0) +
  # add a facet_wrap by vehicle_color
  facet_wrap(~vehicle_color) +
  # change title to reflect new faceting
  labs(title = 'Speed of different car colors, separated by gender of driver')

#First beeswarm
md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  ggplot(aes(x = gender, y = speed)) + 
  # change point size to 0.5 and alpha to 0.8
  geom_beeswarm(cex = 0.5, alpha = 0.8) +
  # add a transparent boxplot on top of points
  geom_boxplot(alpha = 0)


#Fiddling with a violin plot
md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  ggplot(aes(x = gender, y = speed)) + 
  # Replace beeswarm geometry with a violin geometry with kernel width of 2.5
  geom_violin(bw = 2.5, fill = 'steelblue') +
  # add individual points on top of violins
  geom_point(alpha = 0.3, size = 0.5)

#Violins with boxplots
md_speeding %>% 
  filter(vehicle_color == 'RED') %>%
  ggplot(aes(x = gender, y = speed)) + 
  geom_violin(bw = 2.5) +
  # add a transparent boxplot and shrink its width to 0.3
  geom_boxplot(alpha = 0, width = 0.3) +
  # Reset point size to default and set point shape to 95
  geom_point(alpha = 0.3, shape = 95) +
  # Supply a subtitle detailing the kernel width
  labs(subtitle = 'Gaussian kernel SD = 2.5')

#Comparing lots of distributions
md_speeding %>% 
  ggplot(aes(x = gender, y = speed)) + 
  # replace with violin plot with kernel width of 2.5, change color argument to fill 
  geom_violin(bw = 2.5, fill = 'steelblue') +
  # reduce width to 0.3
  geom_boxplot(alpha = 0, width = 0.3) +
  facet_wrap(~vehicle_color) +
  labs(
    title = 'Speed of different car colors, separated by gender of driver',
    # add a subtitle w/ kernel width
    subtitle = 'Gaussian kernel width: 2.5'
  )
md_speeding$day_of_week <- weekdays(as.Date(md_speeding$Date_Of_Stop, format="%m/%d/%y")) 
md_speeding$day_of_week <- as.factor(md_speeding$day_of_week)
#A basic ridgline plot
md_speeding %>% 
  ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
  # Set bandwidth to 3.5
  geom_density_ridges(bandwidth = 3.5) +
  # add limits of 0 to 150 to x-scale
  scale_x_continuous(limits=c(0, 150)) +
  # provide subtitle with bandwidth
  labs(subtitle = 'Gaussian kernel SD = 3.5')


#Cleaning up your ridgelines
md_speeding %>% 
  ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
  # make ridgeline densities a bit see-through with alpha = 0.7
  geom_density_ridges(bandwidth = 3.5, alpha=0.7) +
  # set expand values to c(0,0)
  scale_x_continuous(limits = c(0,150), expand=c(0,0)) +
  labs(subtitle = 'Guassian kernel SD = 3.5') +
  # remove y axis ticks
  theme(axis.ticks.y = element_blank())

#Raincloud plot
md_speeding %>% 
  ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
  geom_point(alpha = 0.2, shape = "|", position = position_nudge(y = -0.05)
             # make semi-transparent with alpha = 0.2
             # turn points to vertical lines with shape = '|'
             # nudge the points downward by 0.05
  ) +
  geom_density_ridges(bandwidth = 3.5, alpha = 0.7) +
  scale_x_continuous(limits = c(0,150), expand  = c(0,0)) +
  labs(subtitle = 'Guassian kernel SD = 3.5') +
  theme( axis.ticks.y = element_blank() )
