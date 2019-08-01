library(ggplot)
who_disease <- read.csv('WHO_disease.csv')

who_disease %>% 
  # filter to india in 1980
  filter(country == "India", year == 1980) %>% 
  # map x aesthetic to disease and y to cases
  ggplot(aes(x = disease, y = cases)) +
  # use geom_col to draw
  geom_col()

who_disease %>%
  # filter data to observations of greater than 1,000 cases
  filter(cases > 1000) %>%
  # map the x-axis to the region column
  ggplot(aes(x = region)) +
  # add a geom_bar call
  geom_bar()

interestingCountries <- c("NGA", "SDN", "FRA", "NPL", "MYS", "TZA", "YEM", "UKR", "BGD", "VNM")

who_subset <- who_disease %>% 
  filter(
    countryCode %in% interestingCountries,
    disease == 'measles',
    year %in% c(1992, 2002) # Modify years to 1992 and 2002
  ) %>% 
  mutate(year = paste0('cases_', year)) %>% 
  spread(year, cases)

# Reorder y axis and change the cases year to 1992
ggplot(who_subset, aes(x = log10(cases_1992), y = reorder(country, cases_1992))) +
  geom_point()

who_subset %>% 
  # calculate the log fold change between 2016 and 2006
  mutate(logFoldChange = log2(cases_2002/cases_1992)) %>% 
  # set y axis as country ordered with respect to logFoldChange
  ggplot(aes(x = logFoldChange, y = reorder(country, logFoldChange))) +
  geom_point() +
  # add a visual anchor at x = 0
  geom_vline(xintercept = 0)


who_subset %>% 
  mutate(logFoldChange = log2(cases_2002/cases_1992)) %>% 
  ggplot(aes(x = logFoldChange, y = reorder(country, logFoldChange))) +
  geom_point() +
  geom_vline(xintercept = 0) +
  xlim(-6,6) +
  # add facet_grid arranged in the column direction by region and free_y scales
  facet_grid(region ~ ., scale = 'free_y')

amr_pertussis <- who_disease %>% 
  filter(   # filter data to our desired subset
    region == 'AMR', 
    year == 1980, 
    disease == 'pertussis'
  )
# Set x axis as country ordered with respect to cases. 
ggplot(amr_pertussis, aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  # flip axes
  coord_flip()

amr_pertussis %>% 
  # filter to countries that had > 0 cases. 
  filter(cases > 0) %>%
  ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  theme(
    # get rid of the 'major' y grid lines
    panel.grid.major.y = element_blank()
  )

amr_pertussis %>% filter(cases > 0) %>% 
  ggplot(aes(x = reorder(country, cases), y = cases)) + 
  # switch geometry to points and set point size = 2
  geom_point(size = 2) + 
  # change y-axis to log10. 
  scale_y_log10() +
  # add theme_minimal()
  theme_minimal() +
  coord_flip()
