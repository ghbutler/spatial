library(tigris)
options(tigris_class = "sf")

library(tidyverse)
library(tidycensus)

race_vars <- c(White = "B03002_003", Black = "B03002_004", 
               Native = "B03002_005", Asian = "B03002_006", 
               HIPI = "B03002_007", Hispanic = "B03002_012")

ca_race <- get_acs(geography = "county", 
                   state = "CA", 
                   variables = race_vars, 
                   summary_var = "B03002_001")

ca_race_pct <- ca_race %>%
  mutate(pct = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, pct)

ca_largest <- ca_race %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate)) %>%
  select(NAME, variable, estimate)

ca_largest_pct <- ca_race_pct %>%
  group_by(NAME) %>%
  filter(pct == max(pct))

ca_largest <- inner_join(ca_largest, ca_largest_pct)

ca_largest <- merge(ca_largest, ca_race)

ca_largest <- ca_largest %>% mutate(NAME = str_replace(NAME, ' County, California', ''))

ca_largest %>% group_by(variable) %>% tally()

ca_tiger <- counties(state = 'CA')
ca_cb <- counties(state = 'CA', cb = TRUE)
ca_cbsf <- counties(state = 'CA', cb = TRUE)

sd_tracts_tiger <- tracts(state = 'CA', county = 'San Diego')
sd_tracts_cb <- tracts(state = 'CA', county = 'San Diego', cb = TRUE)
sd_tracts_cbsf <- tracts(state = 'CA', county = 'San Diego', cb = TRUE)

#Try to use this data to make plots of top ethnic groups in SD census tracts
sd90 <- tracts(state = 'CA', county = 'San Diego', 
               cb = TRUE, year = 1990)
sd16 <- tracts(state = "CA", county = 'San Diego', 
               cb = TRUE, year = 2016)

ca_cbsf %>% ggplot(aes(fill = ca_largest$variable)) + 
  geom_sf() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Largest racial group in each county') +
  labs(caption = 'Source: 2013-2017 5 year ACS')

ca_largest %>% ggplot(aes(x = pct / 100, 
                          y = reorder(NAME, pct))) + 
  geom_point(aes(color = as.factor(ca_largest$variable))) + 
  theme_bw() + 
  scale_x_continuous(labels = scales::percent_format(trim = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.15),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  ylab('County') + xlab('Percentage of population') + 
  ggtitle('Largest racial group in each California county') +
  labs(caption = 'Source: 2013-2017 5 year ACS')