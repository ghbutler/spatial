us_south <- c('AL', 'AR', 'FL', 'GA', 'KY',
              'LA', 'MS', 'MO', 'NC', 'OK',
              'SC', 'TN', 'TX', 'VA', 'WV')

race_vars <- c(White = "B03002_003", Black = "B03002_004", 
               Native = "B03002_005", Asian = "B03002_006", 
               HIPI = "B03002_007", Hispanic = "B03002_012")

us_south_cts <- map(us_south, function(x) {
  counties(state = x, cb = TRUE)
}) %>% rbind_tigris()

us_south_cts %>% ggplot() + geom_sf() + theme_bw() + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        legend.position = c(0.8, 0.8), 
        legend.key = element_blank(), 
        legend.background = element_blank(), 
        legend.title = element_blank())

get_acs(geography = "county", 
        state = "CA", 
        variables = race_vars, 
        summary_var = "B03002_001")

us_south_dat <- map(us_south, function(x) {
  get_acs(geography = 'county',
          state = x,
          variables = race_vars,
          summary_var = 'B03002_001')
}) %>% rbind()

us_south_dat <- ldply(us_south_dat, data.frame)

south_race_pct <- dat %>% group_by(..GEOID) %>% filter(..estimate == max(estimate)) %>% select(..NAME, ..variable, ..estimate, ..moe)

us_south_cts %>% ggplot(aes(fill = south_race_pct$..variable)) + 
  geom_sf() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = c(0.64, 0.2), 
        legend.key = element_blank(), 
        legend.background = element_blank(), 
        legend.title = element_blank()) +
  ggtitle('Largest racial group in each county \nof the American south') +
  labs(caption = 'Source: 2013-2017 5 year ACS')