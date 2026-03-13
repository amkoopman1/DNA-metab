## body condition between groups and years

# metadata samples Wender
database_link <- "https://docs.google.com/spreadsheets/d/1ocuMq1ENjgwsVZ4uvhgyqwzb7D1d6v8HvuZTSrQ3kY0/edit"
database <- read_gsdb(database_link)
names(database)




# adult captures data
var_data <- database[["Captures"]] %>% select(Date, Species, Sexe, Age, Wing, Tarsus, Weight, Fat, Feces) %>%
  mutate(Weight = as.numeric(Weight),
         Wing = as.numeric(Wing),
         Tarsus = as.numeric(Tarsus),
         Fat = as.numeric(Fat),
         Feces = str_sub(Feces, -6, -1),
         day_of_year = yday(Date),  # Day of year (1-365)
         days10 = ceiling((day_of_year -6) / 10), # the + changes the window of time
         days21 = ceiling((day_of_year -6) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)
         )%>%
  filter(str_sub(Feces, 1, 1) == "A" # make sure sample name is correct
  )

# for nestlings    

var_nest_data <- database[["Nestlings"]] %>% select(Date, Species, Tarsus, Weight, Wing, Feces) %>%
  mutate(Weight = as.numeric(Weight),
         Wing = as.numeric(Wing),
         Tarsus = as.numeric(Tarsus),
         Feces = str_sub(Feces, -6, -1),
         day_of_year = yday(Date),  # Day of year (1-365)
         days10 = ceiling((day_of_year -6) / 10), # the + changes the window of time
         days21 = ceiling((day_of_year -6) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)
  )%>%
  filter(str_sub(Feces, 1, 1) == "A" # make sure sample name is correct
  )




# mean variables

# choose between Weight, Wing, Tarsus, Fat (only for adult)
input_var <- "Fat"
# choose between days10, days21 and days42
input_period <- "days21"
# set minimal group size
n_size <- 8

var_data %>%
  filter(Species %in% c('Rietzanger', 'Kleine karekiet'),
         Year %in% c('2020', '2021','2022'),
         Age == 'N1') %>%
  group_by(Species, Year, .data[[input_period]]) %>% 
  summarise(
    n = n(),
    mean_var = mean(.data[[input_var]], na.rm = TRUE),  
    sd_var = sd(.data[[input_var]], na.rm = TRUE), 
    se_var = sd_var / sqrt(n),  
    ci_lower = mean_var - 1.96 * se_var,
    ci_upper = mean_var + 1.96 * se_var
  ) %>%
  filter(n >= n_size) %>%
  ggplot(aes(x = .data[[input_period]], y = mean_var, color = Species)) +  
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_point(size = 2) +
  facet_wrap(~Year, ncol = 3) +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
  labs(x = input_period, y = input_var) +
  theme_minimal()
  
  
  

# nestling var
  var_nest_data %>%
    filter(Species %in% c('Rietzanger', 'Kleine karekiet'),
           Year %in% c('2020', '2021','2022')) %>%
    group_by(Species, Year, .data[[input_period]]) %>% 
    summarise(
      n = n(),
      mean_var = mean(.data[[input_var]], na.rm = TRUE),  
      sd_var = sd(.data[[input_var]], na.rm = TRUE), 
      se_var = sd_var / sqrt(n),  
      ci_lower = mean_var - 1.96 * se_var,
      ci_upper = mean_var + 1.96 * se_var
    ) %>%
    filter(n >= n_size) %>%
    ggplot(aes(x = .data[[input_period]], y = mean_var, color = Species)) +  
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_point(size = 2) +
    facet_wrap(~Year, ncol = 3) +
    scale_x_continuous(
      breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    labs(x = input_period, y = input_var) +
    theme_minimal()
  