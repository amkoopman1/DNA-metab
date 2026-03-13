## weather variation


# read the data

# rain
rain_url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/maandgegevens/mndgeg_270_rh24.txt"

a_temp_url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/maandgegevens/mndgeg_270_tg.txt"

min_temp_url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/maandgegevens/mndgeg_270_tng.txt"

max_temp_url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/maandgegevens/mndgeg_270_txg.txt"

atmos_url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/maandgegevens/mndgeg_270_pg.txt"


# rain data
rain_data <- read_csv(rain_url, 
                         skip = 20,  # skip header lines
                         col_names = c("STN", "YYYY", "JAN", "FEB", "MAR", "APR", 
                                       "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                       "NOV", "DEC", "YEAR"),
                         na = c("", "NA", "     "))  # handle missing values


head(rain_data)


# Convert to long format
rain_long <- rain_data %>%
  pivot_longer(cols = JAN:YEAR,
               names_to = "Month",
               values_to = "Rain") %>%
  mutate(Rain = as.numeric(Rain) / 10, # convert to mm
         YYYY = as.numeric(YYYY)) %>% # treat  year as numeric
  filter(YYYY >= 1995 & YYYY != 2026) # filter for last 30 years
 


# average temp
a_temp_data <- read_csv(a_temp_url, 
                      skip = 20,  # Skip header lines
                      col_names = c("STN", "YYYY", "JAN", "FEB", "MAR", "APR", 
                                    "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                    "NOV", "DEC", "YEAR"),
                      na = c("", "NA", "     "))  # Handle missing values

head(a_temp_data)


# Convert to long format
a_temp_long <- a_temp_data %>%
  pivot_longer(cols = JAN:YEAR,
               names_to = "Month",
               values_to = "avg_temp") %>%
  mutate(avg_temp = as.numeric(avg_temp) / 10, # convert to degrees
         YYYY = as.numeric(YYYY)) %>% # treat  year as numeric
  filter(YYYY >= 1995 & YYYY != 2026) # filter for last 30 years


# minimum temp
min_temp_data <- read_csv(min_temp_url, 
                        skip = 20,  # Skip header lines
                        col_names = c("STN", "YYYY", "JAN", "FEB", "MAR", "APR", 
                                      "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                      "NOV", "DEC", "YEAR"),
                        na = c("", "NA", "     "))  # Handle missing values

head(min_temp_data)


# Convert to long format
min_temp_long <- min_temp_data %>%
  pivot_longer(cols = JAN:YEAR,
               names_to = "Month",
               values_to = "min_temp") %>%
  mutate(min_temp = as.numeric(min_temp) / 10, # convert to degrees
         YYYY = as.numeric(YYYY)) %>% # treat  year as numeric
  filter(YYYY >= 1995 & YYYY != 2026) # filter for last 30 years


# maximum temp
max_temp_data <- read_csv(max_temp_url, 
                        skip = 20,  # Skip header lines
                        col_names = c("STN", "YYYY", "JAN", "FEB", "MAR", "APR", 
                                      "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                      "NOV", "DEC", "YEAR"),
                        na = c("", "NA", "     "))  # Handle missing values

head(max_temp_data)


# Convert to long format
max_temp_long <- max_temp_data %>%
  pivot_longer(cols = JAN:YEAR,
               names_to = "Month",
               values_to = "max_temp") %>%
  mutate(max_temp = as.numeric(max_temp) / 10, # convert to degrees
         YYYY = as.numeric(YYYY)) %>% # treat  year as numeric
  filter(YYYY >= 1995 & YYYY != 2026) # filter for last 30 years



# atmospheric pressure
atmos_data <- read_csv(atmos_url, 
                      skip = 20,  # skip header lines
                      col_names = c("STN", "YYYY", "JAN", "FEB", "MAR", "APR", 
                                    "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                    "NOV", "DEC", "YEAR"),
                      na = c("", "NA", "     "))  # handle missing values


head(atmos_data)


# Convert to long format
atmos_long <- atmos_data %>%
  pivot_longer(cols = JAN:YEAR,
               names_to = "Month",
               values_to = "hPa") %>%
  mutate(hPa = as.numeric(hPa) / 10, # convert to mm
         YYYY = as.numeric(YYYY)) %>% # treat  year as numeric
  filter(YYYY >= 1995 & YYYY != 2026) # filter for last 30 years


# rain graph
rain_long %>%
  group_by(Month) %>%
  summarise(
    variability = sd(Rain, na.rm = TRUE),
    cv = sd(Rain, na.rm = TRUE) / mean(Rain, na.rm = TRUE)
  ) %>%
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "YEAR"))) %>%
  ggplot(aes(x = Month, y = cv)) +
  geom_col(fill = "steelblue") +
  labs(title = "Rain variability (between years) of 1995-2025",
       y = "coefficient of variation (CV)",
       x = "period") +
  theme_minimal()


# average temp graph
a_temp_long %>%
  group_by(Month) %>%
  summarise(
    variability = sd(avg_temp, na.rm = TRUE))%>%
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "YEAR"))) %>%
  ggplot(aes(x = Month, y = variability)) +
  geom_col(fill = "orange") +
  labs(title = "avg_temp variability (between years) of 1995-2025",
       y = "variability (sd)",
       x = "period") +
  ylim(0, 2.5) +
  theme_minimal()


# minimum temp graph
min_temp_long %>%
  group_by(Month) %>%
  summarise(
    variability = sd(min_temp, na.rm = TRUE))%>%
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "YEAR"))) %>%
  ggplot(aes(x = Month, y = variability)) +
  geom_col(fill = "orange") +
  labs(title = "min_temp variability (between years) of 1995-2025",
       y = "variability (sd)",
       x = "period") +
  ylim(0, 2.5) +
  theme_minimal()

# maximum temp graph
max_temp_long %>%
  group_by(Month) %>%
  summarise(
    variability = sd(max_temp, na.rm = TRUE))%>%
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "YEAR"))) %>%
  ggplot(aes(x = Month, y = variability)) +
  geom_col(fill = "orange") +
  labs(title = "max_temp variability (between years) of 1995-2025",
       y = "variability (sd)",
       x = "period") +
  ylim(0, 2.5) +
  theme_minimal()

# atmospheric pressure graph
atmos_long %>%
  group_by(Month) %>%
  summarise(
    variability = sd(hPa, na.rm = TRUE),
    cv = sd(hPa, na.rm = TRUE) / mean(hPa, na.rm = TRUE)
  ) %>%
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "YEAR"))) %>%
  ggplot(aes(x = Month, y = variability)) +
  geom_col(fill = "slategray3") +
  labs(title = "hPa variability (between years) of 1995-2025",
       y = "variability (sd)",
       x = "period") +
  theme_minimal()
