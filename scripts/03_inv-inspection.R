## investigate what invertebrates already came out of the previous analysis

gsheets_auth()

link_inv1 <- "https://docs.google.com/spreadsheets/d/13ImnfdXIXchNJ7vbupZ8Oe8CyUrj5Cpr45j2NpVSlXk/edit"

database_inv1 <- read_gsdb(link_inv1)

names(database_inv1)

data_inv1 <- database_inv1[["Sheet1"]] %>%
  filter(rra >= 0.05, !is.na(host_age_group)) %>% # to filter out uncertain reads
  mutate(day_of_year = yday(collection_date),  # Day of year (1-365)
         days21 = ceiling((day_of_year -6) / 21), # the + changes the window of time
         Year = substr(collection_date, 1,4))

names(data_inv1)

unique(data_inv1$kingdom) # Metazoa
unique(data_inv1$phylum) # Arhtropoda
unique(data_inv1$class) # 4
unique(data_inv1$order) # 17
unique(data_inv1$family) # 81
unique(data_inv1$genus) # 166
unique(data_inv1$species) # 160


sum_data_inv1 <- data_inv1 %>%
  group_by(order) %>%
  summarise(avg_rra = mean(rra))


# get species per order
data_order2species <- data_inv1 %>% group_by(order) %>% distinct(species)

# highest rra species per order
data_order2species_rra <- data_inv1 %>% group_by(order, species) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

# highest rra family per order
data_order2family_rra <- data_inv1 %>% group_by(order, family) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

#View(data_order2species)
#View(data_order2species_rra)
#View(data_order2family_rra)



# compare nestling and adult

n1_order2species_rra <- data_order2species_rra <- data_inv1 %>% filter(host_age_group == 'adult') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

#View(n1_order2species_rra)

kj_order2species_rra <- data_order2species_rra <- data_inv1 %>% filter(host_age_group == 'nestling') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

#View(kj_order2species_rra)


## look at seasons
names(data_inv1)
count_inv1 <- data_inv1 %>%
  group_by(host_age_group, days21, Year) %>%
  summarise(count = n()) 

#View(count_inv1)

ggplot(count_inv1, aes(days21, count, color = Year, shape = host_age_group)) + 
  geom_point() + ggtitle("Rietzanger per 21 days")

# compare for days21 7

data_inv2022 <- data_inv1 %>%
  filter(Year == '2022')

n1_order2species_rra <- data_order2species_rra <- data_inv1 %>% filter(host_age_group == 'adult') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

##View(n1_order2species_rra)

kj_order2species_rra <- data_order2species_rra <- data_inv1 %>% filter(host_age_group == 'nestling') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

##View(kj_order2species_rra)


# graph important diet sources for 2022, compare for age, years and season
data_inv1 %>%
  filter(Year == '2022', 
         order %in% c('Trombidiformes', 'Ephemeroptera', 'Trichoptera', 'Hemiptera', 'Lepidoptera', 'Coleoptera', 'Diptera'))%>%
  group_by(order, days21, host_age_group) %>%
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(x = days21, y = avg_rra, color = order, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "Days21", y = "Average RRA", color = "Order", linetype = "Age Class") +
  theme_bw()



## calculate shannon biodiv for species level

data_div_sp <- data_inv1 %>%
  filter(!is.na(species)) %>%
  select(c(rra, species, host_age_group, days21, Year, sample_id)) %>%
  group_by(species, host_age_group, days21) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
  ungroup()

m_data_div_sp <- data_div_sp %>%
  select(-host_age_group, -days21) 


shannon_sp <- diversity(m_data_div_sp, index = "shannon")

result_sp <- data_div_sp %>%
  select(host_age_group, days21) %>%
  mutate(shannon_sp = shannon_sp)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
result_sp %>%
  ggplot(aes(x = days21, y = shannon_result, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "Days21", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw()



## calculate shannon biodiv for family level

data_div_fa <- data_inv1 %>%
  filter(!is.na(family)) %>%
  select(c(rra, family, host_age_group, days21, Year, sample_id)) %>%
  group_by(family, host_age_group, days21) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = family, values_from = rra, values_fill = 0) %>%
  ungroup()

m_data_div_fa <- data_div_fa %>%
  select(-host_age_group, -days21) 


shannon_fa <- diversity(m_data_div_fa, index = "shannon")

result_fa <- data_div_fa %>%
  select(host_age_group, days21) %>%
  mutate(shannon_fa = shannon_fa)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
result_fa %>%
  ggplot(aes(x = days21, y = shannon_result, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "Days21", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw()
