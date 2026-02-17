## investigate what invertebrates already came out of the previous analysis

gsheets_auth()

link_inv1 <- "https://docs.google.com/spreadsheets/d/13ImnfdXIXchNJ7vbupZ8Oe8CyUrj5Cpr45j2NpVSlXk/edit"

database_inv1 <- read_gsdb(link_inv1)

names(database_inv1)

data_inv1 <- database_inv1[["Sheet1"]] %>%
  filter(rra >= 0.1, !is.na(host_age_group)) %>% # to filter out uncertain reads
  mutate(day_of_year = yday(collection_date),  # Day of year (1-365)
         days21 = ceiling((day_of_year -6) / 21),
         days42 = ceiling((day_of_year -6) / 42), # the + changes the window of time
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
  group_by(host_age_group, days42, Year) %>%
  summarise(count = n()) 

#View(count_inv1)

ggplot(count_inv1, aes(days42, count, color = Year, shape = host_age_group)) + 
  geom_point() + ggtitle("Rietzanger per 42 days")

# compare for days42 7

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
  group_by(order, days42, host_age_group) %>%
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(x = days42, y = avg_rra, color = order, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "Average RRA", color = "Order", linetype = "Age Class") +
  theme_bw()



## calculate shannon biodiv for species level

data_div_sp <- data_inv1 %>%
  filter(!is.na(species)) %>%
  select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
  group_by(species, host_age_group, days42) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
  ungroup()

m_data_div_sp <- data_div_sp %>%
  select(-host_age_group, -days42) 


shannon_sp <- diversity(m_data_div_sp, index = "shannon")

result_sp <- data_div_sp %>%
  select(host_age_group, days42) %>%
  mutate(shannon_sp = shannon_sp)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
result_sp %>%
  ggplot(aes(x = days42, y = shannon_sp, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw()



## calculate shannon biodiv for family level

data_div_fa <- data_inv1 %>%
  filter(!is.na(family)) %>%
  select(c(rra, family, host_age_group, days42, Year, sample_id)) %>%
  group_by(family, host_age_group, days42) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = family, values_from = rra, values_fill = 0) %>%
  ungroup()

m_data_div_fa <- data_div_fa %>%
  select(-host_age_group, -days42) 


shannon_fa <- diversity(m_data_div_fa, index = "shannon")

result_fa <- data_div_fa %>%
  select(host_age_group, days42) %>%
  mutate(shannon_fa = shannon_fa)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
result_fa %>%
  ggplot(aes(x = days42, y = shannon_fa, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw()



## now make a 'even' subset 
# use days42 3-5 for all years for all ages

f_data_inv1 <- data_inv1 %>%
  filter(days42 %in% c('3', '4', '5'),
         !is.na(order)) %>%
  select(
    sample_id,host_age_group,species,family,order,rra,days42,days21,Year, day_of_year) %>%
  group_by(days42, host_age_group, Year) %>%
  slice_sample(n=10) %>%
  ungroup()

f_samples_sum <- f_data_inv1 %>%
  group_by(host_age_group,Year, days42) %>%
  summarise(avg_day = mean(day_of_year))
#View(f_samples_sum)

f_count_inv1 <- f_data_inv1 %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(count = n()) 



## calculate shannon biodiv for species level for filtered set

f_data_div_sp <- f_data_inv1 %>%
  filter(!is.na(species)) %>%
  select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
  group_by(species, host_age_group, days42, Year) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
  ungroup()

f_m_data_div_sp <- f_data_div_sp %>%
  select(-host_age_group, -days42, -Year) 


f_shannon_sp <- diversity(f_m_data_div_sp, index = "shannon")

f_result_sp <- f_data_div_sp %>%
  select(host_age_group, days42, Year) %>%
  mutate(f_shannon_sp = f_shannon_sp)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
f_result_sp %>%
  ggplot(aes(x = days42, y = f_shannon_sp, linetype = host_age_group, color = Year)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw() + ggtitle("Biodiveristy for even subset of data for 10 samples per datapoint")


## kruskal testing for differences between groups

kruskal.test(f_shannon_sp ~ days42, data = f_result_sp) 

kruskal.test(f_shannon_sp ~ Year, data = f_result_sp) 
kruskal.test(f_shannon_sp ~ host_age_group, data = f_result_sp) 



## for loop to see how stable results are

results_list <- vector("list", 100)  # pre-allocate list of length 100

for (i in 1:100) {
  
  f_data_inv1 <- data_inv1 %>%
    filter(days42 %in% c('3', '4', '5'),
           !is.na(order)) %>%
    select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
    group_by(days42, host_age_group, Year) %>%
    slice_sample(n = 14) %>%
    ungroup()
  
  f_data_div_sp <- f_data_inv1 %>%
    filter(!is.na(species)) %>%
    select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
    group_by(species, host_age_group, days42, Year) %>%
    filter(n_distinct(sample_id) >= 1) %>%
    summarise(rra = mean(rra, na.rm = TRUE)) %>%
    pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
    ungroup()
  
  f_m_data_div_sp <- f_data_div_sp %>%
    select(-host_age_group, -days42, -Year)
  
  f_shannon_sp <- diversity(f_m_data_div_sp, index = "shannon")
  
  f_result_sp <- f_data_div_sp %>%
    select(host_age_group, days42, Year) %>%
    mutate(shannon = f_shannon_sp,
           run = i)  # <-- tag each row with which run it came from
  
  results_list[[i]] <- f_result_sp
}

all_results <- bind_rows(results_list)


kruskal.test(shannon ~ run, data = all_results) 

all_results %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(
    mean_shannon = mean(shannon),
    sd_shannon   = sd(shannon),
    cv           = sd(shannon) / mean(shannon)
  )
