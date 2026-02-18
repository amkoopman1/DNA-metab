## investigate what invertebrates already came out of the previous analysis


# link to sedgewarbler_bil20260213 
link_inv1 <- "https://docs.google.com/spreadsheets/d/13ImnfdXIXchNJ7vbupZ8Oe8CyUrj5Cpr45j2NpVSlXk/edit"

database_inv1 <- read_gsdb(link_inv1)

names(database_inv1)

#
data_inv1 <- database_inv1[["Sheet1"]] %>%
  filter(rra >= 0.05, # only take reads with > 5% of resp. sample
         !is.na(host_age_group),
         !is.na(phylum)) %>% # filter that sample has age
  # give timebin of 21 days and 42 days
  mutate(day_of_year = yday(collection_date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +0) / 21), # set for 21 days
         days42 = ceiling((day_of_year +0) / 42), # the + changes the window of time
         Year = substr(collection_date, 1,4)) # give year

names(data_inv1)



# unique names in each taxonomic level 
unique(data_inv1$kingdom) # Metazoa
unique(data_inv1$phylum) # Arhtropoda
unique(data_inv1$class) # for rra >= 0.01, n=6
unique(data_inv1$order) # rra >= 0.01, n=23
unique(data_inv1$family) # rra >= 0.01, n=121
unique(data_inv1$genus) # rra >= 0.01, n=251
unique(data_inv1$species) # rra >= 0.01, n=261


sum_data_inv1 <- data_inv1 %>%
  group_by(order) %>%
  summarise(avg_rra = mean(rra))


# get species and order
data_order2species <- data_inv1 %>% group_by(order) %>% distinct(species)

# highest rra species per order
data_order_max_rra <- data_inv1 %>% group_by(order, species) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

# highest rra family per order
data_family_max_rra <- data_inv1 %>% group_by(order, family) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

#View(data_order2species)
#View(data_order_max_rra)
#View(data_family_max_rra)



# compare nestling and adult 

# adult highest avg rra species per order 
n1_species_rra <-  data_inv1 %>% filter(
  host_age_group == 'adult') %>%
  group_by(species, order) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

#View(n1_species_rra)

# plot species change over season adult
data_inv1 %>%
  filter(
    host_age_group == 'adult') %>%
  group_by(species, days42) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(days42, avg_rra, color = species)) + 
  geom_point() + theme(legend.position = "none")+
  geom_line()
  
# nestling highest avg rra species per order 
kj_species_rra <- data_inv1 %>% 
  filter(host_age_group == 'nestling') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

# plot species change over season nestling
data_inv1 %>%
  filter(
    host_age_group == 'nestling') %>%
  group_by(species, days42) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(days42, avg_rra, color = species)) + 
  geom_point() + theme(legend.position = "none")+
  geom_line()

#View(kj_pecies_rra)


## look at seasons
names(data_inv1)

# look at number of samples per season per year
count_inv1 <- data_inv1 %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(count = n()) 

#View(count_inv1)

ggplot(count_inv1, aes(days42, count, color = Year, shape = host_age_group)) + 
  geom_point() + ggtitle("Rietzanger samples per 42 days bin")

# compare for days42 time periods


# graph important diet sources compare for age and season
c1 <- c("#55D6BE", "#E57B40", "#7D5BA6", "#C95D63", "#AE8799", "#FC6471", "#496DDB")

data_inv1 %>%
  filter( # for readability
    order %in% c('Trombidiformes', 'Ephemeroptera', 'Trichoptera', 'Hemiptera', 'Lepidoptera', 'Coleoptera', 'Diptera'))%>%
  group_by(order, days42, host_age_group) %>%
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(x = days42, y = avg_rra, color = order, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "Average RRA", color = "Order", linetype = "Age Class") +
  theme_bw() + scale_color_manual(values = c1)+ theme(#)
)


## calculate shannon biodiv per season and age, at species level 

data_div_sp <- data_inv1 %>%
  filter(!is.na(species)) %>%
  select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
  group_by(species, host_age_group, days42) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
  ungroup()

s_data_div_sp <- data_div_sp %>%
  select(-host_age_group, -days42) 


shannon_sp <- diversity(s_data_div_sp, index = "shannon")

result_sp <- data_div_sp %>%
  select(host_age_group, days42) %>%
  mutate(shannon_sp = shannon_sp)



# the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most possibly due to sample size bias
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

# filter and use 12 samples per group
f_data_inv1 <- data_inv1 %>%
  filter(days42 %in% c('3', '4', '5'),
         !is.na(order)) %>%
  select(
    sample_id,host_age_group,species,family,order,rra,days42,days21,Year, day_of_year) %>%
  group_by(days42, host_age_group, Year) %>% # take for each days21 to have more even distributed of samples
  slice_sample(n=12) %>% 
  ungroup()

# mean sample day of each season*age 
f_samples_sum <- f_data_inv1 %>%
  group_by(host_age_group, days42) %>%
  summarise(avg_day = mean(day_of_year))
#View(f_samples_sum)

# make sure that 12 to each group
f_count_inv1 <- f_data_inv1 %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(count = n()) 



## calculate shannon biodiv for species level for filtered set

f_data_div_sp <- f_data_inv1 %>%
  filter(!is.na(species)) %>%
  select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
  group_by(species, days42, Year, host_age_group) %>%
  filter(n_distinct(sample_id) >= 1) %>%
  summarise(rra = mean(rra, na.rm = TRUE)) %>%
  pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
  ungroup()

f_m_data_div_sp <- f_data_div_sp %>%
  select(-days42, -Year, -host_age_group) 


f_shannon_sp <- diversity(f_m_data_div_sp, index = "shannon")

f_result_sp <- f_data_div_sp %>%
  select(days42, Year, host_age_group) %>%
  mutate(f_shannon_sp = f_shannon_sp)



# the biodiveristy index throughout the season
f_result_sp %>%
  ggplot(aes(x = days42, y = f_shannon_sp, color = Year, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
  theme_bw() + ggtitle("Biodiversity for even subset of data for 12 samples per datapoint")


## kruskal testing for differences between groups

kruskal.test(f_shannon_sp ~ days42, data = f_result_sp) 

kruskal.test(f_shannon_sp ~ Year, data = f_result_sp)



## for loop to see how stable results are
# based on this, choose sample size per group

results_list <- vector("list", 100)  # pre-allocate list of length 100

for (i in 1:100) {
  
  f_data_inv1 <- data_inv1 %>%
    filter(days42 %in% c('3', '4', '5'), # have plenty samples
           !is.na(order)) %>%
    select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
    group_by(days42, host_age_group, Year) %>%
    slice_sample(n = 12) %>%
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

# compare whether runs are different
kruskal.test(shannon ~ run, data = all_results) 


# mu, sd and variance of groups after 100 runs on random pulls, with now n=12
# ideally, cv < 0.1. otherwise, < 0.2 still acceptable.
all_results %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(
    mean_shannon = mean(shannon),
    sd_shannon   = sd(shannon),
    cv           = sd(shannon) / mean(shannon)
  )
