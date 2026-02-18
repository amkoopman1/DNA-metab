## choose samples for all four analyses 

# load sample data



# samples Wender
database_link3 <- "https://docs.google.com/spreadsheets/d/1bzJzrihGSMf62R97TgQrshgoHXCM8cqVKMqIYb29ebY/edit"

database3 <- read_gsdb(database_link3)
names(database3)


# sheet to write sample selection to 
samples_url <- "https://docs.google.com/spreadsheets/d/1uRL7KNfy2f5I_KIlV1mRosufzjYb88Bu9SbZ1l_s1c8/edit"



# make dataframe of all three sheets combined and prefilter data 
all_data <- bind_rows(
  database3[["Captures"]] %>% select(Date, Species, Feces, Age, Sequenced) %>% mutate(source = "captures"),
  database3[["Nestlings"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest"),
  database3[["Nestlevel"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest")
  )%>%
  mutate(Feces = str_sub(Feces, -6, -1),
         Age = replace_na(Age, "nestling"))%>%
  filter(str_sub(Feces, 1, 1) == "A",  # make sure sample name is correct
         Age %in% c('N1', 'nestling'))

#View(all_data)

# prepare datasets for sample selection

# species * age 
species_age <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year -6) / 21), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), 
         Year == '2022', Age %in% c('N1', 'nestling'))

sum_species_age <- species_age %>%
  group_by(Species, Age, days21) %>%
  summarise(count = n())

ggplot(sum_species_age, aes(days21, count, color = Species, shape = Age)) + 
  geom_point() + ggtitle("Rietzanger + karekiet 2022 adult N1 and nestlings per 21 days")

samples_species_age <- species_age %>%
  filter(days21 == '8') 


# season (21 days) * year * age

season_year_age <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Rietzanger'))


sum_season_year_age <- season_year_age %>%
  group_by(Age, Year, days21) %>%
  summarise(count = n())

ggplot(sum_season_year_age, aes(days21, count, color = Year, shape = Age)) + 
  geom_jitter() + ggtitle("Rietzanger over age and season and years")

samples_season_year_age <- season_year_age %>%
  filter(days21 %in% c('6','7', '8', '9', '10'),
         Year %in% c('2020', '2021') )

# species * year

species_year <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year -6) / 21), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1') %>%
  group_by(Species, days21, Year)

sum_species_year <- species_year %>%
  group_by(Species, Year, days21) %>%
  summarise(count = n())

ggplot(sum_species_year, aes(days21, count, color = Year, shape = Species)) + 
  geom_jitter() + ggtitle("Rietzanger + karekiet adult N1 per year")

samples_species_year <- species_year %>%
  filter(days21 == '8',
         Year != '2023') 

#species * 6 weeks * year

species_season_year <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days42 = ceiling((day_of_year +1) / 42), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1')

sum_species_season_year <- species_season_year %>%
  group_by(Species, Year, days42) %>%
  summarise(count = n())

ggplot(sum_species_season_year, aes(days42, count, color = Year, shape = Species)) + 
  geom_jitter() + ggtitle("Rietzanger + karekiet adult N1 per year")

samples_species_season_year <- species_season_year %>%
  filter(days42 %in% c('3','4', '5'),
         Year != '2023')

sum_samples_species_season_year <- samples_species_season_year %>%
  group_by(Species, Year, days42) %>%
  summarise(count = n())


# write to google sheets with maximum overlap



# take samples and write to google sheets
# List your dataframes
dataframes <- list(samples_species_season_year, samples_season_year_age,samples_species_age, samples_species_year)

# Define grouping columns for each dataframe
grouping_vars <- list(
  c("Species", "days42", "Year"), # groups for species_season_year
  c("days21", "Year", "Age"),    # groups for season_year_age
  c("Species", "Age"),           # groups for samples_species_age
  c("Species", "Year")          # groups for species_year
  
)

# Track all previously sampled rows
previously_sampled <- data.frame(Feces = character())

# Set different seeds
seeds <- c(41, 101, 201, 301)

# Sample from each dataframe
for(i in 1:4) {
  set.seed(seeds[i])
  
  # Get current dataframe
  current_df <- dataframes[[i]]
  
  # Split into 4 priority tiers
  # Tier 1: Overlap + no 'nee' (BEST)
  tier1 <- current_df %>%
    semi_join(previously_sampled, by = "Feces") %>%
    filter(Sequenced != "nee")
  
  # Tier 2: Overlap + has 'nee' (SECOND - at least reused)
  tier2 <- current_df %>%
    semi_join(previously_sampled, by = "Feces") %>%
    filter(Sequenced == "nee")
  
  # Tier 3: New + no 'nee' (THIRD - avoid nee more important than reuse)
  tier3 <- current_df %>%
    anti_join(previously_sampled, by = "Feces") %>%
    filter(Sequenced != "nee")
  
  # Tier 4: New + has 'nee' (WORST)
  tier4 <- current_df %>%
    anti_join(previously_sampled, by = "Feces") %>%
    filter(Sequenced == "nee")
  
  # Sample from each group
  new_sample <- data.frame()
  
  # Get unique groups
  groups_df <- current_df %>%
    distinct(across(all_of(grouping_vars[[i]])))
  
  for(j in 1:nrow(groups_df)) {
    # Filter each tier for this specific group
    group_filter_list <- as.list(groups_df[j, ])
    
    tier1_group <- tier1
    tier2_group <- tier2
    tier3_group <- tier3
    tier4_group <- tier4
    
    for(col in names(group_filter_list)) {
      tier1_group <- tier1_group %>% filter(.data[[col]] == group_filter_list[[col]])
      tier2_group <- tier2_group %>% filter(.data[[col]] == group_filter_list[[col]])
      tier3_group <- tier3_group %>% filter(.data[[col]] == group_filter_list[[col]])
      tier4_group <- tier4_group %>% filter(.data[[col]] == group_filter_list[[col]])
    }
    
    # Sample up to 12, filling from tiers in priority order
    remaining <- 14
    group_sample <- data.frame()
    
    # Take from tier 1 (overlap + no nee)
    n_tier1 <- min(remaining, nrow(tier1_group))
    if(n_tier1 > 0) {
      group_sample <- bind_rows(group_sample, tier1_group %>% slice_sample(n = n_tier1))
      remaining <- remaining - n_tier1
    }
    
    # Take from tier 2 (new + no nee)
    if(remaining > 0) {
      n_tier2 <- min(remaining, nrow(tier2_group))
      if(n_tier2 > 0) {
        group_sample <- bind_rows(group_sample, tier2_group %>% slice_sample(n = n_tier2))
        remaining <- remaining - n_tier2
      }
    }
    
    # Take from tier 3 (overlap + nee)
    if(remaining > 0) {
      n_tier3 <- min(remaining, nrow(tier3_group))
      if(n_tier3 > 0) {
        group_sample <- bind_rows(group_sample, tier3_group %>% slice_sample(n = n_tier3))
        remaining <- remaining - n_tier3
      }
    }
    
    # Take from tier 4 (new + nee)
    if(remaining > 0) {
      n_tier4 <- min(remaining, nrow(tier4_group))
      if(n_tier4 > 0) {
        group_sample <- bind_rows(group_sample, tier4_group %>% slice_sample(n = n_tier4))
        remaining <- remaining - n_tier4
      }
    }
    
    new_sample <- bind_rows(new_sample, group_sample)
  }
  
  # Count overlap and 'nee' stats
  n_overlap <- sum(new_sample$Feces %in% previously_sampled$Feces)
  n_coi <- sum(new_sample$Sequenced == "coi" & 
                 !new_sample$Feces %in% previously_sampled$Feces)
  n_coiplant <- sum(new_sample$Sequenced == "coi+plant" & 
                      !new_sample$Feces %in% previously_sampled$Feces)
  n_new_samples <- nrow(new_sample) - n_overlap
  
  # Add to tracker
  previously_sampled <- bind_rows(previously_sampled, new_sample)
  
  # Write to Google Sheet
  sheet_write(new_sample, 
              ss = samples_url,
              sheet = paste0("samples_", i))
  
  print(paste("Sampled", nrow(new_sample), "rows from samples_", i, 
              "- Reused:", n_overlap,
              "- New samples needed:", n_new_samples,
              "- New contains 'coi':", n_coi,
              "- New contains 'coi+plant':", n_coiplant))
}





# check if days and counts are evenly distributed 
database_samples <- read_gsdb(samples_url)
names(database_samples)


#samples_species_season_year
samples_1_sum <-  database_samples[["samples_1"]] %>%
  group_by(Species, days42) %>%
  summarise(avg_day = mean(day_of_year), count= n())
samples_1_sum

#samples_season_year_age
samples_2_sum <-  database_samples[["samples_2"]] %>%
  group_by(Age, days21, Year) %>%
  summarise(avg_day = mean(day_of_year), count= n())
samples_2_sum

#samples_species_age
samples_3_sum <- database_samples[["samples_3"]] %>%
  group_by(Age, Species) %>%
  summarise(avg_day = mean(day_of_year), count= n())
samples_3_sum

#samples_species_year
samples_4_sum <- database_samples[["samples_4"]] %>%
  group_by(Year, Species) %>%
  summarise(avg_day = mean(day_of_year), count= n())
samples_4_sum


# 16-02-2026
# total samples
180+284+48+72

# new samples for DNA isolation
180-9-78+176-91-9+17+8 #194

# new samples for PCR
180-9+176-9+17+8 #363


# 17-02-2026
# chucked out #2022 for the season_year_age and upped to 14 samples per group, after inspection of the previously sequenced data 

# new samples for DNA isolation
210-10-84+141-56-8+39+12 #244

# new samples for PCR
210-10+141-8+39-9+12 #375 
