## choose samples for all four analyses 

# load sample data

gsheets_auth()


# samples Wender
database_link3 <- "https://docs.google.com/spreadsheets/d/1bzJzrihGSMf62R97TgQrshgoHXCM8cqVKMqIYb29ebY/edit"

database3 <- read_gsdb(database_link3)
names(database3)


# sheet to write samples to 
samples_url <- "https://docs.google.com/spreadsheets/d/1uRL7KNfy2f5I_KIlV1mRosufzjYb88Bu9SbZ1l_s1c8/edit"



# make dataframe of all three sheets combined and prefilter data 
all_data <- bind_rows(
  database3[["Captures"]] %>% select(Date, Species, Feces, Age, Sequenced) %>% mutate(source = "captures"),
  database3[["Nestlings"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest"),
  database3[["Nestlevel"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest")
  )%>%
  mutate(Feces = str_sub(Feces, -6, -1),
         Age = replace_na(Age, "nestling"))%>%
  filter(Sequenced %in% c('coi+plant', 'nee'), # filter away those sequenced without plant
         str_sub(Feces, 1, 1) == "A",
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


# season (28 days) * year * age

season_year_age <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days28 = ceiling((day_of_year -6) / 28), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Rietzanger'))

sum_season_year_age <- season_year_age %>%
  group_by(Age, Year, days28) %>%
  summarise(count = n())

ggplot(sum_season_year_age, aes(days28, count, color = Year, shape = Age)) + 
  geom_jitter() + ggtitle("Rietzanger over age and season and years")

samples_season_year_age <- season_year_age %>%
  filter(days28 %in% c('4','5','6'),
         Year != '2023') 

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
  filter(days21 == '8', Year != 2023) 

#species * 6 weeks * year

species_season_year <- all_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days42 = ceiling((day_of_year -6) / 42), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1')

sum_species_season_year <- species_season_year %>%
  group_by(Species, Year, days42) %>%
  summarise(count = n())

ggplot(sum_species_season_year, aes(days42, count, color = Year, shape = Species)) + 
  geom_jitter() + ggtitle("Rietzanger + karekiet adult N1 per year")

samples_species_season_year <- species_season_year %>%
  filter(days42 %in% c('4', '5'),
         Year != 2023)






# take samples and write to google sheets


# List your dataframes
dataframes <- list(samples_species_age, samples_season_year_age, samples_species_year, samples_species_season_year)

# Define grouping columns for each dataframe
grouping_vars <- list(
  c("Species", "Age"),           # groups for samples_species_age
  c("days28", "Year", "Age"),    # groups for season_year_age
  c("Species", "Year"),          # groups for species_year
  c("Species", "days42", "Year") # groups for species_season_year
)

# Track all previously sampled rows
previously_sampled <- data.frame(Feces = character())

# Set different seeds
seeds <- c(42, 100, 200, 300)

# Sample from each dataframe
for(i in 1:4) {
  set.seed(seeds[i])
  
  # Get current dataframe
  current_df <- dataframes[[i]]
  
  # Split into overlap and new rows
  overlap_rows <- current_df %>%
    semi_join(previously_sampled, by = "Feces")  # rows that ARE in previous samples
  
  new_rows <- current_df %>%
    anti_join(previously_sampled, by = "Feces")  # rows that are NOT in previous samples
  
  # Sample prioritizing overlap
  new_sample <- data.frame()
  
  # Get unique groups
  groups_df <- current_df %>%
    distinct(across(all_of(grouping_vars[[i]])))
  
  for(j in 1:nrow(groups_df)) {
    # Filter for this specific group
    group_filter_list <- as.list(groups_df[j, ])
    
    overlap_in_group <- overlap_rows
    new_in_group <- new_rows
    
    for(col in names(group_filter_list)) {
      overlap_in_group <- overlap_in_group %>%
        filter(.data[[col]] == group_filter_list[[col]])
      new_in_group <- new_in_group %>%
        filter(.data[[col]] == group_filter_list[[col]])
    }
    
    # Sample up to 10: first from overlap, then from new
    n_overlap <- min(10, nrow(overlap_in_group))
    n_new <- max(0, 10 - n_overlap)
    
    sample_from_overlap <- if(n_overlap > 0) {
      overlap_in_group %>% slice_sample(n = n_overlap)
    } else {
      data.frame()
    }
    
    sample_from_new <- if(n_new > 0 && nrow(new_in_group) > 0) {
      new_in_group %>% slice_sample(n = min(n_new, nrow(new_in_group)))
    } else {
      data.frame()
    }
    
    new_sample <- bind_rows(new_sample, sample_from_overlap, sample_from_new)
  }
  
  # Count overlap before adding to tracker
  n_overlap_total <- sum(new_sample$Feces %in% previously_sampled$Feces)
  
  # Add to tracker
  previously_sampled <- bind_rows(previously_sampled, new_sample)
  
  # Write to Google Sheet
  sheet_write(new_sample, 
              ss = samples_url,
              sheet = paste0("samples_", i))
  
  print(paste("Sampled", nrow(new_sample), "rows from samples_", i, 
              "- Overlap with previous:", n_overlap_total))
}


# leads to 223 samples if maximize overlap and use 10 samples per group

