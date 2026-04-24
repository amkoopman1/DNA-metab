## find out which samples are still missing

## there's two categories; missing fecal sample and missing DNA sample (and DNA samples which are present but are too small and need replacement, will be categorized under missing).

## after finding what is missing, a new selection will need to be made that makes up for what is missing. for each subgroup of species-year-daysxx-age, a minimum of samples is required. first, find out which groups are now missing their minimal requirement. then, randomly select samples that satisfy the requirements to make all groups complete again. 


# load extraction IDs


database_link_extraction <- "https://docs.google.com/spreadsheets/d/1kRjM-joPMJhcgCTg3kDBM7eO6BX38MmkYNYHS6QoDIk/edit?"


database_extraction <- read_gsdb(database_link_extraction)
names(database_extraction)




# samples Wender
database_link3 <- "https://docs.google.com/spreadsheets/d/1bzJzrihGSMf62R97TgQrshgoHXCM8cqVKMqIYb29ebY/edit"

database3 <- read_gsdb(database_link3)
names(database3)


# url to write result to
samples_url <- "https://docs.google.com/spreadsheets/d/1uRL7KNfy2f5I_KIlV1mRosufzjYb88Bu9SbZ1l_s1c8/edit"



# which fecal samples are missing?

samples_total <- database_extraction[["total_list_samples"]] # all samples of the selection
samples_present <- database_extraction[["total_list_samples"]] %>% # fecal samples that were extracted
  inner_join(database_extraction[["ID_list"]], by = "Sample_ID")  
samples_missing <- database_extraction[["total_list_samples"]] %>% # fecal samples missing
  anti_join(database_extraction[["ID_list"]], by = "Sample_ID") %>%
  filter(Sequenced == "nee")

# which DNA samples are present or missing?

# load extraction IDs of other

database_extraction[["other_ID_list"]] %>%
  inner_join(database_extraction[["total_list_samples"]], by = "Sample_ID"
  ) %>% bind_rows(
    database_extraction[["other_ID_list"]] %>% 
      filter(sampletype == "negcontrol_extr") # include negative controls
  ) %>% # write DNA sample list to sheet
  select(Extraction_ID, Sample_ID) %>%
  arrange(Extraction_ID) %>%
  range_write(
    ss = samples_url, 
    sheet = "other_DNA_list",
    range = "A2",
    col_names = FALSE  # Set to TRUE if you want to write the column name too
  )

database_samples <- read_gsdb(samples_url) # load samples database

dna_sufficient <- database_samples[["other_DNA_list"]] %>% 
  filter(sufficient == '1') # present and sufficient DNA samples
dna_missing <- database_samples[["other_DNA_list"]] %>% 
  filter(present != '1') # not present or insufficient DNA samples

# what is missing in total?
# total_missing <- bind_rows(
#   samples_missing,
#   inner_join(
#   samples_total, dna_missing, by = 'Sample_ID'
# ) %>% filter(present != '1')
# )

total_present <- bind_rows(
  samples_present,
  inner_join(
    samples_total, dna_sufficient, by = 'Sample_ID'
  ))%>%
  distinct()

# get counts for the four analyses

# set minimal sample size per group

min_n <- 12

set_seed <- 379

# analysis 1
#samples_species_season_year

present_1 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21),  # retain days21 for later
         days42 = ceiling((day_of_year +1) / 42), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days42 %in% c('3','4', '5'),
         Year != '2023', Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1') %>%
  group_by(Age, Species, days42, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_1 <- present_1 %>%
  filter(lack > 0)

# analysis 2
#samples_season_year_age
#total_missing %>%
present_2 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21 %in% c('6','7','8','9','10') & Year %in% c('2020', '2021', '2022'), Age %in% c('N1', 'nestling'), Species %in% c('Rietzanger')) %>%
  group_by(Age, Species, days21,Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_2 <- present_2 %>% 
  filter(lack > 0)

# analysis 3
#samples_species_age
#total_missing %>%
present_3 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21b = ceiling((day_of_year -6) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21b %in% c('8'), Species %in% c('Kleine karekiet', 'Rietzanger'), 
         Year %in% c('2022'), Age %in% c('N1', 'nestling')) %>%
  group_by(Age, Species, days21b, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_3 <- present_3 %>%
  filter(lack > 0)

# analysis 4
#samples_age
#total_missing %>%
present_4 <- total_present %>%
  mutate(day_of_year = yday(Date),
         days21 = ceiling((day_of_year + 1) / 21),
         days42 = ceiling((day_of_year + 1) / 42),
         Year = substr(Date, 1, 4)
  ) %>%
  filter(Species %in% c('Rietzanger'), days21 %in% c('9', '10'),
         Year %in% c('2020')) %>%
  group_by(Age, Species, days21, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_4 <- present_4 %>%
  filter(lack > 0)


## function to assign new samples



# create dataframe with unsampled samples 
available_samples <- bind_rows(
  database3[["Captures"]] %>% select(Date, Species, Feces, Age, Sequenced) %>% mutate(source = "captures"),
  database3[["Nestlings"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest"),
  database3[["Nestlevel"]] %>% select(Date, Species, Feces, Sequenced) %>% mutate(source = "nest")
) %>%
  mutate(Feces = str_sub(Feces, -6, -1),
         Age = replace_na(Age, "nestling"))%>%
  filter(str_sub(Feces, 1, 1) == "A" # make sure sample name is correct
  ) %>% rename(
    Sample_ID = Feces
  ) %>% anti_join(samples_total, by = "Sample_ID")





# create dataframe to sample from


# analysis 1
#species * 6 weeks * year
an_1_species_season_year <- available_samples %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21),  # retain days21 for later
         days42 = ceiling((day_of_year +1) / 42), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days42 %in% c('3','4', '5'),
         Year != '2023', Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1') 

# analysis 2
# season (21 days) * year * age
an_2_season_year_age <- available_samples %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21 %in% c('6','7','8','9','10') & Year %in% c('2020', '2021', '2022'), Age %in% c('N1', 'nestling'), Species %in% c('Rietzanger'))

# analysis 3
# species * age 
an_3_species_age <- available_samples %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21b = ceiling((day_of_year -6) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21b %in% c('8'), Species %in% c('Kleine karekiet', 'Rietzanger'), 
         Year %in% c('2022'), Age %in% c('N1', 'nestling'))

# analysis 4
# juvenile captures
an_4_age_season <- available_samples %>%
  mutate(day_of_year = yday(Date),
         days21 = ceiling((day_of_year + 1) / 21),
         days42 = ceiling((day_of_year + 1) / 42),
         Year = substr(Date, 1, 4)
  ) %>%
  filter(Species %in% c('Rietzanger'), days21 %in% c('9', '10'),
         Year %in% c('2020'))



# combine into 1 dataframe
single_dataframe <-   bind_rows(
  an_1_species_season_year,
  an_2_season_year_age,
  an_3_species_age,
  an_4_age_season) %>% 
  distinct()


## dynamically set sampling rules (based on what was lacking)

rule_df <- bind_rows(
  rules_1, rules_2, rules_3, rules_4
)

# Convert the combined dataframe into sampling_rules list
sampling_rules <- lapply(1:nrow(rule_df), function(i) {
  row <- rule_df[i, ]
  list(
    name = paste0("rule_", i),
    conditions = list(
      Species = row$Species,
      Age = row$Age,
      Year = row$Year,
      days21 = row$days21,
      days21b = row$days21b,
      days42 = row$days42
    ),
    n = row$lack
  )
}) 



# function to select more samples

# Function to select samples based on rules
sample_by_rules <- function(
    data,
    sampling_rules,
    sheet_url,
    sheet_name = "total_list_samples_extra",
    seed = set_seed
) {
  
  set.seed(seed)
  
  previously_sampled <- data.frame(Sample_ID = character())
  skipped_rules <- character()
  all_samples <- data.frame()
  
  for(rule in sampling_rules) {
    
    # Start from full dataset
    filtered_df <- data
    
    # Apply all conditions (but skip NA conditions)
    for(var in names(rule$conditions)) {
      
      values <- rule$conditions[[var]]
      
      # If rule condition is NA → skip this variable entirely
      if(all(is.na(values))) next
      
      filtered_df <- filtered_df %>%
        filter(.data[[var]] %in% values)
    }
    
    # Check if there are any matching rows
    if(nrow(filtered_df) == 0) {
      skipped_rules <- c(skipped_rules, rule$name)
      next
    }
    
    # Split tiers
    tier1_reused <- filtered_df %>%
      semi_join(previously_sampled, by = "Sample_ID")
    
    tier2_new <- filtered_df %>%
      anti_join(previously_sampled, by = "Sample_ID")
    
    remaining <- rule$n
    rule_sample <- data.frame()
    
    # Sample reused first
    n_tier1 <- min(remaining, nrow(tier1_reused))
    if(n_tier1 > 0) {
      rule_sample <- bind_rows(rule_sample, tier1_reused %>% slice_sample(n = n_tier1))
      remaining <- remaining - n_tier1
    }
    
    # Then new
    if(remaining > 0) {
      n_tier2 <- min(remaining, nrow(tier2_new))
      if(n_tier2 > 0) {
        rule_sample <- bind_rows(rule_sample, tier2_new %>% slice_sample(n = n_tier2))
        remaining <- remaining - n_tier2
      }
    }
    
    # Track
    previously_sampled <- bind_rows(previously_sampled, rule_sample)
    all_samples <- bind_rows(all_samples, rule_sample)
    
    # Print progress
    cat("Rule:", rule$name, "- matching rows:", nrow(filtered_df), "\n")
    print(paste("Rule:", rule$name, "- Sampled:", nrow(rule_sample)))
  }
  
  # # Write to sheet ONCE after all sampling is done
  distinct_samples <- all_samples %>% distinct(Sample_ID, .keep_all = TRUE) %>% 
    arrange(Sample_ID)
  
  sheet_write(distinct_samples, ss = sheet_url, sheet = sheet_name)
  
  # Print summary
  cat("\n===== SAMPLING SUMMARY =====\n")
  cat("Total rows sampled:", nrow(all_samples), "\n")
  cat("Unique samples:", nrow(all_samples %>% distinct(Sample_ID)), "\n")
  
  if(length(skipped_rules) > 0) {
    cat("Skipped rules (no matching rows):", paste(skipped_rules, collapse = ", "), "\n")
  } else {
    cat("No rules were skipped.\n")
  }
  
  
}



result <- sample_by_rules(
  data = single_dataframe,
  sampling_rules = sampling_rules,
  sheet_url = samples_url,
  sheet_name = "total_list_samples_extra",
  seed = set_seed
)




# decided to revert back to 10 samples per group. but already took 37 out of the freezer. so, resampling for n=10 with what was already taken out because I don't want to go back to the freezer. 

# set minimal sample size per group

min_n <- 10

set_seed <- 379


subset_selected <- database_samples[["total_list_samples_extra"]] %>%
  filter(!Sample_ID %in% c("A01907", "A01908"))

# analysis 1
#samples_species_season_year

present_1 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21),  # retain days21 for later
         days42 = ceiling((day_of_year +1) / 42), # the + offsets the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days42 %in% c('3','4', '5'),
         Year != '2023', Species %in% c('Kleine karekiet', 'Rietzanger'), Age == 'N1') %>%
  group_by(Age, Species, days42, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_1 <- present_1 %>%
  filter(lack > 0)

# analysis 2
#samples_season_year_age
#total_missing %>%
present_2 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +1) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21 %in% c('6','7','8','9','10') & Year %in% c('2020', '2021', '2022'), Age %in% c('N1', 'nestling'), Species %in% c('Rietzanger')) %>%
  group_by(Age, Species, days21,Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_2 <- present_2 %>% 
  filter(lack > 0)

# analysis 3
#samples_species_age
#total_missing %>%
present_3 <- total_present %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         days21b = ceiling((day_of_year -6) / 21), # the + changes the window of time
         days42 = ceiling((day_of_year +1) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(days21b %in% c('8'), Species %in% c('Kleine karekiet', 'Rietzanger'), 
         Year %in% c('2022'), Age %in% c('N1', 'nestling')) %>%
  group_by(Age, Species, days21b, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_3 <- present_3 %>%
  filter(lack > 0)

# analysis 4
#samples_age
#total_missing %>%
present_4 <- total_present %>%
  mutate(day_of_year = yday(Date),
         days21 = ceiling((day_of_year + 1) / 21),
         days42 = ceiling((day_of_year + 1) / 42),
         Year = substr(Date, 1, 4)
  ) %>%
  filter(Species %in% c('Rietzanger'), days21 %in% c('9', '10'),
         Year %in% c('2020')) %>%
  group_by(Age, Species, days21, Year) %>%
  summarise(
    count = n(),
    lack = pmax(min_n - n(), 0),
    .groups = "drop"
  )

rules_4 <- present_4 %>%
  filter(lack > 0)


## dynamically set sampling rules (based on what was lacking)

rule_df <- bind_rows(
  rules_1, rules_2, rules_3, rules_4
)

# Convert the combined dataframe into sampling_rules list
sampling_rules <- lapply(1:nrow(rule_df), function(i) {
  row <- rule_df[i, ]
  list(
    name = paste0("rule_", i),
    conditions = list(
      Species = row$Species,
      Age = row$Age,
      Year = row$Year,
      days21 = row$days21,
      days21b = row$days21b,
      days42 = row$days42
    ),
    n = row$lack
  )
}) 



# function to select more samples REDRAW

# Function to select samples based on rules
sample_by_rules <- function(
    data,
    sampling_rules,
    sheet_url,
    sheet_name = "total_list_samples_extra_redraw",
    seed = set_seed
) {
  
  set.seed(seed)
  
  previously_sampled <- data.frame(Sample_ID = character())
  skipped_rules <- character()
  all_samples <- data.frame()
  
  for(rule in sampling_rules) {
    
    # Start from full dataset
    filtered_df <- data
    
    # Apply all conditions (but skip NA conditions)
    for(var in names(rule$conditions)) {
      
      values <- rule$conditions[[var]]
      
      # If rule condition is NA → skip this variable entirely
      if(all(is.na(values))) next
      
      filtered_df <- filtered_df %>%
        filter(.data[[var]] %in% values)
    }
    
    # Check if there are any matching rows
    if(nrow(filtered_df) == 0) {
      skipped_rules <- c(skipped_rules, rule$name)
      next
    }
    
    # Split tiers
    tier1_reused <- filtered_df %>%
      semi_join(previously_sampled, by = "Sample_ID")
    
    tier2_new <- filtered_df %>%
      anti_join(previously_sampled, by = "Sample_ID")
    
    remaining <- rule$n
    rule_sample <- data.frame()
    
    # Sample reused first
    n_tier1 <- min(remaining, nrow(tier1_reused))
    if(n_tier1 > 0) {
      rule_sample <- bind_rows(rule_sample, tier1_reused %>% slice_sample(n = n_tier1))
      remaining <- remaining - n_tier1
    }
    
    # Then new
    if(remaining > 0) {
      n_tier2 <- min(remaining, nrow(tier2_new))
      if(n_tier2 > 0) {
        rule_sample <- bind_rows(rule_sample, tier2_new %>% slice_sample(n = n_tier2))
        remaining <- remaining - n_tier2
      }
    }
    
    # Track
    previously_sampled <- bind_rows(previously_sampled, rule_sample)
    all_samples <- bind_rows(all_samples, rule_sample)
    
    # Print progress
    cat("Rule:", rule$name, "- matching rows:", nrow(filtered_df), "\n")
    print(paste("Rule:", rule$name, "- Sampled:", nrow(rule_sample)))
  }
  
  # # Write to sheet ONCE after all sampling is done
  distinct_samples <- all_samples %>% distinct(Sample_ID, .keep_all = TRUE) %>% 
    arrange(Sample_ID)
  
  sheet_write(distinct_samples, ss = sheet_url, sheet = sheet_name)
  
  # Print summary
  cat("\n===== SAMPLING SUMMARY =====\n")
  cat("Total rows sampled:", nrow(all_samples), "\n")
  cat("Unique samples:", nrow(all_samples %>% distinct(Sample_ID)), "\n")
  
  if(length(skipped_rules) > 0) {
    cat("Skipped rules (no matching rows):", paste(skipped_rules, collapse = ", "), "\n")
  } else {
    cat("No rules were skipped.\n")
  }
  
  
}



sample_by_rules(
  data = single_dataframe,
  sampling_rules = sampling_rules,
  sheet_url = samples_url,
  sheet_name = "total_list_samples_extra_redraw",
  seed = set_seed
)

