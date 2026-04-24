## primer mock samples test dry mass 2 DNA

link_nano <- "https://docs.google.com/spreadsheets/d/1NwqaNFbSUDX1Ql7RzCBbkkSWXrom6PsiYlW9_qKW6cI/edit"

database_nano <- read_gsdb(link_nano)

names(database_nano)

# mass data 
mass_data <- database_nano[["FactMashMass"]] %>%
  mutate(across(mass_empty_1:mass_dry_3, as.numeric)) %>%
  left_join(
    database_nano[["DimSample"]] %>% 
      group_by(ID_mash) %>% 
      summarise(
        n = n() * first(volume2mashtube)
      ),
    by = "ID_mash"
  ) %>%
  mutate(
         # % sample left after 45 minutes of extra evaporation (compared to after drying once)
         dry_corr   = (mass_dry_2-mass_empty_2)/(mass_wet_2-mass_empty_2),
         # % sample left after 45 minutes of extra evaporation (compared to after drying twice)
         dry_curve   = (mass_dry_3-mass_empty_2)/(mass_dry_2-mass_empty_2), 
         # corrected mass for 'undry' sample
         mass_used = (mass_dry_used-mass_empty_1) * ifelse(is.na(dry_corr), 1, dry_corr),
         mass_sample = (mass_dry_1-mass_empty_1)* ifelse(is.na(dry_corr), 1, dry_corr),
         mass_individ= mass_sample/(n)) 


# nanodrop data
nano_data <- database_nano[["FactNanodrop"]] %>%
  mutate(across(conc_nano:nano_230, as.numeric)) %>%
  # connect correct mash tube to correct extraction ID
  left_join(
    database_nano[["DimSample"]] %>%
      distinct(ID_mash, Extraction_ID, order, genus_species),
    by = "Extraction_ID"
  ) %>%
  mutate(
    taxonomic_group = if_else(order == "Orthoptera", genus_species, order)
  ) %>%
  select(-order, -genus_species) %>%
  # connect correct mass to correct extraction ID
  left_join(
    mass_data %>%
      select(ID_mash, mass_used, mass_individ, n),
    by = "ID_mash"
  ) %>%
  mutate(
    conc_mass    = mass_used / 50, # dry mass per uL (g/uL)
    mass2dna     = (conc_nano * 50) / mass_used, # dna extracted / gram dry matter (ng/g)
    n2dna          = (conc_nano *50 )/ n # ng/individual
  )


# plot
nano_data %>% 
  ggplot(aes(conc_mass,conc_nano)) + geom_point()

nano_data %>%
  with(cor(conc_mass, conc_nano, use = "complete.obs"))

nano_data %>%
  filter(!taxonomic_group %in% c("Hymenoptera", "Araneae")) %>%
  with(cor(conc_mass, conc_nano, use = "complete.obs"))









## function to calculate dna-mix 
calculate_dna_pooling <- function(
    nano_data,
    species_to_include,
    fractions,
    final_volume,
    species_col = "taxonomic_group",
    conc_var,
    dilutions,
    dna_col = "conc_nano",
    fixed_volumes = NULL
) {
  # Validate columns
  if (!species_col %in% names(nano_data)) stop("Column '", species_col, "' not found")
  if (!conc_var %in% names(nano_data)) stop("Column '", conc_var, "' not found")
  if (!dna_col %in% names(nano_data)) stop("Column '", dna_col, "' not found")
  
  # Subset and reorder
  data_subset <- nano_data[nano_data[[species_col]] %in% species_to_include, ]
  data_subset <- data_subset[match(species_to_include, data_subset[[species_col]]), ]
  
  # Validate fractions
  if (length(fractions) != nrow(data_subset)) {
    message("Fractions length mismatch: expected ", nrow(data_subset), 
            " fractions but got ", length(fractions), ". Exiting function.")
    return(NULL)
  } 
  if (abs(sum(fractions) - 1) > 0.01) {
    warning("Fractions should sum to 1, but currently sum to ", sum(fractions))
  }  
  
  # Concentrations
  concentrations <- data_subset[[conc_var]]
  dna_conc <- data_subset[[dna_col]]
  
  diluted_conc <- concentrations / dilutions
  diluted_dna <- dna_conc / dilutions
  
  # Identify which species have fixed volumes
  is_fixed <- data_subset[[species_col]] %in% names(fixed_volumes)
  
  # Calculate volumes
  if (any(is_fixed)) {
    # Set fixed volumes
    volumes <- rep(NA, nrow(data_subset))
    for (species_name in names(fixed_volumes)) {
      idx <- which(data_subset[[species_col]] == species_name)
      if (length(idx) > 0) {
        volumes[idx] <- fixed_volumes[species_name]
      }
    }
    
    # Calculate remaining volume for non-fixed species
    fixed_total <- sum(volumes[is_fixed], na.rm = TRUE)
    remaining_volume <- final_volume - fixed_total
    
    if (remaining_volume <= 0) {
      stop("Fixed volumes (", fixed_total, " µL) exceed final volume (", final_volume, " µL)")
    }
    
    # Calculate non-fixed volumes to maintain their fractions
    non_fixed_idx <- which(!is_fixed)
    if (length(non_fixed_idx) > 0) {
      scaled_volumes <- fractions[non_fixed_idx] / diluted_conc[non_fixed_idx]
      volumes[non_fixed_idx] <- scaled_volumes / sum(scaled_volumes) * remaining_volume
    }
  } else {
    # No fixed volumes - calculate normally
    scaled_volumes <- fractions / diluted_conc
    volumes <- scaled_volumes / sum(scaled_volumes) * final_volume
  }
  
  # Warning for volume too small to pipet
  if (any(volumes < 0.5)) {
    warning("Some calculated volumes are too small to pipet! Adjust dilutions of other components. \n")
  }
  
  # calculate for results
  final_dna_conc <- sum(volumes * diluted_dna) / final_volume
  amounts_added <- volumes * diluted_conc
  true_fractions <- amounts_added / sum(amounts_added)
  
  # Output
  result <- data.frame(
    Extraction_ID = data_subset$Extraction_ID,
    species = data_subset[[species_col]],
    volume_uL = round(volumes, 2),
    dilution = dilutions,
    target_fraction = fractions,
    true_fraction = round(true_fractions, 3)
  )
  
  cat("\n=== How much uL to add ===\n")
  
  if (conc_var == "conc_nano") {
    cat("To ratio for: DNA concentration (conc_nano)\n")
  } else if (conc_var == "conc_mass") {
    cat("To ratio for: dry mass (conc_mass)\n")
  } else {
    cat("To ratio for: unknown concentration variable:", conc_var, "\n")
  }
  
  if (!is.null(fixed_volumes)) {
    cat("Fixed volumes:", paste(names(fixed_volumes), "=", fixed_volumes, "µL", collapse = ", "), "\n")
  }
  cat("Final volume of mix:", round(final_volume, 2), "µL\n")
  cat("Final DNA concentration in mix:", round(final_dna_conc, 2), "ng/µL\n\n")
  print(result, row.names = FALSE)
  
  invisible(list(
    samples = result,
    final_volume = final_volume,
    final_dna_conc_ng_uL = final_dna_conc
  ))
}



#use function


# for mock_01
calculate_dna_pooling(
  nano_data,
  # choose from: 
  # Hymenoptera, Diptera, Lepidoptera, Araneae, Conocephalus dorsalis, Leptophyes punctatissima, Stethophyma grossum or Locusta migratoria  
  species_to_include = c(
    "Hymenoptera", "Lepidoptera","Diptera","Conocephalus dorsalis","Araneae","Leptophyes punctatissima"),
  fractions = c(rep(1/6,6)),   # target fraction in final pool
  dilutions = c(1,1,1,1,1,1), # set dilution factor
  fixed_volumes = c("Hymenoptera" = 10),  # fix volumes (overrides fraction)
  final_volume = 50, # end volume of this mix
  conc_var = "conc_mass"  # choose dna (conc_nano)  or mass (conc_mass)
)



# for mock_02
calculate_dna_pooling(
  nano_data,
  # choose from: 
  # Hymenoptera, Diptera, Lepidoptera, Araneae, Conocephalus dorsalis, Leptophyes punctatissima, Stethophyma grossum or Locusta migratoria  
  species_to_include = c(
    "Lepidoptera","Conocephalus dorsalis"),
  fractions = c(rep(1/2,2)),   # target fraction in final pool
  dilutions = c(1,1), # set dilution factor
  #fixed_volumes = c("Hymenoptera" = 10),  # fix volumes (overrides fraction)
  final_volume = 5, # end volume of this mix
  conc_var = "conc_mass"  # choose dna (conc_nano)  or mass (conc_mass)
)

