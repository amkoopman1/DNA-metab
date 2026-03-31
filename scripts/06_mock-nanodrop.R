## primer mock samples test dry mass 2 DNA

link_nano <- "https://docs.google.com/spreadsheets/d/1NwqaNFbSUDX1Ql7RzCBbkkSWXrom6PsiYlW9_qKW6cI/edit"

database_nano <- read_gsdb(link_nano)

names(database_nano)

# mass data 
mass_data <- database_nano[["FactMashMass"]] %>%
  mutate(across(mass_empty:mass_dry_3, as.numeric)) %>%
  left_join(
    database_nano[["DimIndivid2Mash"]] %>% 
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
         mass_used = (mass_dry_used-mass_empty) * ifelse(is.na(dry_corr), 1, dry_corr),
         mass_sample = (mass_dry_1-mass_empty)* ifelse(is.na(dry_corr), 1, dry_corr),
         mass_individ= mass_sample/(n)) 


# nanodrop data
nano_data <- database_nano[["FactNanodrop"]] %>%
  mutate(across(conc_nano:nano_230, as.numeric)) %>%
  # connect correct mash tube to correct extraction ID
  left_join(
    database_nano[["DimIndivid2Mash"]] %>%
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
calculate_dna_pooling_simple <- function(
    nano_data,
    species_to_include,
    fractions,
    final_volume,
    species_col,
    conc_var,
    dilutions = "dilution",
    dna_col = "conc_nano"  # used to calculate final DNA concentration
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
  # Concentrations used for volume scaling
  concentrations <- data_subset[[conc_var]]
  dilutions <- data_subset[[dilutions]]
  dna_conc <- data_subset[[dna_col]]
  
  # undiluted concentration
  undiluted_conc <- concentrations  # from conc_var
  
  # actual concentration in the diluted stock
  diluted_conc <- concentrations / dilutions
  diluted_dna <- dna_conc / dilutions
  
  # calculate scaled volumes based on the diluted stock
  scaled_volumes <- fractions / diluted_conc
  volumes <- scaled_volumes / sum(scaled_volumes) * final_volume
  
  #warning for volume too small to pipet
  if (any(volumes < 0.5)) {
    warning("Some calculated volumes are too small to pipet! Adjust dilutions of other components. \n",
            paste(data_subset[[species_col]][volumes < 0.5], 
                  "=", round(volumes[volumes < 0.5], 2), "µL", collapse = "; "))
  }
  
  # final concentration in the pool (still based on the DNA column)
  final_dna_conc <- sum(volumes * diluted_dna) / final_volume
  
  # 
  # # Calculate scaled volumes
  # scaled_volumes <- fractions / (concentrations * dilutions)
  # volumes <- scaled_volumes / sum(scaled_volumes) * final_volume 
  # 
  # # Calculate final DNA concentration in mix (always using DNA column)
  # final_dna_conc <- sum(volumes * data_subset[[dna_col]]) / final_volume
  # 
  # Output
  result <- data.frame(
    Extraction_ID = data_subset$Extraction_ID,
    species = data_subset[[species_col]],
    fraction = fractions,
    volume_uL = round(volumes, 2),
    dilution = dilutions #,
    # diluted_conc = diluted_conc,
    # undiluted_conc = undiluted_conc
  )
  
  cat("\n=== How much uL to add ===\n")
  
  if (conc_var == "conc_nano") {
    cat("To ratio for: DNA concentration (conc_nano)\n")
  } else if (conc_var == "conc_mass") {
    cat("To ratio for: dry mass (conc_mass)\n")
  } else {
    cat("To ratio for: unknown concentration variable:", conc_var, "\n")
  }
  
  cat("Final volume:", final_volume, "µL\n")
  cat("Final DNA concentration in mix:", round(final_dna_conc, 2), "ng/µL\n\n")
  print(result, row.names = FALSE)
  
  invisible(list(
    samples = result,
    final_volume = final_volume,
    final_dna_conc_ng_uL = final_dna_conc
  ))
}


#use function
calculate_dna_pooling_simple(
  nano_data,
  # choose from: 
  # Hymenoptera, Diptera, Lepidoptera, Araneae, Conocephalus dorsalis, Leptophyes punctatissima, Stethophyma grossum or Locusta migratoria  
  species_to_include = c("Hymenoptera", "Lepidoptera","Diptera","Conocephalus dorsalis", "Araneae"),
  fractions = c(0.2,0.2,0.2,0.2,0.2),   # fraction in final pool
  final_volume = 25, # end volume of this mix
  species_col = "taxonomic_group",
  conc_var = "conc_nano"  # choose dna (conc_nano)  or mass (conc_mass)
)

# reset all dilutions
nano_data <- nano_data %>%
  mutate(
    dilution = c(rep(1,8))
  )

# set dilutions
nano_data <- nano_data %>%
  mutate(
    dilution    = c(1, # Hymenoptera
                    10, # Diptera
                    10, # Lepidoptera
                    10, # Araneae
                    10, # Conocephalus dorsalis
                    1, # Leptophyes punctatissima
                    1, # Stethophyma grossum
                    1) # Locusta migratoria
  )
