### Check taxonomic assignment

# Load packages
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ivs)

# Set working directory
#setwd("G:/My Drive/ConsEco/Projects/2024-07-Yearround diet European stonechat/04-Bioinformatics/")

setwd("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2")

# Load function to select best top hit
source("07-phyloseq/get_tophit.R")
source("07-phyloseq/agreement_tophit.R")

### Load nsr comparison
nsr <- read.table("06-taxonmatcher/taxonmatcher.out", 
                  sep = "\t", quote = "", 
                  comment.char = "", 
                  header = TRUE) |> 
  as_tibble() |> 
  rename(input.taxon = X.Input,
         match.type = X.MatchType,
         synonym = X.Synonym,
         name.accepted = X.Accepted.name,
         match.taxon.rank = X.Taxon.rank,
         match.kingdom = X.Kingdom,
         match.phylum = X.Phylum,
         match.class = X.Class,
         match.order = X.Order,
         match.family = X.Family,
         match.genus = X.Genus,
         match.metadata = X.Metadata) |> 
  mutate(
    input.taxon = str_split_fixed(input.taxon, " / ", 
                                  n = 7)) |> # Kingdom Phylum Class Order Family Genus Species
  mutate(kingdom = input.taxon[,1],
         phylum = input.taxon[,2],
         class = input.taxon[,3],
         order = input.taxon[,4],
         family = input.taxon[,5],
         genus = input.taxon[,6],
         species = input.taxon[,7]) |> 
  select(-X.Input.read, -input.taxon) |> 
  mutate(across(12:18, ~ replace(., str_detect(., "unknown"), NA))) |> 
  mutate(across(1:11, ~ replace(., . == "", NA))) |> 
  distinct() |> 
  # Add an id to each line
  mutate(nsr_id = 1:n()) 

### Load blast results
blast.files <- tibble(
  file = list.files(
    path = "04-blast",
    pattern = "taxonomic", 
    recursive = TRUE, 
    full.names = TRUE)) |> 
  mutate(reference_db = str_extract(file, "gb|bold"), locus = str_extract(file, "leray|verkuil_mod1|verkuil_mod2|verkuil_og"))

blast.taxa <- lapply(1:nrow(blast.files), function(i) read.table(blast.files$file[i], sep="\t", dec = ".", quote = "") |> 
                       rename(asv_id = V1,
                              subject = V2,
                              accession_id = V3,
                              taxonomy_id = V4,
                              identity = V5,
                              coverage = V6,
                              evalue = V7,
                              bitscore = V8,
                              reference_db = V9,
                              taxonomy = V10) |> 
                       mutate(reference_db = blast.files$reference_db[i],
                              asv_id = str_c(asv_id,blast.files$locus[i], sep = "_"),
                              taxonomy = str_remove_all(taxonomy, "'"))) |> 
  do.call(what = rbind)

lca.files <- tibble(file = list.files(path = "05-taxonomic_assignment", pattern = "lca-", recursive = TRUE, full.names = TRUE)) |> 
  mutate(reference_db = str_extract(file, "gb|bold"), locus = str_extract(file, "leray|verkuil_mod1|verkuil_mod2|verkuil_og"))

lca <- lapply(1:nrow(lca.files), function(i) read.table(lca.files$file[i],  sep="\t", dec = ".", header = FALSE, na = c("NA")) |> 
                rename(asv_id = V1,
                       lca_rank = V2,
                       lca_taxon = V3,
                       kingdom = V4,
                       phylum = V5,
                       class = V6,
                       order = V7,
                       family = V8,
                       genus = V9,
                       species = V10,
                       method = V11,
                       identity = V12,
                       coverage = V13) |>
                as_tibble() |> 
                mutate(identity.min = as.numeric(str_split_fixed(identity, "-", 2)[,1]),
                       identity.max = as.numeric(str_split_fixed(identity, "-", 2)[,2]),
                       coverage.min = as.numeric(str_split_fixed(coverage, "-", 2)[,1]),
                       coverage.max = as.numeric(str_split_fixed(coverage, "-", 2)[,2]),
                       n = as.numeric(str_extract(method, "\\d+")),
                       ## Change, mistake below
                       reference_db = lca.files$reference_db[i],
                       asv_id = str_c(asv_id,blast.files$locus[i], sep = "_"))) |> 
  do.call(what = rbind)

## Split blast hits in a asvs identified through top hits and through an lca analysis
taxa.tophit <- lca |> 
  filter(str_detect(method, "top hit"))
taxa.lca <- lca |> 
  filter(!str_detect(method, "top hit"))

# select relevant taxa for both methods
# lca
blast.taxa.lca <- blast.taxa |> 
  filter(asv_id %in% taxa.lca$asv_id) |> 
  group_by(reference_db, asv_id) |> 
  filter(bitscore > 0.92*max(bitscore) & identity >= 80 & coverage >= 80) |> 
  summarise(identity.max = max(identity),
            identity.min = min(identity),
            coverage.max = max(coverage),
            coverage.min = min(coverage),
            evalue.min = min(evalue),
            evalue.max = max(evalue),
            bitscore.min = min(bitscore),
            bitscore.max = max(bitscore),
            n = n())

# tophit
blast.taxa.tophit <- blast.taxa |> 
  filter(asv_id %in% taxa.tophit$asv_id &
           coverage == 100 &
           identity >= 98) |> 
  group_by(reference_db, asv_id, taxonomy) |> 
  summarise(identity.max = max(identity),
            identity.min = min(identity),
            coverage.max = max(coverage),
            coverage.min = min(coverage),
            evalue.min = min(evalue),
            evalue.max = max(evalue),
            bitscore.min = min(bitscore),
            bitscore.max = max(bitscore),
            n = n())

# Combine
# top hits
taxa.tophit <- taxa.tophit |> 
  mutate(taxonomy = str_c(kingdom, phylum, class, order, family, genus, species, sep = " / ")) |> 
  full_join(y = blast.taxa.tophit, by = c("asv_id", "reference_db","taxonomy"), suffix = c(".lca", ""))


# check whether identity, coverage and number of hits are the same in the blast and lca files
taxa.tophit |> 
  mutate(identity.max.dif = round(identity.max, 1) - identity.max.lca,
         identity.min.dif = round(identity.min, 1) - identity.min.lca,
         n.dif = n - n.lca) |> 
  filter(identity.max.dif !=0 | identity.min.dif != 0 | n.dif != 0)
# Correct, only difference is a rounding error

taxa.tophit <- taxa.tophit |> 
  select(-c(contains(".lca"), identity, coverage))

# non-top hits
taxa.lca <- taxa.lca |> 
  full_join(blast.taxa.lca, by = c("asv_id", "reference_db"), suffix = c(".lca", "")) |> 
  select(-c(contains(".lca"), identity, coverage))


# combine
taxa <- bind_rows(taxa.lca, taxa.tophit) |> 
  mutate(method = ifelse(str_detect(method, "top hit"), "top hit", method)) 

## Do all asvs with a taxonomic assignment have info on identity, coverage, evalue and bitscore?
taxa |> 
  filter((is.na(identity.max) |
            is.na(identity.min) |
            is.na(coverage.min) |
            is.na(coverage.max) |
            is.na(bitscore.min) |
            is.na(bitscore.max) |
            is.na(evalue.min) |
            is.na(evalue.max)) & lca_taxon != "no identification") |> 
nrow() == 0
# Correct

# have all asvs been included?
sum(!(lca$asv_id %in% taxa$asv_id)) == 0
# correct

## Set unknown taxa to NA and fix lca_taxon
fix_taxon <- function(lca_rank, lca_taxon, kingdom, phylum, class, order, family, genus, species) {
  if (is.na(lca_taxon)) {
    if (lca_rank == "species") {
      if (is.na(species)) lca_rank <- "genus"
    }
    if  (lca_rank == "genus") {
      if (is.na(genus)) lca_rank <- "family"
    }
    if  (lca_rank == "family") {
      if (is.na(family)) lca_rank <- "order"
    }
    if  (lca_rank == "order") {
      if (is.na(order)) lca_rank <- "class"
    }
    if  (lca_rank == "class") {
      if (is.na(class)) lca_rank <- "phylum"
    }
    if  (lca_rank == "class") {
      if (is.na(class)) lca_rank <- "phylum"
    }
    if  (lca_rank == "phylum") {
      if (is.na(phylum)) lca_rank <- "kingdom"
    }
    if  (lca_rank == "kingdom") {
      if (is.na(kingdom)) lca_rank <- "no identification"
    }
  }
  return(lca_rank)
}
fix_taxon2 <- function(lca_rank, lca_taxon, kingdom, phylum, class, order, family, genus, species) {
  if (!is.na(species)) {
    lca_rank <- "species"
  } else if (!is.na(genus)) {
    lca_rank <- "genus"
  } else if (!is.na(genus)) {
    lca_rank <- "genus"
  } else if (!is.na(family)) {
    lca_rank <- "family"
  } else if (!is.na(order)) {
    lca_rank <- "order"
  } else if (!is.na(class)) {
    lca_rank <- "class"
  } else if (!is.na(phylum)) {
    lca_rank <- "phylum"
  } else if (!is.na(kingdom)) {
    lca_rank <- "kingdom"
  } else {
    lca_rank <- "no identification"
  }
  
  return(lca_rank)
}
# To do: can fix_taxon & fix_taxon2 be combined?

agreement.level <- function(kingdom, phylum, class, order, family, genus, species, rm.na = FALSE) {
  # Remove NAs?
  if (rm.na) {
    kingdom = na.omit(kingdom)
    phylum = na.omit(phylum)
    class = na.omit(class)
    order = na.omit(order)
    family = na.omit(family)
    genus = na.omit(genus)
    species = na.omit(species)
  }
  if (length(unique(species)) == 1 & !any(is.na(species))) {
    lvl <- 7
  } else if (length(unique(genus)) == 1 & !any(is.na(genus))) {
    lvl <- 6
  } else if (length(unique(family)) == 1 & !any(is.na(family))) {
    lvl <- 5
  } else if (length(unique(order)) == 1 & !any(is.na(order))) {
    lvl <- 4
  } else if (length(unique(class)) == 1 & !any(is.na(class))) {
    lvl <- 3
  } else if (length(unique(phylum)) == 1 & !any(is.na(phylum))) {
    lvl <- 2
  } else if (length(unique(kingdom)) == 1 & !any(is.na(kingdom))) {
    lvl <- 1
  } else lvl <- -1
  return(lvl)
}

# Remove environmental samples and fix unidentified species
taxa <- taxa |> 
  filter(!str_detect(lca_taxon, "environmental")) |> 
  # Assume that taxa with a "." in the name (sp., aff., nr., etc.), or with a number are unknown species
  # Exception: sometimes a species is identified as a species "sensu lato", this identification is informative, but removed at this step
  mutate(across(3:10, ~ ifelse(.x == "no identification" | 
                                 str_detect(.x, "unknown") | 
                                 str_detect(.x, "\\.") | 
                                 str_detect(.x, "\\d") | 
                                 str_detect(.x, "endosymbiont") |
                                 str_detect(.x, "synthetic construct"), NA, .x))) |> 
  mutate(assign_id = 1:n()) |> 
  rowwise() |> 
  mutate(lca_rank = fix_taxon(lca_rank, lca_taxon, kingdom, phylum, class, order, family, genus, species)) |> 
  pivot_longer(cols = 4:10, names_to = "taxon_level", values_to = "taxon") |> 
  group_by(assign_id) |> 
  mutate(lca_taxon = ifelse(any(is.na(lca_taxon)), taxon[taxon_level == lca_rank], lca_taxon)) |> 
  pivot_wider(names_from = "taxon_level", values_from = "taxon") |> 
  ungroup() |> 
  select(-assign_id) 

## Combine with id from nsr comparisons 
# Convert to nsr ids to long format
nsr.long <- nsr |> 
  select(c(1:2, 12:19)) |> 
  pivot_longer(3:9, names_to = "rank", values_to = "taxon") |> 
  # For taxa that have been compared multiple times (e.g., based on multiple taxa nested within a taxa), keep only matched comparisons
  group_by(rank, taxon) |> 
  filter(length(unique(match.type)) == 1 | match.type %in% c("match", "fuzzy")) |> 
  select(rank, taxon, nsr_id) |> 
  distinct(rank, taxon, .keep_all = TRUE)

# Match hits that have only been done at lower taxonomic levels
taxa <- taxa |> 
  left_join(y = nsr.long, by = c("lca_rank" = "rank", "lca_taxon" = "taxon")) 

# Have all hits that could have been identified assigned to an nsr id?
taxa |> 
  filter(is.na(nsr_id) & !is.na(lca_taxon)) |> 
  nrow() == 0
# Correct!


### Are there cases where lcas including hits to a kingdom without a name resulted in an unjustified assignment as metazoa?
asv_metazoa <- lca |> 
  filter(lca_taxon == "Metazoa") |> 
  distinct(asv_id, reference_db)

asv_metazoa <- blast.taxa |> 
  filter(str_c(asv_id, reference_db) %in% str_c(asv_metazoa$asv_id, asv_metazoa$reference_db)) |> 
  mutate(kingdom = str_split_fixed(taxonomy, " / ", 2)[,1]) |> 
  distinct(asv_id, reference_db, kingdom) |> 
  group_by(asv_id, reference_db) |> 
  filter(length(unique(kingdom)) > 1) |> 
  distinct(asv_id, reference_db)
# 3 cases, correct lca output

lca <- lca |> 
  mutate(lca_rank = ifelse(str_c(asv_id, reference_db) %in% str_c(asv_metazoa$asv_id, asv_metazoa$reference_db), "no identification", lca_rank),
         lca_taxon = ifelse(str_c(asv_id, reference_db) %in% str_c(asv_metazoa$asv_id, asv_metazoa$reference_db), "no identification", lca_taxon),
         kingdom = ifelse(str_c(asv_id, reference_db) %in% str_c(asv_metazoa$asv_id, asv_metazoa$reference_db), "no identification", kingdom))

## Change taxonomic nomenclature to make genbank and bold comparable
# Change BOLDs Animalia to Metazoa and Plantae to Viridiplantae
# Change Genbanks Oomycota to BOLDs classification
temp <- taxa
taxa <- temp
bold.taxa <- taxa |> 
  filter(reference_db == "bold")
gb.taxa <- taxa |> 
  filter(reference_db == "gb")
bold.taxa <- bold.taxa |> 
  rowwise() |> 
  mutate(kingdom = ifelse(!is.na(kingdom) & kingdom == "Animalia", "Metazoa", kingdom),
         kingdom = ifelse(!is.na(kingdom) & kingdom == "Plantae", "Viridiplantae", kingdom),
         kingdom = ifelse(!is.na(phylum) & phylum == "Rhodophyta", "Viridiplantae", kingdom),
         kingdom = ifelse(!is.na(phylum) & phylum == "Ciliophora", "Chromista", kingdom))
gb.taxa <- gb.taxa |>  
  rowwise() |> 
  mutate(class = ifelse(!is.na(phylum) & phylum == "Oomycota", "Oomycota", class),
         kingdom = ifelse(!is.na(phylum) & !is.na(class) & (phylum == "Oomycota" | class == "Oomycota"), "Chromista", kingdom),
         phylum = ifelse(!is.na(phylum) & phylum == "Oomycota" & !is.na(phylum), "Heterokontophyta", phylum),
         kingdom = ifelse(!is.na(phylum) & phylum == "Rhodophyta", "Viridiplantae", kingdom),
         kingdom = ifelse(!is.na(phylum) & phylum == "Ciliophora", "Chromista", kingdom))

# Some other differences
gb.taxa <- gb.taxa |> 
  rowwise() |> 
  mutate(class = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Leotiomycetes", class),
         order = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Thelebolales", order),
         family = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Pseudeurotiaceae", family),
         family = ifelse(!is.na(family) & family == "Caeciliusidae", "Paracaeciliidae", family),
         order = ifelse(!is.na(order) & order == "Psocoptera", "Psocodea", order),
         family = ifelse(!is.na(family) & family == "Apionidae", "Brentidae", family),
         family = ifelse(!is.na(family) & family == "Elachistidae", "Geometridae", family),
         class = ifelse(!is.na(class) & class == "Eurotatoria", "Monogonta", class),
         class = ifelse(!is.na(class) & class == "Monogonta", "Monogononta", class),
         class = ifelse(!is.na(class) & class == "Hexanauplia", "Copepoda", class),
         family = ifelse(!is.na(family) & family == "Calliphoridae", "Rhinophoridae", family),
         family = ifelse(!is.na(family) & family == "Erirhinidae", "Curculionidae", family),
         family = ifelse(!is.na(family) & family == "Tenthredinidae", "Athaliidae", family),
         phylum = ifelse(!is.na(class) & class == "Dinophyceae", "Pyrrophycophyta", phylum),
         kingdom = ifelse(!is.na(class) & class == "Dinophyceae", "Chromista", kingdom),
         family = ifelse(!is.na(family) & family == "Diplogasteridae", "Diplogastridae", family),
         family = ifelse(!is.na(genus) & genus == "Rotaria", "Philodinidae", family),
         order = ifelse(!is.na(genus) & genus == "Rotaria", "Bdelloidea", order),
         order = ifelse(!is.na(genus) & genus == "Stilbella", "Hypocreales", order),
         class = ifelse(!is.na(genus) & genus == "Stilbella", "Sordariomycetes", class),
         phylum = ifelse(!is.na(genus) & genus == "Stilbella", "Ascomycota", phylum),
         kingdom = ifelse(!is.na(genus) & genus == "Stilbella", "Fungi", kingdom),
         family = ifelse(!is.na(genus) & genus == "Leohumicola", "Leotiomycetes_family_incertae_sedis", family),
         order = ifelse(!is.na(genus) & genus == "Leohumicola", "Leotiomycetes_order_Incertae_sedis", order),
         class = ifelse(!is.na(genus) & genus == "Leohumicola", "Leotiomycetes", class),
         phylum = ifelse(!is.na(genus) & genus == "Leohumicola", "Ascomycota", phylum),
         kingdom = ifelse(!is.na(genus) & genus == "Leohumicola", "Fungi", kingdom),
         class = ifelse(!is.na(genus) & genus == "Rotaria", "Monogononta", class),
         phylum = ifelse(!is.na(genus) & genus == "Rotaria", "Rotifera", phylum),
         kingdom = ifelse(!is.na(genus) & genus == "Rotaria", "Metazoa", kingdom),
         #class = ifelse(!is.na(order) & order == "Adinetida", "Bdelloidea", class),
         class = ifelse(!is.na(order) & order == "Adinetida", "Eurotatoria", class),
         class = ifelse(!is.na(order) & order == "Poales", "Liliopsida", class),
         family = ifelse(!is.na(genus) & genus == "Stilbella", NA, family),
         family = ifelse(!is.na(genus) & genus == "Proctolaelaps", "Melicharidae", family),
         order = ifelse(!is.na(family) & family == "Philodinidae", "Bdelloidea", order),
         class = ifelse(!is.na(family) & family == "Philodinidae", "Eurotatoria", class),
         family = ifelse(!is.na(genus) & genus == "Elachista", "Elachistidae", family),
         family = ifelse(!is.na(genus) & genus == "Thelaxes", "Aphididae", family),
         family = ifelse(!is.na(genus) & genus == "Cryphoeca", "Cybaeidae", family),
         family = ifelse(!is.na(genus) & genus == "Elachista", "Elachistidae", family),
         family = ifelse(!is.na(genus) & genus == "Xylophagus", "Xylophagidae", family),
         order = ifelse(!is.na(family) & family == "Acroloxidae", "Pulmonata", order),
         ## Lines added below
         order = ifelse(!is.na(family) & family == "Lymnaeidae", "Hygrophila", order),
         family = ifelse(!is.na(genus) & (genus == "Lagenidium" | genus == "Paralagenidium"), "Pythiaceae", family),
         order = ifelse(!is.na(genus) & (genus == "Lagenidium" | genus == "Paralagenidium"), "Peronosporales", order),
         order = ifelse(!is.na(family) & family == "Dunaliellaceae", "Volvocales", order),
         family = ifelse(!is.na(genus) & genus == "Leptolegnia", "Leptolegniaceae", family),
         family = ifelse(!is.na(genus) & genus == "Ourococcus", "Sphaeropleaceae", family),
         family = ifelse(!is.na(genus) & genus == "Aphanomyces", "Leptolegniaceae", family),
         class = ifelse(!is.na(order) & order == "Myliobatiformes", "Elasmobranchii", class),
         phylum = ifelse(!is.na(class) & class == "Bryopsida", "Bryophyta", phylum),
         order = ifelse(!is.na(family) & family == "Lutjanidae", "Perciformes", order),
         class = ifelse(!is.na(family) & family == "Lutjanidae", "Actinopterygii", class),
         class = ifelse(!is.na(order) & order == "Chaetonotida", "Gastrotricha_class_incertae_sedis", class),
         family = ifelse(!is.na(genus) & genus == "Haptoglossa", "Haptoglossaceae", family),
         order = ifelse(!is.na(genus) & genus == "Haptoglossa", "Haptoglossales", order),
         family = ifelse(!is.na(genus) & genus == "Toxocara", "Ascarididae", family),
         family = ifelse(!is.na(genus) & (genus == "Pichia" | genus == "Komagataella"), "Saccharomycetaceae", family),
         order = ifelse(!is.na(genus) & (genus == "Pichia" | genus == "Komagataella"), "Saccharomycetales", order),
         class = ifelse(!is.na(genus) & (genus == "Pichia" | genus == "Komagataella"), "Saccharomycetes", class),
         order = ifelse(!is.na(family) & family == "Polyplacidae", "Psocodea", order),
         class = ifelse(!is.na(order) & (order == "Tetraodontiformes" | order == "Gadiformes"), "Actinopterygii", class),
         order = ifelse(!is.na(family) & family == "Dipodascaceae", "Saccharomycetales", order),
         class = ifelse(!is.na(family) & family == "Dipodascaceae", "Saccharomycetes", class),
         class = ifelse(!is.na(order) & order == "Monostilifera", "Hoplonemertea", class),
         order = ifelse(!is.na(family) & family == "Hoplopleuridae", "Psocodea", order),
         kingdom = ifelse(!is.na(phylum) & phylum == "Discosea", "Protozoa", kingdom),
         family = ifelse(!is.na(genus) & genus == "Bolitophila", "Bolitophilidae", family),
         family = ifelse(!is.na(genus) & genus == "Linopodes", "Eupodidae", family),
         family = ifelse(!is.na(genus) & genus == "Discus", "Discidae", family),
         family = ifelse(!is.na(genus) & genus == "Syngamus", "Strongylidae", family),
         family = ifelse(!is.na(genus) & genus == "Acrolepia", "Glyphipterigidae", family),
         family = ifelse(!is.na(genus) & genus == "Stathmopoda", "Stathmopodidae", family),
         family = ifelse(!is.na(genus) & genus == "Opetia", "Opetiidae", family),
         family = ifelse(!is.na(genus) & genus == "Symmerus", "Ditomyiidae", family),
         family = ifelse(!is.na(genus) & genus == "Acrolophus", "Tineidae", family),
         family = ifelse(!is.na(genus) & genus == "Zora", "Miturgidae", family),
         phylum = ifelse(!is.na(class) & class == "Pinopsida", "Pinophyta", phylum),
         order = ifelse(!is.na(family) & family == "Cupressaceae", "Pinales", order),
         family = ifelse(!is.na(genus) & genus == "Trisateles", "Erebidae", family),
         family = ifelse(!is.na(genus) & genus == "Achlya", "Saprolegniaceae", family),
         family = ifelse(!is.na(genus) & genus == "Stomorhina", "Calliphoridae", family),
         order = ifelse(!is.na(family) & family == "Erinaceidae", "Erinaceomorpha", order),
         order = ifelse(!is.na(family) & family == "Cafeteriaceae", "Bicosoecales", order),
         class = ifelse(!is.na(family) & family == "Cafeteriaceae", "Bicosoecophyceae", class),
         phylum = ifelse(!is.na(family) & family == "Cafeteriaceae", "Heterokontophyta", phylum),
         kingdom = ifelse(!is.na(family) & family == "Cafeteriaceae", "Chromista", kingdom),
         family = ifelse(!is.na(genus) & genus == "Crenosoma", "Metastrongylidae", family),
         family = ifelse(!is.na(genus) & genus == "Gaidropsarus", "Gadidae", family),
         order = ifelse(!is.na(family) & family == "Metschnikowiaceae", "Saccharomycetales", order),
         class = ifelse(!is.na(family) & family == "Metschnikowiaceae", "Saccharomycetes", class),
         family = ifelse(!is.na(genus) & genus == "Peyssonneliopsis", "Furcellariaceae", family),
         order = ifelse(!is.na(genus) & genus == "Peyssonneliopsis", "Gigartinales", order),
         family = ifelse(!is.na(genus) & genus == "Clytia", "Campanulariidae", family),
         class = ifelse(!is.na(class) & class == "Actinopteri", "Actinopterygii", class),
         order = ifelse(!is.na(genus) & genus == "Myxotrichum", "Onygenales", order),
         order = ifelse(!is.na(family) & family == "Verrucidae", "Sessilia", order),
         order = ifelse(!is.na(family) & family == "Heterophyidae", "Plagiorchiida", order),
         family = ifelse(!is.na(genus) & genus == "Gammarella", "Nuuanuidae", family),
         family = ifelse(!is.na(genus) & genus == "Rutilus", "Cyprinidae", family),
         order = ifelse(!is.na(family) & family == "Gymnophallidae", "Plagiorchiida", order),
         order = ifelse(!is.na(family) & family == "Aeolosomatidae", "Polychaeta_incertae_sedis", order),
         class = ifelse(!is.na(family) & family == "Aeolosomatidae", "Polychaeta", class),
         family = ifelse(!is.na(genus) & genus == "Acrocephalus", "Acrocephalidae", family),
         family = ifelse(!is.na(genus) & genus == "Rhinia" | (is.na(genus) & family == "Rhiniidae"), "Rhinophoridae", family),
         class = ifelse(!is.na(family) & order == "Carcharhiniformes", "Elasmobranchii", class))


bold.taxa <- bold.taxa |> 
  rowwise() |> 
  mutate(class = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Leotiomycetes", class),
         order = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Thelebolales", order),
         family = ifelse(!is.na(genus) & genus == "Pseudogymnoascus", "Pseudeurotiaceae", family),
         family = ifelse(!is.na(genus) & genus == "Leohumicola", "Leotiomycetes_family_incertae_sedis", family),
         order = ifelse(!is.na(genus) & genus == "Leohumicola", "Leotiomycetes_order_Incertae_sedis", order),
         family = ifelse(!is.na(family) & family == "Erirhinidae", "Curculionidae", family),
         family = ifelse(!is.na(family) & family == "Calliphoridae", "Rhinophoridae", family),
         order = ifelse(!is.na(order) & order == "Flosculariacea", "Flosculariaceae", order),
         family = ifelse(!is.na(family) & family == "Crotoniidae", "Camisiidae", family),
         family = ifelse(!is.na(family) & family == "Tenthredinidae", "Athaliidae", family),
         order = ifelse(!is.na(genus) & genus == "Stilbella", "Hypocreales", order),
         class = ifelse(!is.na(genus) & genus == "Stilbella", "Sordariomycetes", class),
         phylum = ifelse(!is.na(genus) & genus == "Stilbella", "Ascomycota", phylum),
         kingdom = ifelse(!is.na(genus) & genus == "Stilbella", "Fungi", kingdom),
         family = ifelse(!is.na(genus) & genus == "Liebstadia", "Liebstadiidae", family),
         family = ifelse(!is.na(family) & family == "Bionectriaaceae", "Bionectriaceae", family),
         #family = ifelse(!is.na(family) & family == "Nectriaceae", "Clavicipitaceae", family),
         order = ifelse(!is.na(family) & family == "Daphniidae", "Diplostraca", order),
         family = ifelse(!is.na(genus) & genus == "Liebstadia", "Liebstadiidae", family),
         order = ifelse(!is.na(genus) & genus == "Rotaria", "Bdelloidea", order),
         class = ifelse(!is.na(genus) & genus == "Rotaria", "Monogononta", class),
         family = ifelse(!is.na(genus) & genus == "Thelonectria", "Nectriaceae", family),
         family = ifelse(!is.na(genus) & genus == "Fusarium", "Nectriaceae", family),
         order = ifelse(!is.na(order) & order == "Spirurida", "Rhabditida", order),
         family = ifelse(!is.na(family) & family == "Rhabditoididae", "Rhabditidae", family),
         phylum = ifelse(!is.na(class) & (class == "Magnoliopsida" | class == "Liliopsida"), "Streptophyta", phylum),
         family = ifelse(!is.na(genus) & genus == "Stilbella", NA, family),
         family = ifelse(!is.na(genus) & genus == "Doydirhynchus", "Nemonychidae", family),
         family = ifelse(!is.na(genus) & genus == "Penicillium", "Aspergillaceae", family),
         family = ifelse(!is.na(genus) & genus == "Ramazzottius", "Ramazzottiidae", family),
         order = ifelse(!is.na(genus) & genus == "Ramazzottius", "Parachela", order),
         family = ifelse(!is.na(genus) & genus == "Passaloecus", "Crabronidae", family),
         family = ifelse(!is.na(genus) & genus == "Phytophthora", "Peronosporaceae", family),
         order = ifelse(!is.na(family) & family == "Peronosporaceae", "Peronosporales", order),
         # Added lines below
         order = ifelse(!is.na(family) & family == "Philodinidae", "Bdelloidea", order),
         class = ifelse(!is.na(family) & family == "Philodinidae", "Eurotatoria", class),
         family = ifelse(!is.na(genus) & (genus == "Lagenidium" | genus == "Paralagenidium"), "Pythiaceae", family),
         order = ifelse(!is.na(genus) & (genus == "Lagenidium" | genus == "Paralagenidium"), "Peronosporales", order),
         family = ifelse(!is.na(genus) & (genus == "Chlorella" | genus == "Helicosporidium"), "Chlorellaceae", family),
         order = ifelse(!is.na(genus) & (genus == "Chlorella" | genus == "Helicosporidium"), "Chlorellales", order),
         class = ifelse(!is.na(genus) & genus == "Chlorella", "Trebouxiophyceae", class),
         family = ifelse(!is.na(genus) & genus == "Malassezia", "Malasseziaceae", family),
         class = ifelse(!is.na(genus) & genus == "Malassezia", "Malasseziomycetes", class),
         order = ifelse(!is.na(family) & family == "Macrobiotidae", "Parachela", order),
         family = ifelse(!is.na(genus) & genus == "Haptoglossa", "Haptoglossaceae", family),
         order = ifelse(!is.na(genus) & genus == "Haptoglossa", "Haptoglossales", order),
         class = ifelse(!is.na(genus) & genus == "Haptoglossa", "Oomycota", class),
         kingdom = ifelse(!is.na(genus) & genus == "Haptoglossa", "Chromista", kingdom),
         class = ifelse(!is.na(order) & order == "Philodinida", "Monogononta", class),
         order = ifelse(!is.na(family) & family == "Hypsibiidae", "Parachela", order),
         order = ifelse(!is.na(family) & family == "Capillariidae", "Trichinellida", order),
         family = ifelse(!is.na(genus) & genus == "Synchaetomella", "Chaetomellaceae", family),
         order = ifelse(!is.na(genus) & genus == "Synchaetomella", "Chaetomellales", order),
         class = ifelse(!is.na(order) & order == "Adinetida", "Eurotatoria", class),
         family = ifelse(!is.na(genus) & genus == "Asaphes", "Pteromalidae", family),
         family = ifelse(!is.na(genus) & genus == "Verticillium", "Plectosphaerellaceae", family),
         family = ifelse(!is.na(genus) & genus == "Toxocara", "Ascarididae", family),
         order = ifelse(!is.na(genus) & genus == "Verticillium", "Glomerellales", order),
         class = ifelse(!is.na(genus) & genus == "Verticillium", "Sordariomycetes", class),
         family = ifelse(!is.na(genus) & genus == "Metarhizium", "Clavicipitaceae", family),
         family = ifelse(!is.na(genus) & (genus == "Cerceris" | genus == "Psenulus"), "Crabronidae", family),
         family = ifelse(!is.na(genus) & genus == "Aspergillus", "Aspergillaceae", family),
         family = ifelse(!is.na(genus) & genus == "Phialocephala", "Mollisiaceae", family),
         order = ifelse(!is.na(genus) & genus == "Phialocephala", "Helotiales", order),
         class = ifelse(!is.na(genus) & genus == "Phialocephala", "Leotiomycetes", class),
         family = ifelse(!is.na(genus) & genus == "Hemileius", "Hemileiidae", family),
         family = ifelse(!is.na(genus) & genus == "Ismarus", "Diapriidae", family),
         family = ifelse(!is.na(genus) & genus == "Pemphredon", "Crabronidae", family),
         family = ifelse(!is.na(genus) & genus == "Xenillus", "Xenillidae", family),
         class = ifelse(!is.na(order) & order == "Mortierellales", "Mortierellomycetes", class),
         phylum = ifelse(!is.na(order) & order == "Mortierellales", "Mucoromycota", phylum),
         family = ifelse(!is.na(genus) & genus == "Lophoptera", "Euteliidae", family),
         family = ifelse(!is.na(genus) & genus == "Achlya", "Saprolegniaceae", family),
         family = ifelse(!is.na(genus) & genus == "Stomorhina", "Calliphoridae", family),
         family = ifelse(!is.na(genus) & genus == "Cernuella", "Geomitridae", family),
         family = ifelse(!is.na(genus) & genus == "Argogorytes", "Argogorytes", family),
         genus = ifelse(!is.na(genus) & genus == "Myxotrichium", "Myxotrichum", genus),
         class = ifelse(!is.na(genus) & genus == "Myxotrichum", "Leotiomycetes", class),
         family = ifelse(!is.na(genus) & genus == "Bathycoccus", "Bathycoccaceae", family),
         family = ifelse(!is.na(family) & family == "Portunidae", "Carcinidae", family),
         order = ifelse(!is.na(family) & family == "Pfiesteriaceae", "Peridiniales", order),
         family = ifelse(!is.na(genus) & genus == "Protoperidinium", "Protoperidiniaceae", family),
         family = ifelse(!is.na(genus) & genus == "Rossia", "Naviculaceae", family),
         order = ifelse(!is.na(genus) & genus == "Rossia", "Naviculales", order),
         class = ifelse(!is.na(genus) & genus == "Rossia", "Bacillariophyceae", class),
         phylum = ifelse(!is.na(genus) & genus == "Rossia", "Bacillariophyta", phylum),
         kingdom = ifelse(!is.na(genus) & genus == "Rossia", NA, kingdom),
         order = ifelse(!is.na(family) & family == "Bulimulidae", "Stylommatophora", order),
         family = ifelse(!is.na(genus) & genus == "Cryptococcus" & kingdom == "Fungi", "Cryptococcaceae", family),
         family = ifelse(!is.na(genus) & genus == "Bracteacoccus", "Bracteacoccaceae", family),
         family = ifelse(!is.na(genus) & genus == "Microglena", "Chlamydomonadaceae", family),
         order = ifelse(!is.na(genus) & genus == "Microglena", "Chlamydomonadales", order),
         class = ifelse(!is.na(genus) & genus == "Microglena", "Chlorophyceae", class),
         family = ifelse(!is.na(genus) & genus == "Ctenocladus", "Ctenocladaceae", family),
         class = ifelse(!is.na(order) & order == "Entomophthorales", "Entomophthoromycetes", class),
         phylum = ifelse(!is.na(order) & order == "Entomophthorales", "Zoopagomycota", phylum),
         family = ifelse(!is.na(genus) & genus == "Oecanthus", "Gryllidae", family),
         class = ifelse(!is.na(order) & order == "Mucorales", "Mucoromycetes", class),
         phylum = ifelse(!is.na(order) & order == "Mucorales", "Mucoromycota", phylum),
         family = ifelse(!is.na(genus) & genus == "Sphyrna", "Carcharhinidae", family),
         order = ifelse(!is.na(family) & family == "Brachytheciaceae", "Hypnales", order))


taxa <- bind_rows(bold.taxa, gb.taxa) |> 
  select(- taxonomy)

# Fix taxa assignment levels again
taxa <- taxa |> 
  ungroup() |> 
  mutate(assign_id = 1:n()) |> 
  rowwise() |> 
  mutate(lca_rank = fix_taxon2(lca_rank, lca_taxon, kingdom, phylum, class, order, family, genus, species)) |> 
  pivot_longer(cols = 15:21, names_to = "taxon_level", values_to = "taxon") |> 
  group_by(assign_id) |> 
  mutate(lca_taxon = ifelse(any(is.na(lca_taxon)), taxon[taxon_level == lca_rank], lca_taxon)) |> 
  pivot_wider(names_from = "taxon_level", values_from = "taxon") |> 
  ungroup() |> 
  select(-assign_id) 

### Summarise multiple hits to the same species
taxa <- taxa |> 
  group_by(asv_id, lca_rank, lca_taxon, method, reference_db, nsr_id, kingdom, phylum, class, order, family, genus, species) |> 
  summarise(identity.max = max(identity.max),
            identity.min = min(identity.min),
            coverage.max = max(coverage.max),
            coverage.min = min(coverage.min),
            evalue.max = max(evalue.max),
            evalue.min = min(evalue.min),
            bitscore.max = max(bitscore.max),
            bitscore.min = min(bitscore.min),
            n = sum(n)) 


length(unique(taxa$asv_id)) # 635 ASVs assigned by genbank, bold or both


# Add number for taxonomic rank
taxon.level <- tibble(rank = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "no identification"),
                      lca_level = c(1:7, -1))
taxa <-  taxa |>
  left_join(y = taxon.level, by = c("lca_rank" = "rank"))

#### Remove singleton assignments
## If a top hit has a single deviating assignment which makes up for less than a defined percentage of all hits, remove that assignment

threshold <- 0.1
taxa <- taxa |> 
  ungroup() |> 
  mutate(assignment.id = 1:n()) |> 
  group_by(asv_id, reference_db) |> 
  mutate(n.taxa = n(),
         match.fraction = n / sum(n)) |> 
  mutate(fraction.rank = rank(match.fraction, ties.method = "min"))

# Do hits deviate from other hits at the family level?
taxa <- taxa |> 
  group_by(asv_id, reference_db, kingdom, phylum, class, order, family) |>
  mutate(n.family = n())



# Remove taxa if:
# Match fraction is less than the threshold
# Hit belongs to a different family than the other hits
# In general, hits disagree on the at least the family level
# Hits are not nested
# Remove a maximum of two deviating hits
# Previous version: order should be deviating from other to remove hits
taxa.removed <- taxa |> 
  group_by(asv_id, reference_db) |>
  mutate(lvl_agree = agreement.level(kingdom, phylum, class, order, family, genus, species, rm.na = FALSE)) |> 
  #filter(match.fraction < threshold & n.order == 1 & !any(lca_level %in% lvl_agree) & lvl_agree < 5) |> 
  filter(match.fraction < threshold & lvl_agree < 5 & lca_level != lvl_agree & n.family == 1 & fraction.rank <= 2)
# Potential problems > how to deal with NA families?

# Check if there a no Homo or Wolbachia hits removed > if so, undo
taxa.removed <- taxa.removed |> 
  filter(!(genus %in% c("Homo", "Wolbachia")))

taxa <- taxa |> 
  filter(!(assignment.id %in% taxa.removed$assignment.id)) |> 
  select(-c(assignment.id, n.taxa, match.fraction, n.family, fraction.rank))

## Find top hits that do not occur in the Netherlands
# Skip for now...
# 
# 
# taxa |> 
#   left_join(nsr.long, by = "nsr_id")
#   select(nsr_id)
# 
# 
# 
# ### NSR comparison
# nsr.wide <- nsr |> 
#   mutate(nsr_id = as.character(nsr_id)) |> 
#   select(nsr_id, name.accepted, contains("match."), synonym)
# taxa <- taxa |> 
#   ungroup() |> 
#   mutate(assignment.id = 1:n())
#   
# # Remove species level-assignments that are not found in the nsr
# taxa.removed.nsr <- taxa |> 
#   separate_longer_delim(nsr_id, ";") |>
#   # Don't compare with nsr if the taxa could only assigned at the kingdom level
#   mutate(nsr_id = ifelse(lca_rank == "kingdom", NA, nsr_id)) |> 
#   left_join(y = nsr.wide, by = "nsr_id") |> 
#   filter(str_detect(method, "top hit") & lca_level == 7 & match.taxon.rank != "species")
# 
# taxa.removed.nsr |> 
#   filter(phylum == "Chordata") |> 
#   View()
# 
# 
# ## Check by hand!
# # Bring Homo sapiens back
# taxa.removed.nsr |> 
#   filter(genus != "Homo")
# 
# 
# taxa <- taxa |> 
#   filter(!(assignment.id %in% taxa.removed.nsr$assignment.id)) |> 
#   select(-assignment.id)

### Check where ASVs that need extra attention are
# taxa |> 
#   filter(asv_id %in% c("Zotu2", "Zotu81", "Zotu733", "Zotu2045")) |> 
#   arrange(asv_id, reference_db) 
# # Solved for now...

### 1. If at least one database identifies an ASV as a top hit or based on an lca analysis to either Wolbachia or Homo, use that assignment
taxa.wolbacha.homo <- taxa |> 
  group_by(asv_id) |> 
  filter(any(str_detect(genus, "Wolbachia")) | any(str_detect(genus, "Homo"))) 

summarised.taxa.wolbacha.homo <- taxa.wolbacha.homo |> 
  filter(any(str_detect(genus, "Wolbachia")) | any(str_detect(genus, "Homo"))) |> 
  filter(genus %in% c("Wolbachia", "Homo")) |> 
  # Remove species-level assignment
  mutate(lca_taxon = ifelse(lca_rank == "species", genus, lca_taxon)) |> 
  mutate(lca_level = ifelse(lca_rank == "species", 6, lca_level)) |> 
  mutate(species = ifelse(lca_rank == "species", NA, species)) |> 
  mutate(lca_rank = ifelse(lca_rank == "species", "genus", lca_rank)) |> 
  group_by(asv_id, lca_taxon) |> 
  mutate(n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n)) |> 
  group_by(asv_id) |> 
  filter(identity.min == max(identity.min)) |> 
  # If all asvs have now been assigned to the same taxon, make sure the nsr id is the same as well
  mutate(nsr_id = ifelse(length(unique(lca_taxon)) == 1, min(nsr_id), nsr_id)) |> 
  distinct() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
test <- summarised.taxa.wolbacha.homo |>
  filter(n() >1)
taxa |>
  filter(asv_id %in% test$asv_id) 
# View()
summarised.taxa.wolbacha.homo |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.wolbacha.homo)
# 1 ASVs have been identified in this way


# Remove summarised taxa from list
taxa.left <- taxa |> 
  group_by(asv_id) |> 
  filter(!(asv_id %in% summarised.taxa.wolbacha.homo$asv_id))


### 1. Select ASVs where both reference databases used an lca analysis for taxonomic assignment
taxa.lca <- taxa.left |> 
  group_by(asv_id) |> 
  filter(all(method == "lca"))

## For which ASVs is there agreement on the assignment?
# This includes ASV that have only been identified in a single database
taxa.lca.agree <- taxa.lca |> 
  filter(length(unique(lca_taxon)) == 1)



# Summarise, use genbank taxonomy for summarised assignment
summarised.taxa.lca.agree <- taxa.lca.agree |> 
  mutate(n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n),
         nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  distinct()

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.lca.agree |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0

# Correct
nrow(summarised.taxa.lca.agree)
# 95 ASVs have been identified in this way


taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.lca.agree$asv_id))

# For which taxa is there no agreement on the assignment?
taxa.lca.disagree <- taxa.lca |> 
  filter(length(unique(lca_taxon)) > 1)

# Some of this disagreement may be because of differences in the level of taxonomic assignment
taxa.lca.disagree.level <- taxa.lca.disagree |> 
  filter(length(unique(lca_level)) > 1)

# Check if there is agreement at higher taxonomic levels

# Find the level of agreement
taxa.lca.disagree.level <- taxa.lca.disagree.level |> 
  mutate(lvl_agree = agreement.level(kingdom, phylum, class, order, family, genus, species))

# If the level of agreement between bold and genbank corresponds to the lca level of the database that identifies to the higher taxonomic level, 
# the databases only disagree on the level of taxonomic assignment, not on the taxonomy itself
# In that case, use the assignment with the highest taxonomic level
summarised.taxa.lca.disagree.level <- taxa.lca.disagree.level |> 
  filter(any(lvl_agree == min(lca_level))) |> 
  mutate(n_dbs = length(unique(reference_db))) |> 
  filter(lca_level == min(lca_level)) |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.lca.disagree.level |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.lca.disagree.level)
# 95 ASVs have been identified in this way

# What ASVs are left 
taxa.lca.disagree <- taxa.lca.disagree |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.level$asv_id))
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.level$asv_id))


# For what ASVs does the assignment disagree at the kingdom level?
# This usually means that a hit has the wrong label in the database
taxa.lca.disagree.kingdom <- taxa.lca.disagree |> 
  filter(length(unique(kingdom)) > 1)

# If BOLD suggests a metazoan assignment and genbank a plant, fungus, algae or unicellular organism, follow Genbank
summarised.taxa.lca.disagree.kingdom <- taxa.lca.disagree.kingdom |> 
  filter(asv_id %in% asv_id[reference_db == "bold" & kingdom == "Metazoa"]) |> 
  filter(reference_db == "gb") |> 
  mutate(n_dbs = 1) |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.lca.disagree.kingdom |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.lca.disagree.kingdom)
# 59 ASVs have been identified in this way


# What ASVs are left that disagree at the kingdom level?
taxa.lca.disagree.kingdom <- taxa.lca.disagree.kingdom |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom$asv_id))
taxa.lca.disagree <- taxa.lca.disagree |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom$asv_id))
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom$asv_id))

# Set the other asvs to 'no identification'
summarised.taxa.lca.disagree.kingdom.noident <- taxa.lca.disagree.kingdom |> 
  group_by(asv_id) |> 
  mutate(kingdom = NA, phylum = NA, class = NA, order = NA, family = NA, genus = NA, species = NA,
         lca_level = -1, lca_rank = "no identification", lca_taxon = "no identification", nsr_id = NA,
         n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n)) |> 
  select(-lca_level) |> 
  distinct() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.lca.disagree.kingdom.noident |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.lca.disagree.kingdom.noident)
# 5 ASVs have been identified in this way

# What samples are left?
taxa.lca.disagree.kingdom <- taxa.lca.disagree.kingdom |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom.noident$asv_id))
taxa.lca.disagree <- taxa.lca.disagree |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom.noident$asv_id))
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree.kingdom.noident$asv_id))

# For all samples left that disagree on the lca assignement, look at the lowest common ancestor in the taxonomic classification
summarised.taxa.lca.disagree <- taxa.lca.disagree |>  
  mutate(lvl_agree = agreement.level(kingdom, phylum, class, order, family, genus, species)) |> 
  mutate(assign_id = 1:n()) |> 
  rowwise() |> 
  pivot_longer(cols = 7:13, names_to = "taxon_level", values_to = "taxon") |> 
  left_join(y = rename(taxon.level, rank_level = lca_level), by = c("taxon_level" = "rank")) |> 
  # Remove taxonomic levels below the lca
  mutate(taxon = ifelse(rank_level <= lvl_agree, taxon, NA)) |> 
  # Fix lca rank and taxon
  group_by(asv_id, reference_db) |> 
  mutate(lca_rank = ifelse(lvl_agree == -1, "no identification", taxon_level[lvl_agree == rank_level]),
         lca_taxon = ifelse(lvl_agree == -1, "no identification", taxon[lvl_agree == rank_level])) |> 
  select(-rank_level) |> 
  pivot_wider(names_from = "taxon_level", values_from = "taxon") |> 
  group_by(asv_id) |> 
  select(-assign_id) |> 
  # Combine identity and coverage scores, reference dbs and number of hits
  mutate(n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n),
         nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  select(-lca_level) |> 
  distinct() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.lca.disagree |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.lca.disagree)
# 21 ASVs have been identified in this way

# What ASVs are left?
taxa.lca.disagree <- taxa.lca.disagree |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree$asv_id))
# None left!
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.lca.disagree$asv_id))

### 2. Taxa with at least one top hit match in one of the databases, and a lca in the other
taxa.toplca <- taxa.left |> 
  filter(!all(method == "top hit") & any(method == "top hit") & any(method == "lca"))

# For what ASVs does the assignment disagree at the kingdom level?
# This usually means that a hit has the wrong label in the database
taxa.toplca.disagree.kingdom <- taxa.toplca |> 
  filter(length(unique(kingdom)) > 1)

# If BOLD suggests a metazoan assignment and genbank a plant, fungus, algae or unicellular organism, follow Genbank
# If Genbank has multiple top hits, find the lowest common ancestor
summarised.taxa.toplca.disagree.kingdom <- taxa.toplca.disagree.kingdom |> 
  filter(asv_id %in% asv_id[reference_db == "bold" & kingdom == "Metazoa"]) |> 
  filter(reference_db == "gb") |> 
  mutate(n_dbs = 1) |> 
  group_by(asv_id) |> 
  group_split()


summarised.taxa.toplca.disagree.kingdom <- lapply(summarised.taxa.toplca.disagree.kingdom, get_tophit, prefer_perfect = T) |> 
  bind_rows() |> 
  # Only keep the best top hit
  filter(iv.rank == 1) |> 
  # Get lowest common ancestor if there is still more then a single top hit
  group_by(asv_id, reference_db) |> 
  group_split()



summarised.taxa.toplca.disagree.kingdom <- lapply(summarised.taxa.toplca.disagree.kingdom, agreement.tophit) |> 
  bind_rows() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.toplca.disagree.kingdom |> 
  group_by(asv_id) |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.toplca.disagree.kingdom)
# 5 ASVs have been identified in this way

# What ASVs are left 
taxa.toplca <- taxa.toplca |> 
  filter(!(asv_id %in% summarised.taxa.toplca.disagree.kingdom$asv_id))
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.toplca.disagree.kingdom$asv_id))


## Summarise the top hits into a single assignment per ASV, and compare with the lca analysis of the other database
taxa.toplca <- taxa.toplca |> 
  # Combine duplicate assignemnts
  group_by(asv_id, reference_db, lca_rank, lca_taxon, method, kingdom, phylum, class, order, family, genus, species, lca_level) |> 
  summarise(n_dbs = length(unique(reference_db)),
            reference_db = str_c(unique(reference_db), collapse = ";"),
            method = str_c(unique(method), collapse = ";"),
            identity.min = min(identity.min),
            identity.max = max(identity.max),
            coverage.min = min(coverage.min),
            coverage.max = max(coverage.max),
            evalue.min = min(evalue.min),
            evalue.max = max(evalue.max),
            bitscore.min = min(bitscore.min),
            bitscore.max = max(bitscore.max),
            n = sum(n),
            nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  group_by(asv_id, reference_db) |> 
  group_split()


taxa.toplca <- lapply(taxa.toplca, get_tophit) |> 
  bind_rows() |> 
  # Only keep the best top hit
  filter(iv.rank == 1) |> 
  # Get lowest common ancestor if there is still more then a single top hit
  group_by(asv_id, reference_db) |> 
  group_split()
taxa.toplca <- lapply(taxa.toplca, agreement.tophit) |> 
  bind_rows()

## Check if top hits are nested within the assignment by the lca analysis in the other
## In that case, follow genbank (assignments to a lower taxonomic level in BOLD only are not to be trusted)

summarised.toplca.nested <- taxa.toplca |>
  group_by(asv_id) |> 
  mutate(lvl_agree = agreement.level(kingdom, phylum, class, order, family, genus, species)) |> 
  filter(any(lvl_agree %in% lca_level)) |> 
  filter(reference_db  == "gb") |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.toplca.nested |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.toplca.nested)
# 8 ASVs have been identified in this way


# What ASVs are left 
taxa.toplca <- taxa.toplca |> 
  filter(!(asv_id %in% summarised.toplca.nested$asv_id))
# No taxa left
taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.toplca.nested$asv_id))

### Added 20250204
## Taxa identified with lca analyses that disagree with top hits
# Get the lowest common ancestor for the other asvs
taxa.toplca.disagreement <- taxa.toplca |>
  group_by(asv_id) |>
  mutate(n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n),
         nsr_id = str_c(unique(nsr_id), collapse = ";")) |>
  group_split()

summarised.taxa.toplca.disagreement <- lapply(taxa.toplca.disagreement, agreement.tophit) |>
  bind_rows() |>
  mutate(nsr_id = as.character(nsr_id))


# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.toplca.disagreement |>
  summarise(n = n()) |>
  filter(n > 1) |>
  nrow() == 0
# Correct
nrow(summarised.taxa.toplca.disagreement)
# 1 ASVs have been identified in this way

#What ASVs are left
taxa.toplca <- taxa.toplca |>
  filter(!(asv_id %in% summarised.taxa.toplca.disagreement$asv_id))
#No taxa left

taxa.left <- taxa.left |>
  filter(!(asv_id %in% summarised.taxa.toplca.disagreement$asv_id))


### 3. Taxa with top hits in both databases
taxa.top <- taxa.left |> 
  group_by(asv_id) |> 
  filter(all(method == "top hit"))

taxa.top <- taxa.top |> 
  # Combine duplicate assignemnts
  group_by(asv_id, reference_db, lca_rank, lca_taxon, method, kingdom, phylum, class, order, family, genus, species, lca_level) |> 
  summarise(n_dbs = length(unique(reference_db)),
            reference_db = str_c(unique(reference_db), collapse = ";"),
            method = str_c(unique(method), collapse = ";"),
            identity.min = min(identity.min),
            identity.max = max(identity.max),
            coverage.min = min(coverage.min),
            coverage.max = max(coverage.max),
            evalue.min = min(evalue.min),
            evalue.max = max(evalue.max),
            bitscore.min = min(bitscore.min),
            bitscore.max = max(bitscore.max),
            n = sum(n),
            nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  group_by(asv_id) |> 
  group_split()

taxa.top <- lapply(taxa.top, get_tophit) |> 
  bind_rows() |> 
  # Only keep the best top hit
  filter(iv.rank == 1) 

# First, find asvs for which all databases (can be only one) agree on the taxonomic assignment
summarised.taxa.top.agree <- taxa.top |> 
  group_by(asv_id) |> 
  filter(length(unique(lca_taxon)) == 1 & length(unique(lca_rank)) == 1) |> 
  # Combine duplicate hits 
  group_by(asv_id, lca_rank, lca_taxon, method, kingdom, phylum, class, order, family, genus, species, lca_level) |> 
  summarise(n_dbs = length(unique(reference_db)),
            reference_db = str_c(unique(reference_db), collapse = ";"),
            method = str_c(unique(method), collapse = ";"),
            identity.min = min(identity.min),
            identity.max = max(identity.max),
            coverage.min = min(coverage.min),
            coverage.max = max(coverage.max),
            evalue.min = min(evalue.min),
            evalue.max = max(evalue.max),
            bitscore.min = min(bitscore.min),
            bitscore.max = max(bitscore.max),
            n = sum(n),
            nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  mutate(nsr_id = as.character(nsr_id))


# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.top.agree |> 
  group_by(asv_id) |> 
  summarise(n = n()) |>
  filter(n > 1) |> 
  nrow() == 0

# Correct
nrow(summarised.taxa.top.agree)
# 79 ASVs have been identified in this way

# What ASVs are left 
taxa.top <- taxa.top |> 
  filter(!(asv_id %in% summarised.taxa.top.agree$asv_id))

taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.top.agree$asv_id))


# If taxonomic assignments are nested, follow the database with the identification to the lowest taxonomic level within hits from the same reference database
# Between databases, follow the level of genbank
summarised.taxa.top.nested <- taxa.top |>
  group_by(asv_id) |> 
  mutate(lvl_agree = agreement.level(kingdom, phylum, class, order, family, genus, species)) |> 
  filter(any(lvl_agree %in% lca_level))

summarised.taxa.top.nested <- summarised.taxa.top.nested |> 
  group_by(asv_id, reference_db) |> 
  mutate(lvl_agree_refdb = agreement.level(kingdom, phylum, class, order, family, genus, species)) |> 
  filter(any(lvl_agree_refdb %in% lca_level)) |> 
  filter(lca_level == max(lca_level)) 

# Within databases, do hits agree on the assignment level?
summarised.taxa.top.nested |> 
  group_by(asv_id, reference_db) |> 
  filter(length(unique(lca_level)) > 1) |> 
  nrow() == 0
# Correct

# If both databases agree on the assignment level, keep both
# Otherwise, follow genbank
summarised.taxa.top.nested <- summarised.taxa.top.nested |> 
  group_by(asv_id) |> 
  filter(n_distinct(lca_level) == 1 | reference_db == "gb") |> 
  group_by(asv_id) |> 
  group_split()

# If there still is more than one hit, find lowest common ancestor
summarised.taxa.top.nested <- lapply(summarised.taxa.top.nested, agreement.tophit) |> 
  bind_rows()

summarised.taxa.top.nested <- summarised.taxa.top.nested |> 
  # Combine duplicate hits
  group_by(asv_id, lca_rank, lca_taxon, method, kingdom, phylum, class, order, family, genus, species, lca_level) |> 
  summarise(n_dbs = length(unique(reference_db)),
            reference_db = str_c(unique(reference_db), collapse = ";"),
            method = str_c(unique(method), collapse = ";"),
            identity.min = min(identity.min),
            identity.max = max(identity.max),
            coverage.min = min(coverage.min),
            coverage.max = max(coverage.max),
            evalue.min = min(evalue.min),
            evalue.max = max(evalue.max),
            bitscore.min = min(bitscore.min),
            bitscore.max = max(bitscore.max),
            n = sum(n),
            nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  group_by(asv_id) |> 
  mutate(nsr_id = as.character(nsr_id))



# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.top.nested |> 
  group_by(asv_id) |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.top.nested)
# 38 ASVs have been identified in this way


# What ASVs are left 
taxa.top <- taxa.top |> 
  filter(!(asv_id %in% summarised.taxa.top.nested$asv_id))

taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.top.nested$asv_id))

### Get the lowest common ancestor for the other asvs
taxa.top.disagreement <- taxa.top |> 
  group_by(asv_id) |> 
  mutate(n_dbs = length(unique(reference_db)),
         reference_db = str_c(unique(reference_db), collapse = ";"),
         method = str_c(unique(method), collapse = ";"),
         identity.min = min(identity.min),
         identity.max = max(identity.max),
         coverage.min = min(coverage.min),
         coverage.max = max(coverage.max),
         evalue.min = min(evalue.min),
         evalue.max = max(evalue.max),
         bitscore.min = min(bitscore.min),
         bitscore.max = max(bitscore.max),
         n = sum(n),
         nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  group_split()

summarised.taxa.top.disagreement <- lapply(taxa.top.disagreement, agreement.tophit) |> 
  bind_rows() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.top.disagreement |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.top.disagreement)
# 10 ASVs have been identified in this way

# What ASVs are left 
taxa.top <- taxa.top |> 
  filter(!(asv_id %in% summarised.taxa.top.disagreement$asv_id))
# No taxa left

taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.top.disagreement$asv_id))

### 4. Taxa that have not been identified in at least one of the reference dbs ("no lca")
taxa.nolca <- taxa.left |> 
  filter(any(method == "no lca"))

# No identification in BOLD: follow Genbank
# No identification in GB: also follow genbank
summarised.taxa.nolca1 <- taxa.nolca |> 
  filter(reference_db == "gb") |> 
  mutate(nsr_id = as.character(nsr_id))

# Summarise Genbank hits into a single taxonomic assignment
summarised.taxa.nolca1 <- summarised.taxa.nolca1 |> 
  # Combine duplicate assignemnts
  group_by(asv_id, reference_db, lca_rank, lca_taxon, method, kingdom, phylum, class, order, family, genus, species, lca_level) |> 
  summarise(n_dbs = length(unique(reference_db)),
            reference_db = str_c(unique(reference_db), collapse = ";"),
            method = str_c(unique(method), collapse = ";"),
            identity.min = min(identity.min),
            identity.max = max(identity.max),
            coverage.min = min(coverage.min),
            coverage.max = max(coverage.max),
            evalue.min = min(evalue.min),
            evalue.max = max(evalue.max),
            bitscore.min = min(bitscore.min),
            bitscore.max = max(bitscore.max),
            n = sum(n),
            nsr_id = str_c(unique(nsr_id), collapse = ";")) |> 
  group_by(asv_id, reference_db) |> 
  group_split()

summarised.taxa.nolca1 <- lapply(summarised.taxa.nolca1, function(x) {
  if (nrow(x) > 1) {
    out <- get_tophit(x) |> 
      filter(iv.rank == 1)
    out <- agreement.tophit(out)
  } else out <- x
  return(out)
}) |> 
  bind_rows()


# Have all these ASVs been summarised into a single taxonomic assignment?
testb <- summarised.taxa.nolca1 %>%
  filter(n() >1)

taxa %>% filter(asv_id %in% testb$asv_id) 

summarised.taxa.nolca1 |> 
  group_by(asv_id, reference_db) |>
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.nolca1)
# 218 ASVs have been identified in this way

# What ASVs are left?
taxa.nolca <- taxa.nolca |> 
  filter(!(asv_id %in% summarised.taxa.nolca1$asv_id))

taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.nolca1$asv_id))


# No identification in both databases
summarised.taxa.noident <- taxa.nolca |> 
  filter(all(method == "no lca")) |> 
  mutate(reference_db = str_c(unique(reference_db), collapse = ";"),
         identity.min = NA, identity.max = NA, coverage.min = NA, coverage.max = NA, n = NA) |> 
  distinct() |> 
  mutate(nsr_id = as.character(nsr_id))

# Have all these ASVs been summarised into a single taxonomic assignment?
summarised.taxa.noident |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct
nrow(summarised.taxa.noident)
# 0 ASV has been identified in this way


# What ASVs are left?
taxa.nolca <- taxa.nolca |> 
  filter(!(asv_id %in% summarised.taxa.noident$asv_id))
# No taxa are left 

taxa.left <- taxa.left |> 
  filter(!(asv_id %in% summarised.taxa.noident$asv_id))
# No taxa left!



## Combine everything
taxatable <- list(summarised.taxa.lca.agree, 
                  summarised.taxa.lca.disagree, 
                  summarised.taxa.lca.disagree.kingdom, 
                  summarised.taxa.lca.disagree.kingdom.noident,
                  summarised.taxa.lca.disagree.level, 
                  summarised.taxa.noident, 
                  summarised.taxa.nolca1, 
                  summarised.taxa.top.agree, 
                  summarised.taxa.top.disagreement, 
                  summarised.taxa.top.nested, 
                  summarised.taxa.toplca.disagree.kingdom, 
                  summarised.taxa.toplca.disagreement,
                  summarised.taxa.wolbacha.homo, 
                  summarised.toplca.nested) |> 
  lapply(function(x) select(x, c(asv_id, lca_rank, lca_taxon, method, reference_db, 
                                 identity.min, identity.max, coverage.min, coverage.max, 
                                 evalue.min, evalue.max, bitscore.min, bitscore.max, 
                                 n, kingdom, phylum, class, order, family, genus, species, nsr_id))) |> 
  do.call(what = rbind)

## Final checks: have all ASVs been assigned to a single taxon?
taxatable |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow() == 0
# Correct!




## Are all ASVs included in the taxatable?
sum(!(unique(taxa$asv_id) %in% unique(taxatable$asv_id))) == 0
# Correct!


# Have all taxa with a taxonomic assignment been assigned a nsr id?
taxatable |> 
  filter(is.na(nsr_id) & !is.na(lca_taxon) & lca_taxon != "no identification") |> 
  nrow() == 0
# Correct  

length(unique(taxatable$asv_id))

# Calculate assignment grade and correct assignment
taxatable <- taxatable |> 
  mutate(identity.max = ifelse(is.na(identity.max), identity.min, identity.max)) |> 
  mutate(grade = 0.5*coverage.max + 0.25*identity.max + ifelse(evalue.max < 0.01, 25, 0)) |> 
  left_join(y = taxon.level, by = c("lca_rank" = "rank"))

taxatable <- taxatable |>
  ungroup() |> 
  mutate(species = ifelse(grade < 98 & lca_level == 7, NA, species),
         genus = ifelse(grade < 97 & lca_level >= 6, NA, genus),
         family = ifelse(grade < 93 & lca_level >= 5, NA, family),
         order = ifelse(grade < 80 & lca_level >= 4, NA, order),
         class = ifelse(grade < 70 & lca_level >= 3, NA, class),
         method = ifelse(grade < 98 & lca_level == 7 |
                           grade < 97 & lca_level >= 6 |
                           grade < 93 & lca_level >= 5 |
                           grade < 80 & lca_level >= 4 |
                           grade < 70 & lca_level >= 3 |
                           grade < 98 & lca_level == 7, str_c(method, "_gradecorrected"), method)) |> 
  select(-lca_level)


taxatable |> 
  filter(str_detect(method, "gradecorrected")) 

# Fix final taxon ranks
taxatable <- taxatable |> 
  ungroup() |> 
  mutate(assign_id = 1:n()) |> 
  pivot_longer(cols = 15:21, names_to = "taxon_level", values_to = "taxon") |> 
  left_join(y = taxon.level, by = c("taxon_level" = "rank")) |> 
  group_by(assign_id) |> 
  filter(!is.na(taxon)) |> 
  mutate(lca_rank = taxon_level[lca_level == max(lca_level)],
         lca_taxon = taxon[lca_level == max(lca_level)]) |> 
  select(-lca_level) |> 
  pivot_wider(names_from = "taxon_level", values_from = "taxon") |> 
  ungroup() |> 
  select(-assign_id)



### Export
write.csv(taxatable, file = "07-phyloseq/taxatable_20260729.csv", quote = FALSE, row.names = FALSE)


### NSR comparison
nsr.wide <- nsr |> 
  mutate(nsr_id = as.character(nsr_id)) |> 
  select(nsr_id, name.accepted, contains("match."), synonym)
nsr.comp <- taxatable |> 
  separate_longer_delim(nsr_id, ";") |>
  # Don't compare with nsr if the taxa could only assigned at the kingdom level
  mutate(nsr_id = ifelse(lca_rank == "kingdom", NA, nsr_id)) |> 
  left_join(y = nsr.wide, by = "nsr_id") 


# Which species are not found?
taxa.notfound <- nsr.comp |> 
  filter(!(match.type %in% c("match", "fuzzy")) & !is.na(match.type))

# Are some of these asvs assigned to animal taxa?
taxa.notfound |> 
  filter(kingdom == "Metazoa") 
# None

# Which asvs have been assigned to the a different taxonomic level than the detection in the nsr?
# Correct nsr comparison
nsr.comp <- taxon.level |> 
  left_join(x = nsr.comp, by = c("lca_rank" = "rank")) 




nsr.comp <- taxon.level |> 
  rename(match.level = lca_level) |> 
  left_join(x = nsr.comp, by = c("match.taxon.rank" = "rank")) #|> 
# Remove comparisions at a higher taxonomic level that the assigned taxonomic level
#filter(match.level >= lca_level)

# If there is any comparison where the accepted name equals the assigned taxon, only keep that comparison
nsr.comp <- nsr.comp |> 
  group_by(asv_id) |> 
  filter(!any(name.accepted == lca_taxon) | name.accepted == lca_taxon) |> 
  filter(match.type == "match" | !any(match.type == "match")) 

# Are there any cases were the nsr match is with a higher taxonomic level than the actual taxon?
# Correct the taxonomic assignment in those cases
taxatable.nsr <- nsr.comp |> 
  group_by(asv_id) |> 
  filter(match.level >= lca_level | all(match.level < lca_level)) |> 
  mutate(method = ifelse(all(match.level < lca_level), str_c(method, "_nsrcorrected"), method)) |> 
  mutate(species = ifelse(!is.na(match.level) & match.level < 7 & lca_level > 6, NA, species)) |> 
  mutate(genus = ifelse(!is.na(match.level) & match.level < 6 & lca_level > 5, NA, genus)) |> 
  mutate(family = ifelse(!is.na(match.level) & match.level < 5 & lca_level > 4, NA, family)) |> 
  mutate(order = ifelse(!is.na(match.level) & match.level < 4 & lca_level > 3, NA, order)) |> 
  mutate(class = ifelse(!is.na(match.level) & match.level < 3 & lca_level > 2, NA, class)) |> 
  mutate(phylum = ifelse(!is.na(match.level) & match.level < 2 & lca_level > 1, NA, phylum)) |> 
  mutate(kingdom = ifelse(!is.na(match.level) & match.level < 1 & lca_level > 0, NA, kingdom)) |> 
  mutate(lca_level = ifelse(!is.na(match.level) & lca_level >= match.level, match.level, lca_level)) |> 
  mutate(lca_rank = case_when(lca_level == 7 & !is.na(lca_level) ~ "species",
                              lca_level == 6 & !is.na(lca_level) ~ "genus", 
                              lca_level == 5 & !is.na(lca_level) ~ "family",
                              lca_level == 4 & !is.na(lca_level) ~ "order",
                              lca_level == 3 & !is.na(lca_level) ~ "class",
                              lca_level == 2 & !is.na(lca_level) ~ "phylum", 
                              lca_level == 1 & !is.na(lca_level) ~ "kingdom",
                              lca_level == -1 & !is.na(lca_level) ~ "no identification",
                              .default = lca_rank)) |>
  mutate(lca_taxon = case_when(lca_level == 7 & !is.na(lca_level) ~ species,
                               lca_level == 6 & !is.na(lca_level) ~ genus, 
                               lca_level == 5 & !is.na(lca_level) ~ family,
                               lca_level == 4 & !is.na(lca_level) ~ order,
                               lca_level == 3 & !is.na(lca_level) ~ class,
                               lca_level == 2 & !is.na(lca_level) ~ phylum, 
                               lca_level == 1 & !is.na(lca_level) ~ kingdom,
                               lca_level == -1 & !is.na(lca_level) ~ NA,
                               .default = lca_taxon)) 
taxatable.nsr |> 
  filter(lca_level > match.level) |> 
  nrow() == 0
# No cases left

# Are there any cases were the nsr match is with a lower taxonomic level than the actual taxon?
# Correct the nsr comparison in those cases

taxatable.nsr <- taxatable.nsr |> 
  mutate(match.genus = ifelse(!is.na(match.level) & match.level > 5 & lca_level < 6, NA, match.genus)) |> 
  mutate(match.family = ifelse(!is.na(match.level) & match.level > 4 & lca_level < 5, NA, match.family)) |> 
  mutate(match.order = ifelse(!is.na(match.level) & match.level > 3 & lca_level < 4, NA, match.order)) |> 
  mutate(match.class = ifelse(!is.na(match.level) & match.level > 2 & lca_level < 3, NA, match.class)) |> 
  mutate(match.phylum = ifelse(!is.na(match.level) & match.level > 1 & lca_level < 2, NA, match.phylum)) |> 
  mutate(match.kingdom = ifelse(!is.na(match.level) & match.level > 0 & lca_level < 1, NA, match.kingdom)) |> 
  mutate(name.accepted = case_when(lca_level == 1 & !is.na(lca_level) ~ match.kingdom,
                                   lca_level == 2 & !is.na(lca_level) ~ match.phylum,
                                   lca_level == 3 & !is.na(lca_level) ~ match.class,
                                   lca_level == 4 & !is.na(lca_level) ~ match.order,
                                   lca_level == 5 & !is.na(lca_level) ~ match.family,
                                   lca_level == 6 & !is.na(lca_level) ~ match.genus,
                                   lca_level == 7 & !is.na(lca_level) ~ name.accepted,
                                   .default = name.accepted)) |> 
  # Correct match level
  mutate(match.level = lca_level,
         match.taxon.rank =  case_when(lca_level == 1 & !is.na(lca_level) ~ "kingdom",
                                       lca_level == 2 & !is.na(lca_level) ~ "phylum",
                                       lca_level == 3 & !is.na(lca_level) ~ "class",
                                       lca_level == 4 & !is.na(lca_level) ~ "order",
                                       lca_level == 5 & !is.na(lca_level) ~ "family",
                                       lca_level == 6 & !is.na(lca_level) ~ "genus",
                                       lca_level == 7 & !is.na(lca_level) ~ "species",
                                       .default = match.taxon.rank)) |> 
  # Remove nsr id column
  select(-nsr_id) |> 
  # Remove metadata for asvs without a species-level assignment
  mutate(match.metadata = ifelse(lca_level != 7, NA, match.metadata)) |> 
  ## Extract urls from metadata
  mutate(match.url = str_extract(match.metadata, "https:.+")) |> 
  mutate(match.metadata = str_extract(match.metadata, ".+?(?=;https)")) 




# Combine all urls into a single list per asv and add to table
meta.url <- taxatable.nsr |> 
  group_by(asv_id) |> 
  summarise(match.url = list(match.url))
taxatable.nsr <- taxatable.nsr |> 
  select(-match.url) |> 
  left_join(y = meta.url, by = "asv_id") |> 
  # Remove double rows
  distinct()

# Remove fuzzy matches if there is also a true match
# Sometimes, fuzzy matches result in mistakes (e.g., asv_891 > Corella <> Chlorella)
taxatable.nsr <- taxatable.nsr |> 
  group_by(asv_id) |> 
  filter(match.type == "match" | !any(match.type == "match")) |> 
  filter(synonym == "match" | !any(synonym == "match")) 

# If asv is assigned to Oomycota, multiple nsr comparisions can exist due to differences in taxonomy across databases.
# Choose the hit with where the matched phylum is Oomycota, set higher taxonomic levels to NA
taxatable.nsr <- taxatable.nsr |> 
  group_by(asv_id) |>
  #filter(lca_taxon == "Oomycota")
  filter(!(n() > 1 & lca_taxon == "Oomycota" & match.phylum != "Oomycota")) |> 
  mutate(match.class = ifelse(lca_taxon == "Oomycota" & match.phylum == "Oomycota", NA, match.class),
         name.accepted = ifelse(lca_taxon == "Oomycota" & match.phylum == "Oomycota", "Oomycota", name.accepted))
# # If asv is assigned to Heterokontophyta, multiple nsr comparisions can exist. Choose one, remove Oomycota
# taxatable.nsr <- taxatable.nsr |> 
#   group_by(asv_id) |> 
#   filter(!(n() > 1 & lca_taxon == "Heterokontophyta" & name.accepted == "Oomycota"))

# If there is an exact match, use that match
taxatable.nsr <- taxatable.nsr |> 
  group_by(asv_id) |> 
  filter(!any(name.accepted == lca_taxon) | name.accepted == lca_taxon)

# If there are multiple hits, choose the one with the lowest taxonomic level
taxatable.nsr <- taxatable.nsr |> 
  group_by(asv_id) |> 
  filter(match.level == max(match.level))


# Have all taxa again been assigned to a single taxon?
taxatable.nsr |> 
  select(-match.url) |> 
  group_by(asv_id) |> 
  filter(n() > 1) 

# two cases left, where the order Rhabditida is matched to both Ascaridida and Strongylida. Both are synomyms of Rhabditida
taxatable.nsr <- taxatable.nsr |> 
  mutate(name.accepted = ifelse(asv_id %in% c("Zotu1954", "Zotu2964"), "Rhabditida", name.accepted),
         match.order = ifelse(asv_id %in% c("Zotu1954", "Zotu2964"), "Rhabditida", match.order)) |> 
  distinct()
taxatable.nsr |> 
  select(-match.url) |> 
  group_by(asv_id) |> 
  filter(n() > 1) |> 
  nrow() == 0
# Correct

## Are all ASVs included in the taxatable?
sum(!(taxatable$asv_id) %in% unique(taxatable.nsr$asv_id)) == 0
# No

# Add asvs that could not be compared to nsr as no identification
taxatable.nsr <- taxatable |> 
  filter(!(asv_id %in% taxatable.nsr$asv_id)) |> 
  mutate(kingdom = NA, phylum = NA, class = NA, order = NA, family = NA, genus = NA, species = NA,
         lca_level = -1, lca_rank = "no identification", lca_taxon = "no identification", nsr_id = NA,
         reference_db = NA, method = "no_nsr") |> 
  bind_rows(taxatable.nsr)

# Find asvs assigned to non-native species
taxatable.nsr |> 
  filter(lca_level == 7 & !str_detect(match.metadata, "^1a")) |> 
  mutate(test = str_extract(match.metadata, "")) 

taxatable.nsr.nourl <- taxatable.nsr |> 
  select(-match.url)


## Are all ASVs included in the taxatable?
sum(!(taxatable$asv_id) %in% unique(taxatable.nsr.nourl$asv_id)) == 0
# Yes

# Export
write.csv(taxatable.nsr.nourl, file = "07-phyloseq/taxatable_nsr_20260729.csv", quote = FALSE, row.names = FALSE)














