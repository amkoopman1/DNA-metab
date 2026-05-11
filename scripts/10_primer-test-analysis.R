## processing results of the primer test

library(phyloseq)

# this script is dependent on file created by script 09 

setwd("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test")

# load taxatable (Zotu x taxonomy) from file ( /07-phyloseq)
# file created in script 09
{
  
  taxa_data <- read.csv("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/07-phyloseq/taxatable_20260505.csv") %>%
  select(c(asv_id, kingdom, phylum, class, order, family, genus, species)) %>%
  column_to_rownames("asv_id") %>%
  as.matrix() %>%
  tax_table()

}

# load ASVs data

{
# load ASVs from file ( /03-asvs )
asv_files <- list(
  jusino  = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asv_tab-coi_jusino-0.98-20260504.txt",
  leray   = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asv_tab-coi_leray-0.98-20260504.txt",
  verkuil = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asv_tab-coi_verkuil-0.98-20260504.txt"
)

asv_data <- imap(asv_files, ~ read_tsv(.x) %>%
                rename(asv_id = `#OTU ID`) %>%
                  mutate(asv_id = str_c(asv_id, .y, sep = "_")) %>%
                pivot_longer(cols = -asv_id, names_to = "pcr_id", values_to = "nreads")) %>%
  list_rbind() %>%
  pivot_wider(names_from = "pcr_id", values_from = "nreads", values_fill = 0) %>%
  column_to_rownames("asv_id") %>%
  as.matrix() %>%
  otu_table(asv_data, taxa_are_rows = TRUE)
}

# load metadata

{
  database_primer_url <- "https://docs.google.com/spreadsheets/d/1NwqaNFbSUDX1Ql7RzCBbkkSWXrom6PsiYlW9_qKW6cI"

database_primer <- read_gsdb(database_primer_url)
names(database_primer)


# calculate mass and nano ratio per pcr_ID contents

# calculate mass per sample
ratio_data <- database_primer[["FactMashMass"]] %>%
  mutate(across(mass_empty_1:mass_dry_3, as.numeric)) %>%
  left_join(
    database_primer[["DimSample"]] %>% 
      group_by(ID_mash) %>% 
      summarise(
        n = n() * first(volume2mashtube)
      ),
    by = "ID_mash"
  ) %>%
  mutate(
    # % sample left after 45 minutes of extra evaporation (compared to after drying once)
    dry_corr   = (mass_dry_2-mass_empty_2)/(mass_wet_2-mass_empty_2),
    # corrected mass for 'undry' sample
    mass_used = (mass_dry_used-mass_empty_1) * ifelse(is.na(dry_corr), 1, dry_corr),
    mass_sample = (mass_dry_1-mass_empty_1)* ifelse(is.na(dry_corr), 1, dry_corr)) %>%
  select(ID_mash,mass_used) %>%

# calculate ratios in mock mix for PCR

# match sample extraction ID to dry mass of sample
  inner_join(database_primer[["DimSample"]], by = "ID_mash") %>%
  select("ID_mash", "Extraction_ID", "mass_used") %>%
  distinct() %>%
# match sample extraction ID to uL added and ID_mix_PCR
  inner_join(database_primer[["DimMock"]], by = c("Extraction_ID" = "content")) %>%
# match nanodrop concentration to Extraction_ID
  inner_join(database_primer[["FactNanodrop"]], by = "Extraction_ID") %>%
  select("ID_mash","Extraction_ID","mass_used","ID_mix_PCR","uL_added","metadata", "conc_nano") %>%
  mutate(conc_nano = as.numeric(conc_nano)) %>%
# calculate total mass and nano per ID_mix_PCR
  group_by(ID_mix_PCR) %>% 
  mutate(
    total_mass = sum((mass_used*(
      uL_added/50))), # mass_used is in total 50 uL of extraction
    total_nano = sum((conc_nano*(
      uL_added))) # conc_nano is already per uL
  ) %>% ungroup() %>%
# calculate sample mass and nano ratio per ID_mix_PCR 
  mutate(
    mass_ratio = (mass_used*
      uL_added/50)/total_mass,
    nano_ratio = conc_nano*uL_added/
      total_nano
  ) %>% select("ID_mix_PCR","metadata", "mass_ratio", "nano_ratio")%>%
# add PT_04 (the only fecal sample)
  bind_rows(
    database_primer[["DimMock"]] %>%
      filter(ID_mix_PCR == "PT_04") %>%
      mutate(nano_ratio = NA, mass_ratio = NA, metadata)  # fill missing cols with NA
  ) %>% select(-c(content, uL_added))

# match pcr_ID to primer and contents
meta_data <- ratio_data %>%
  inner_join(database_primer[["FactPCR"]], by = c("ID_mix_PCR" = "ID_mix_mock")) %>%
  select("pcr_ID", 	"ID_mix_primer",	"ID_mix_PCR") %>%
  distinct() %>%
  column_to_rownames("pcr_ID") %>%
  sample_data()


}

# set ggplot visuals

{

# cols6 <-  c(
#     "#1F2421", "#9CC5A1", "#204645",
#     "#DCE1DE", "#49A078","#216869"
#   )
  
cols6 <- c("#3E885B", "#7FC29B","#204645","#817E9F", "#BEDCFE", "#3E78B2")
  
  # Set ggplot theme
  theme_replace(text = element_text(size = 14),
                strip.background = element_blank(),
                strip.text = element_text(margin = margin(3.5, 3.5, 3.5, 3.5)),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", linewidth = 0.5, fill = NA),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.justification = c("left", "center"),
                legend.text = element_text(),
                legend.key = element_blank(),
                axis.ticks = element_line(linewidth = 0.25),
                axis.ticks.length = unit(0.075, "cm"),
                axis.line = element_blank(),
                axis.text = element_text())

}

## Construct phyloseq object
sequence_data <-  phyloseq(asv_data, taxa_data, meta_data) # taxonomy, reads and metadata

# start analysis

rra_data <- psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
  #select(OTU, seq_id, sample_id, sample_type, kingdom, phylum, class, order, family, genus, species, n_reads) %>% 
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
  filter(!(ID_mix_primer == "primer_1" & read_adapter == "jusino")) %>%
  select(-read_adapter)

# filter out small reads
# d.all %>% 
#   filter(is.na(order) & rra > 0.2) %>% 
#   ungroup() %>% 
#   distinct(OTU) 
# d.all %>% 
#   filter(is.na(family) & rra > 0.2) %>% 
#   ungroup() %>% 
#   distinct(OTU)



## Plot read counts
readsums <- tibble(nreads = sort(sample_sums(sequence_data), decreasing = TRUE), sorted = 1:nsamples(sequence_data), type = "samples") %>% 
  bind_rows(tibble(nreads = sort(taxa_sums(sequence_data), decreasing = TRUE), sorted = 1:ntaxa(sequence_data), type = "asvs"))

title = "Overview number of reads for all samples"
p = ggplot(readsums, aes(x = sorted, y = nreads)) + geom_bar(stat = "identity")
p + ggtitle(title) + scale_y_log10() + facet_wrap(~type, 1, scales = "free")


## Diversity as a function of number of reads
sample_sums_sequence_tib <- sample_sums(sequence_data) %>%
  as_tibble() %>%
  rename(total_reads = value) %>%
  mutate(pcr_ID = rownames(meta_data))

richness_tib <- estimate_richness(sequence_data, measures =c("Observed", "Shannon")) %>%
  mutate(pcr_ID = rownames(meta_data)) %>%
  left_join(y = sample_sums_sequence_tib, by = "pcr_ID") %>% 
  pivot_longer(1:2, names_to = "metric", values_to = "diversity")


ggplot(richness_tib, aes(total_reads, diversity)) +
  facet_wrap(~metric, scales = "free") +
  #geom_smooth(method = "lm") +
  geom_point() +
  scale_x_log10() 
# No strong pattern


### Other plots to make:
### Relative read abundance in mocks in categories Fungi, Aves, other metazoan and other
# Agglomerate  taxa at the level of class
sequence_data_class <- tax_glom(sequence_data, taxrank = "class", NArm = FALSE) %>% transform_sample_counts(fun = function(x) x / sum(x))

# 'Melt' phyloseq object
sequence_class_tib <- psmelt(sequence_data_class) %>% 
  as_tibble() %>% 
  # Determine categories for plat
  mutate(class_cat = case_when(class == "Aves" ~ "Aves",
                               kingdom == "Metazoa" ~ "other metazoan",
                               kingdom == "Fungi" ~ "Fungi",
                               .default = "other")) %>%
  mutate(ID_mix_primer = factor(ID_mix_primer, levels = sort(unique(ID_mix_primer))))

# Plot
ggplot(sequence_class_tib, aes(Sample, Abundance, fill = class_cat)) + 
  #facet_grid(~ Sample , scale = "free_x", space = "free_x") +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual(values = cols6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# Aves blocking primer seems to work! 

# What are the Aves reads?
rra_data %>% 
  filter(class == "Aves") %>% 
  ungroup() %>% 
  distinct(kingdom, phylum, class, order, family, genus, species)

# how much human per sample?
sequence_data_human <- subset_taxa(sequence_data, order == "Primates") %>%
  prune_samples(sample_sums(.) > 0, .)


## Do samples have reads from the correct primers?
rra_data_adapter <- rra_data |> 
  mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) |> 
  group_by(read_adapter, Sample, ID_mix_primer) |> 
  summarise(n_reads = sum(n_reads)) 

# Plot
ggplot(rra_data_adapter, aes(Sample, n_reads, fill = read_adapter)) +
  facet_wrap(~ ID_mix_primer, scales = "free_x") +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Verkuil has Jusino reads: why?


# compare ability to read orders

# construct dataframe and graph of exptected order distribution for each mix

# ratio_data %>% filter(ID_mix_PCR == "PT_02") %>%
#   ggplot(aes(x = ID_mix_PCR, y = mass_ratio, fill = metadata)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(x = NULL) + scale_fill_manual(values = cols6) + ggtitle(c("Expected distribution"))
# 
# 
#  rra_data %>%
#   filter(ID_mix_PCR == "PT_02", order %in% c("Araneae", "Diptera", "Hymenoptera", "Lepidoptera", "Orthoptera")) %>%
#   mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
#   group_by(read_adapter, ID_mix_primer, order) %>%
#   summarise(n_reads = sum(n_reads)) %>%
#    filter(n_reads != 0) %>%
#    ggplot(aes(x = ID_mix_primer, y = n_reads, fill = order)) +
#    geom_bar(stat = "identity", position = "fill") +
#    labs(x = NULL) + scale_fill_manual(values = cols6)

 
 # PT_02
 
 bind_rows(
   ratio_data %>%
     filter(ID_mix_PCR == "PT_02") %>%
     mutate(metadata = recode(metadata,
                              "Leptophyes punctatissima" = "Orthoptera",
                              "Conocephalus dorsalis" = "Orthoptera"
     )) %>%
     rename(order = metadata, proportion = nano_ratio) %>%
     mutate(ID_mix_primer = "Expected"),
   
   rra_data %>%
     filter(ID_mix_PCR == "PT_02", order %in% c("Araneae", "Diptera", "Hymenoptera", "Lepidoptera", "Orthoptera")) %>%
     mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
     group_by(ID_mix_primer, order) %>%
     summarise(n_reads = sum(n_reads)) %>%
     filter(n_reads != 0) %>%
     group_by(ID_mix_primer) %>%
     mutate(proportion = n_reads / sum(n_reads))
 ) %>%
   mutate(ID_mix_primer = factor(ID_mix_primer, levels = c("Expected", sort(unique(ID_mix_primer[ID_mix_primer != "Expected"]))))) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = order)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "proportion") + scale_fill_manual(values = cols6) +
   scale_x_discrete(labels = c("Expected" = "Expected", "primer_1" = "Verkuil", "primer_2" = "Jusino", "primer_3" = "Leray", "primer_4" = "Leray - fungi", "primer_5" = "Leray - aves", "primer_6" = "Leray - fungi/aves" )) + ggtitle(c("Mock community \norder distribution"))
 
 
 # PT_03
 
 bind_rows(
   ratio_data %>%
     filter(ID_mix_PCR == "PT_03") %>%
     mutate(metadata = recode(metadata,
                              "Conocephalus dorsalis" = "Orthoptera"
     )) %>%
     rename(order = metadata, proportion = mass_ratio) %>%
     mutate(ID_mix_primer = "Expected"),
   
   rra_data %>%
     filter(ID_mix_PCR == "PT_03", order %in% c("Lepidoptera", "Orthoptera")) %>%
     mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
     group_by(ID_mix_primer, order) %>%
     summarise(n_reads = sum(n_reads)) %>%
     filter(n_reads != 0) %>%
     group_by(ID_mix_primer) %>%
     mutate(proportion = n_reads / sum(n_reads))
 ) %>%
   mutate(ID_mix_primer = factor(ID_mix_primer, levels = c("Expected", sort(unique(ID_mix_primer[ID_mix_primer != "Expected"]))))) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = order)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "proportion") + scale_fill_manual(values = cols6) +
   scale_x_discrete(labels = c("Expected" = "Expected", "primer_1" = "Verkuil", "primer_2" = "Jusino", "primer_3" = "Leray", "primer_4" = "Leray - fungi", "primer_5" = "Leray - aves", "primer_6" = "Leray - fungi/aves" )) + ggtitle(c("50/50 mix Lepidoptera - Conocephalus dorsalis \norder distribution"))
 
 
 # PT_04

 rra_data %>%
   filter(ID_mix_PCR == "PT_04", order %in% c("Araneae", "Diptera", "Hymenoptera", "Lepidoptera", "Orthoptera")) %>%
   mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
   group_by(ID_mix_primer, order) %>%
   summarise(n_reads = sum(n_reads)) %>%
   filter(n_reads != 0) %>%
   group_by(ID_mix_primer) %>%
   mutate(proportion = n_reads / sum(n_reads)) %>%
  ggplot(aes(x = ID_mix_primer, y = proportion, fill = order)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "proportion") + scale_fill_manual(values = cols6) +
  scale_x_discrete(labels = c("primer_1" = "Verkuil", "primer_2" = "Jusino", "primer_3" = "Leray", "primer_4" = "Leray - fungi", "primer_5" = "Leray - aves", "primer_6" = "Leray - fungi/aves" )) + ggtitle(c("Fecal sample sedge warbler \norder distribution"))

 
 
 
 # compare taxonomic resolution
 
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_02", 
          phylum == "Arthropoda") %>%
   mutate(
     lowest_tax = case_when(
       !is.na(species) & species != "" ~ "species",
       !is.na(genus)   & genus   != "" ~ "genus",
       !is.na(family)  & family  != "" ~ "family",
       !is.na(order)   & order   != "" ~ "order",
       !is.na(class)   & class   != "" ~ "class",
       !is.na(phylum)  & phylum  != "" ~ "phylum",
       TRUE                            ~ "kingdom"
     ),
     lowest_tax = factor(lowest_tax, levels = c("species", "genus", "family", "order", "class", "phylum", "kingdom"))
   ) %>%
   group_by(ID_mix_primer, lowest_tax) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   filter(n_reads != 0) %>%
   group_by(ID_mix_primer) %>%
   mutate(
     proportion = n_reads / sum(n_reads)) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = lowest_tax)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "Proportion", fill = "Lowest taxonomic\nresolution") +
   scale_fill_manual(values = cols6) +
   scale_x_discrete(labels = c(
     "primer_1"  = "Verkuil",
     "primer_2"  = "Jusino",
     "primer_3"  = "Leray",
     "primer_4"  = "Leray - fungi",
     "primer_5"  = "Leray - aves",
     "primer_6"  = "Leray - fungi/aves"
   )) +
   ggtitle("Mock community \n taxonomic resolution Arthropoda") 
 
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_04", 
          phylum == "Arthropoda") %>%
   mutate(
     lowest_tax = case_when(
       !is.na(species) & species != "" ~ "species",
       !is.na(genus)   & genus   != "" ~ "genus",
       !is.na(family)  & family  != "" ~ "family",
       !is.na(order)   & order   != "" ~ "order",
       !is.na(class)   & class   != "" ~ "class",
       !is.na(phylum)  & phylum  != "" ~ "phylum",
       TRUE                            ~ "kingdom"
     ),
     lowest_tax = factor(lowest_tax, levels = c("species", "genus", "family", "order", "class", "phylum", "kingdom"))
   ) %>%
   group_by(ID_mix_primer, lowest_tax) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   filter(n_reads != 0) %>%
   group_by(ID_mix_primer) %>%
   mutate(
     proportion = n_reads / sum(n_reads)) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = lowest_tax)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "Proportion", fill = "Lowest taxonomic\nresolution") +
   scale_fill_manual(values = cols6) +
   scale_x_discrete(labels = c(
     "primer_1"  = "Verkuil",
     "primer_2"  = "Jusino",
     "primer_3"  = "Leray",
     "primer_4"  = "Leray - fungi",
     "primer_5"  = "Leray - aves",
     "primer_6"  = "Leray - fungi/aves"
   )) +
   ggtitle("Fecal sample sedge warbler \n taxonomic resolution Arthropoda") 

 
 
 # graph diversity for reads

 # mock community
 rra_data %>%
   filter(ID_mix_PCR == "PT_02", 
         phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") +
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Mock community \nShannon diversity reads")
 
 # Orthoptera Lepidoptera mix
 rra_data %>%
   filter(ID_mix_PCR == "PT_03", 
          phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") +
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Orthoptera Lepidoptera mix \nShannon diversity reads")
 
 
 # fecal sample
 rra_data %>%
   filter(ID_mix_PCR == "PT_04", 
          phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") + 
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Fecal sample \nShannon diversity reads")
 
 
 # graph diversity for species
 
 # mock community
 rra_data %>%
   filter(ID_mix_PCR == "PT_02", 
          phylum == "Arthropoda",
          !is.na(species)) %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") +
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Mock community \nShannon diversity species")
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_02", phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, species) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   filter(n_reads != 0, !is.na(species), species != "") %>%
   group_by(ID_mix_primer) %>%
   summarise(species_richness = n_distinct(species)) %>%
   ggplot(aes(x = ID_mix_primer, y = species_richness)) +
   geom_col(fill = "#E69F00") +
   geom_text(aes(label = species_richness), vjust = -0.5) +
   labs(x = NULL, y = "Species richness") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Mock community \nArthropoda species richness")
 
 # Orthoptera Lepidoptera mix
 rra_data %>%
   filter(ID_mix_PCR == "PT_03", 
          phylum == "Arthropoda",
          !is.na(species)) %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") +
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Orthoptera Lepidoptera mix \nShannon diversity species")
  
 rra_data %>%
   filter(ID_mix_PCR == "PT_03", phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, species) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   filter(n_reads != 0, !is.na(species), species != "") %>%
   group_by(ID_mix_primer) %>%
   summarise(species_richness = n_distinct(species)) %>%
   ggplot(aes(x = ID_mix_primer, y = species_richness)) +
   geom_col(fill = "#E69F00") +
   geom_text(aes(label = species_richness), vjust = -0.5) +
   labs(x = NULL, y = "Species richness") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Orthoptera Lepidoptera mix \nArthropoda species richness")
 
 # fecal sample
 rra_data %>%
   filter(ID_mix_PCR == "PT_04", 
          phylum == "Arthropoda",
          !is.na(species)) %>%
   group_by(ID_mix_primer, OTU) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
   column_to_rownames("ID_mix_primer") %>%
   diversity(index = "shannon") %>%
   as.data.frame() %>%
   rownames_to_column("ID_mix_primer") %>%
   pivot_longer(-ID_mix_primer, names_to = "metric", values_to = "shannon") %>%
   ggplot(aes(x = ID_mix_primer, y = shannon)) +
   geom_col(fill = "seagreen") + 
   labs(x = NULL, y = "Shannon diversity") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Fecal sample \nShannon diversity Arthropoda species")
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_04", phylum == "Arthropoda") %>%
   group_by(ID_mix_primer, species) %>%
   summarise(n_reads = sum(n_reads), .groups = "drop") %>%
   filter(n_reads != 0, !is.na(species), species != "") %>%
   group_by(ID_mix_primer) %>%
   summarise(species_richness = n_distinct(species)) %>%
   ggplot(aes(x = ID_mix_primer, y = species_richness)) +
   geom_col(fill = "#E69F00") +
   geom_text(aes(label = species_richness), vjust = -0.5) +
   labs(x = NULL, y = "Species richness") +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Fecal sample sedge warbler \nArthropoda species richness")
 
 #proportion of reads and target reads
 
 # mock community
 rra_data %>%
   filter(ID_mix_PCR == "PT_02") %>%
   group_by(ID_mix_primer, phylum) %>%
   summarise(sum_reads = sum(n_reads), .groups = "drop") %>%
   mutate(is_arthropoda = phylum == "Arthropoda") %>%
   group_by(ID_mix_primer) %>%
   mutate(
     prop  = sum_reads / sum(sum_reads),
     label = ifelse(is_arthropoda, paste0(round(prop * 100, 1), "%"), NA)
   ) %>%
   ggplot(aes(x = ID_mix_primer, y = sum_reads, fill = is_arthropoda)) +
   geom_col() +
   geom_text(aes(label = label),
             position = position_stack(vjust = 1.05),
             na.rm = TRUE) +
   labs(x = NULL, y = "Number of reads", fill = NULL) +
   scale_fill_manual(values = c("TRUE" = "#E69F00", "FALSE" = "grey70"),
                     labels = c("TRUE" = "Arthropoda", "FALSE" = "Other")) +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Mock community \ntotal reads and target reads")
 
 
 # fecal sample
 rra_data %>%
   filter(ID_mix_PCR == "PT_04") %>%
   group_by(ID_mix_primer, phylum) %>%
   summarise(sum_reads = sum(n_reads), .groups = "drop") %>%
   mutate(is_arthropoda = phylum == "Arthropoda") %>%
   group_by(ID_mix_primer) %>%
   mutate(
     prop  = sum_reads / sum(sum_reads),
     label = ifelse(is_arthropoda, paste0(round(prop * 100, 1), "%"), NA)
   ) %>%
   ggplot(aes(x = ID_mix_primer, y = sum_reads, fill = is_arthropoda)) +
   geom_col() +
   geom_text(aes(label = label),
             position = position_stack(vjust = 1.05),
             na.rm = TRUE) +
   labs(x = NULL, y = "Number of reads", fill = NULL) +
   scale_fill_manual(values = c("TRUE" = "#E69F00", "FALSE" = "grey70"),
                     labels = c("TRUE" = "Arthropoda", "FALSE" = "Other")) +
   scale_x_discrete(labels = c(
     "primer_1" = "Verkuil",
     "primer_2" = "Jusino",
     "primer_3" = "Leray",
     "primer_4" = "Leray - fungi",
     "primer_5" = "Leray - aves",
     "primer_6" = "Leray - fungi/aves"
   )) +
   ggtitle("Fecal sample \ntotal reads and target reads")
 
 
 
 # species distribution PT_03
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_03", phylum == "Arthropoda") %>%
   mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
   group_by(ID_mix_primer, species) %>%
   summarise(n_reads = sum(n_reads)) %>%
   filter(n_reads != 0) %>%
   group_by(ID_mix_primer) %>%
   mutate(proportion = n_reads / sum(n_reads)) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = species)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "proportion") + 
   scale_x_discrete(labels = c("primer_1" = "Verkuil", "primer_2" = "Jusino", "primer_3" = "Leray", "primer_4" = "Leray - fungi", "primer_5" = "Leray - aves", "primer_6" = "Leray - fungi/aves" )) + ggtitle(c("Lepidoptera Orthoptera mix \nArthropoda species distribution")) +theme(legend.text = element_text(size = 10))
 
 
 # species distribution contamination PT_03
 
 rra_data %>%
   filter(ID_mix_PCR == "PT_03", phylum == "Arthropoda", !species %in% c("Deltote uncula", "Conocephalus dorsalis"), !is.na(species)) %>%
   mutate(read_adapter = str_split_fixed(OTU, "_", 2)[,2]) %>%
   group_by(ID_mix_primer, species) %>%
   summarise(n_reads = sum(n_reads)) %>%
   filter(n_reads != 0) %>%
   group_by(ID_mix_primer) %>%
   mutate(proportion = n_reads / sum(n_reads)) %>%
   ggplot(aes(x = ID_mix_primer, y = proportion, fill = species)) +
   geom_bar(stat = "identity", position = "stack") +
   labs(x = NULL, y = "proportion") + 
   scale_x_discrete(labels = c("primer_1" = "Verkuil", "primer_2" = "Jusino", "primer_3" = "Leray", "primer_4" = "Leray - fungi", "primer_5" = "Leray - aves", "primer_6" = "Leray - fungi/aves" )) + ggtitle(c("Lepidoptera Orthoptera mix \nArthropoda species distribution")) +theme(legend.text = element_text(size = 10))
 