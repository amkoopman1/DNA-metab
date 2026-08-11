## processing results of the primer test

library(phyloseq)

# this script is dependent on file created by script 13 

setwd("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2")

# load taxatable (Zotu x taxonomy) from file ( /07-phyloseq)
# file created in script 09
{
  
  taxa_data <- read.csv("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2/07-phyloseq/taxatable_20260729.csv") %>%
    select(c(asv_id, kingdom, phylum, class, order, family, genus, species)) %>%
    column_to_rownames("asv_id") %>%
    as.matrix() %>%
    tax_table()
  
}

# load ASVs data

{
  # load ASVs from file ( /03-asvs )
  asv_files <- list(
    leray  = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2/03-asvs/asv_tab-coi_leray-0.98-20260729.txt",
    verkuil_mod1  = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2/03-asvs/asv_tab-coi_verkuil_mod1-0.98-20260729.txt",
    verkuil_mod2  = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2/03-asvs/asv_tab-coi_verkuil_mod2-0.98-20260729.txt",
    verkuil_og  = "C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_2/primer_test_2/03-asvs/asv_tab-coi_verkuil_og-0.98-20260729.txt"
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

# enter metadata

meta_data <- data.frame(
  f_sample_ID = c("KB22022", "KB22022", "KB22022", "KB22022", "KB22101", "KB22101", "KB22101"),
  primer_ID   = c("verkuil_og", "leray_48", "verkuil_mod2", "verkuil_mod1", "leray_48", "leray_52", "leray_56"),
  row.names = sort(sample_names(asv_data))
) %>% sample_data()

# set ggplot visuals

{
  
  # cols6 <-  c(
  #     "#1F2421", "#9CC5A1", "#204645",
  #     "#DCE1DE", "#49A078","#216869"
  #   )
  
  cols6 <- c("#3E885B", "#7FC29B","#204645","#817E9F", "#BEDCFE", "#3E78B2")
  
  cols8 <- c("#3E885B", "#7FC29B","#A88B3E", "#D9C48C", "#D98CB3" ,"#817E9F", "#BEDCFE", "#3E78B2")
  
  cols14 <- c(
    "#204645", "#5C9C99",  
    "#817E9F", "#BEB8D6", 
    "#7FC29B", "#D9C48C",   
    "#A33E72", "#D98CB3", 
    "#3E885B", "#A88B3E",
    "#3E78B2", "#8CC5E3",
    "#B25B3E", "#E3A98C"  
  )
  
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
# select() %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order) & rra > 0.0001) # filter out very small reads and reads without order

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
# 3 clusters


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
  mutate(primer_ID = factor(primer_ID, levels = sort(unique(primer_ID))))

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
  group_by(read_adapter, Sample, primer_ID) |> 
  summarise(n_reads = sum(n_reads)) 

# Plot
ggplot(rra_data_adapter, aes(Sample, n_reads, fill = read_adapter)) +
  facet_wrap(~ primer_ID, scales = "free_x") +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# compare ability to read 

rra_data %>%
  group_by(Sample, kingdom, f_sample_ID) %>%
  summarise(n_reads = sum(n_reads)) %>%
  filter(n_reads != 0) %>%
  group_by(Sample, f_sample_ID) %>%
  mutate(proportion = n_reads / sum(n_reads),
         Sample = factor(Sample,
                         levels = c( c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035")))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = kingdom)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "proportion") +
  scale_fill_manual(values = cols6) +
  facet_grid(~ f_sample_ID, scales = "free_x", space = "free_x")+
  scale_x_discrete(labels = c(
    "pcrAK26011" = "verkuil_og",
    "pcrAK26014"   = "leray_48",
    "pcrAK26025"   = "verkuil_mod2",
    "pcrAK26027"   = "verkuil_mod1",
    "pcrAK26033"   = "leray_48",
    "pcrAK26034"   = "leray_52",
    "pcrAK26035"   = "leray_56"
  ))+
  ggtitle(c("Fecal sample warbler \nkingdom distribution"))

rra_data %>%
  filter(kingdom == "Metazoa") %>%
  group_by(Sample, phylum, f_sample_ID) %>%
  summarise(n_reads = sum(n_reads)) %>%
  filter(n_reads != 0) %>%
  group_by(Sample,f_sample_ID) %>%
  mutate(proportion = n_reads / sum(n_reads),
         Sample = factor(Sample,
                         levels = c( c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035")))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "proportion") +
  scale_fill_manual(values = cols6) +
  facet_grid(~ f_sample_ID, scales = "free_x", space = "free_x")+
  scale_x_discrete(labels = c(
    "pcrAK26011" = "verkuil_og",
    "pcrAK26014"   = "leray_48",
    "pcrAK26025"   = "verkuil_mod2",
    "pcrAK26027"   = "verkuil_mod1",
    "pcrAK26033"   = "leray_48",
    "pcrAK26034"   = "leray_52",
    "pcrAK26035"   = "leray_56"
  ))+
  ggtitle(c("Fecal sample warbler \nphylum distribution Metazoa"))

rra_data %>%
  filter(phylum %in% c("Chordata", "Arthropoda", "Mollusca")) %>%
  group_by(Sample, order,f_sample_ID) %>%
  summarise(n_reads = sum(n_reads)) %>%
  filter(n_reads != 0) %>%
  group_by(Sample,f_sample_ID) %>%
  mutate(proportion = n_reads / sum(n_reads),
         Sample = factor(Sample,
         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = order)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "proportion") +
  facet_grid(~ f_sample_ID, scales = "free_x", space = "free_x")+
  scale_fill_manual(values = cols14) +
  scale_x_discrete(
    labels = c(
    "pcrAK26011" = "verkuil_og",
    "pcrAK26025"   = "verkuil_mod2",
    "pcrAK26027"   = "verkuil_mod1",
    "pcrAK26014"   = "leray_48",
    "pcrAK26033"   = "leray_48",
    "pcrAK26034"   = "leray_52",
    "pcrAK26035"   = "leray_56"
  ))+
  ggtitle(c("Fecal sample warbler \norder distribution"))


rra_data %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample, order,f_sample_ID) %>%
  summarise(n_reads = sum(n_reads)) %>%
  filter(n_reads != 0) %>%
  group_by(Sample,f_sample_ID) %>%
  mutate(proportion = n_reads / sum(n_reads),
         Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = order)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "proportion") +
  scale_fill_manual(values = cols14) +
  facet_grid(~ f_sample_ID, scales = "free_x", space = "free_x")+
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle(c("Fecal sample warbler Arthropoda + Mollusca \norder distribution"))



# compare taxonomic resolution


rra_data %>%
  filter(phylum %in% c("Arthropoda","Mollusca")) %>%
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
  group_by(primer_ID, lowest_tax, f_sample_ID,Sample) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  filter(n_reads != 0) %>%
  group_by(primer_ID,f_sample_ID,Sample) %>%
  mutate(proportion = n_reads / sum(n_reads),
         Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = lowest_tax)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "Proportion", fill = "Lowest taxonomic\nresolution") +
  scale_fill_manual(values = cols6) +
  facet_grid(~ f_sample_ID, scales = "free_x", space = "free_x")+
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Taxonomic resolution Arthropoda + Mollusca") 


# plot diversity

# species richness rra > 0.00001

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order) & rra > 0.00001) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  filter(n_reads != 0, !is.na(species), species != "") %>%
  group_by(Sample) %>%
  summarise(species_richness = n_distinct(species)) %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = species_richness)) +
  geom_col(fill = "#E69F00") +
  geom_text(aes(label = species_richness), vjust = -0.5) +
  labs(x = NULL, y = "Species richness") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Species richness Arthropoda + Mollusca \nrra > 0.00001")



# species richness rra > 0.001
psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order) & rra > 0.001) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  filter(n_reads != 0, !is.na(species), species != "") %>%
  group_by(Sample) %>%
  summarise(species_richness = n_distinct(species)) %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = species_richness)) +
  geom_col(fill = "#E69F00") +
  geom_text(aes(label = species_richness), vjust = -0.5) +
  labs(x = NULL, y = "Species richness") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Species richness Arthropoda + Mollusca \nrra > 0.001")

# species richness and 0.01 rra 

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order) & rra > 0.0001) %>%
  filter(phylum %in% c("Arthropoda", "Mollusca"),
         rra > 0.01) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  filter(n_reads != 0, !is.na(species), species != "") %>%
  group_by(Sample) %>%
  summarise(species_richness = n_distinct(species)) %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = species_richness)) +
  geom_col(fill = "#E69F00") +
  geom_text(aes(label = species_richness), vjust = -0.5) +
  labs(x = NULL, y = "Species richness") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Species richness Arthropoda + Mollusca \nrra > 0.01")


# graph diversity for species

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order)) %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = species, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "seagreen") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Shannon diversity species Arthropoda + Mollusca")


# rra > 0.01

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order) & rra > 0.0001) %>%
  filter(phylum %in% c("Arthropoda", "Mollusca"),
         rra > 0.01) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = species, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "seagreen") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Shannon diversity species Arthropoda + Mollusca \nrra > 0.01")

# rra < 0.001

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order)) %>%
  filter(phylum %in% c("Arthropoda", "Mollusca"),
         rra > 0.001) %>%
  group_by(Sample, species) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = species, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "seagreen") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    ))+
  ggtitle("Shannon diversity species Arthropoda + Mollusca \nrra > 0.001")


# graph diversity for reads

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  group_by(Sample, OTU) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "#7FC29B") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    )) +
  ggtitle("Shannon diversity reads Arthropoda + Mollusca \nrra > 0")

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(rra > 0.001) %>%
  group_by(Sample, OTU) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "#7FC29B") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    )) +
  ggtitle("Shannon diversity reads Arthropoda + Mollusca \nrra > 0.001")


  psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
# select() %>%
  filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
  group_by(Sample) %>% 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads)) %>%
  ungroup() %>%
  filter(!is.na(order)) %>%
  filter(rra > 0.01) %>%
  group_by(Sample, OTU) %>%
  summarise(n_reads = sum(n_reads), .groups = "drop") %>%
  pivot_wider(names_from = OTU, values_from = n_reads, values_fill = 0) %>%
  column_to_rownames("Sample") %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(-Sample, names_to = "metric", values_to = "shannon") %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = shannon)) +
  geom_col(fill = "#7FC29B") + 
  labs(x = NULL, y = "Shannon diversity") +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025"   = "verkuil_mod2",
      "pcrAK26027"   = "verkuil_mod1",
      "pcrAK26014"   = "leray_48",
      "pcrAK26033"   = "leray_48",
      "pcrAK26034"   = "leray_52",
      "pcrAK26035"   = "leray_56"
    )) +
  ggtitle("Shannon diversity reads \nrra > 0.01")



# proportion of reads and target reads

psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
  mutate(is_target = phylum %in% c("Arthropoda", "Mollusca"),
         rra = n_reads / sum(n_reads)) %>%
  group_by(Sample, is_target) %>%
  summarise(sum_reads = sum(n_reads), .groups = "drop") %>%
  group_by(Sample) %>%
  mutate(
    prop  = sum_reads / sum(sum_reads),
    label = ifelse(is_target, paste0(round(prop * 100, 1), "%"), NA),
    Sample = factor(Sample,
                    levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = sum_reads, fill = is_target)) +
  geom_col() +
  geom_text(aes(label = label),
            position = position_stack(vjust = 1.05),
            na.rm = TRUE) +
  labs(x = NULL, y = "Number of reads", fill = NULL) +
  scale_fill_manual(values = c("TRUE" = "#E69F00", "FALSE" = "grey70"),
                    labels = c("TRUE" = "Arthropoda or Mollusca", "FALSE" = "Other")) +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025" = "verkuil_mod2",
      "pcrAK26027" = "verkuil_mod1",
      "pcrAK26014" = "leray_48",
      "pcrAK26033" = "leray_48",
      "pcrAK26034" = "leray_52",
      "pcrAK26035" = "leray_56"
    )) +
  ggtitle("Total reads and target reads")


# this one doesn't really  make sense tbh
psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
  mutate(is_target = phylum %in% c("Arthropoda", "Mollusca"),
         rra = n_reads / sum(n_reads)) %>%
  filter(rra > 0.001) %>%
  group_by(Sample, is_target) %>%
  summarise(sum_reads = sum(n_reads), .groups = "drop") %>%
  group_by(Sample) %>%
  mutate(
    prop  = sum_reads / sum(sum_reads),
    label = ifelse(is_target, paste0(round(prop * 100, 1), "%"), NA),
    Sample = factor(Sample,
                    levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = sum_reads, fill = is_target)) +
  geom_col() +
  geom_text(aes(label = label),
            position = position_stack(vjust = 1.05),
            na.rm = TRUE) +
  labs(x = NULL, y = "Number of reads", fill = NULL) +
  scale_fill_manual(values = c("TRUE" = "#E69F00", "FALSE" = "grey70"),
                    labels = c("TRUE" = "Arthropoda or Mollusca", "FALSE" = "Other")) +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025" = "verkuil_mod2",
      "pcrAK26027" = "verkuil_mod1",
      "pcrAK26014" = "leray_48",
      "pcrAK26033" = "leray_48",
      "pcrAK26034" = "leray_52",
      "pcrAK26035" = "leray_56"
    )) +
  ggtitle("Total reads and target reads for rra > 0.001")



# this one doesn't really  make sense tbh
psmelt(sequence_data) %>% 
  as_tibble() %>% 
  rename(n_reads = Abundance) %>% 
  mutate(is_target = phylum %in% c("Arthropoda", "Mollusca"),
         rra = n_reads / sum(n_reads)) %>%
  filter(rra > 0.01) %>%
  group_by(Sample, is_target) %>%
  summarise(sum_reads = sum(n_reads), .groups = "drop") %>%
  group_by(Sample) %>%
  mutate(
    prop  = sum_reads / sum(sum_reads),
    label = ifelse(is_target, paste0(round(prop * 100, 1), "%"), NA),
    Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = sum_reads, fill = is_target)) +
  geom_col() +
  geom_text(aes(label = label),
            position = position_stack(vjust = 1.05),
            na.rm = TRUE) +
  labs(x = NULL, y = "Number of reads", fill = NULL) +
  scale_fill_manual(values = c("TRUE" = "#E69F00", "FALSE" = "grey70"),
                    labels = c("TRUE" = "Arthropoda or Mollusca", "FALSE" = "Other")) +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025" = "verkuil_mod2",
      "pcrAK26027" = "verkuil_mod1",
      "pcrAK26014" = "leray_48",
      "pcrAK26033" = "leray_48",
      "pcrAK26034" = "leray_52",
      "pcrAK26035" = "leray_56"
    )) +
  ggtitle("Total reads and target reads for rra > 0.01")


# taxonomic completeness


# KB22022

rra_data %>%
  filter(phylum %in% c("Arthropoda", "Mollusca"),
         n_reads != 0,
         f_sample_ID == "KB22022") %>%
  select(Sample,species, genus, family, order, class, n_reads) %>%
  group_by(Sample) %>%
  mutate(
    proportion = n_reads / sum(n_reads)) %>%
  ungroup() %>%
  group_by(species, genus, family, order) %>%
  mutate(n_Sample = n_distinct(Sample)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(Sample,species, genus, family) %>%
  mutate(Sample = factor(Sample,
                levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = factor(n_Sample))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "Proportion", fill = "species occurs in \nnumber of samples") +
  scale_fill_manual(values = cols8) +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025" = "verkuil_mod2",
      "pcrAK26027" = "verkuil_mod1",
      "pcrAK26014" = "leray_48",
      "pcrAK26033" = "leray_48",
      "pcrAK26034" = "leray_52",
      "pcrAK26035" = "leray_56"
    ))  + ylim(0,1)+
  ggtitle("Fecal sample taxonomic 'completeness'? of KB22022\nArthropoda and Mollusca") 

# KB22101
rra_data %>%
  filter(phylum %in% c("Arthropoda", "Mollusca"),
         n_reads != 0,
         f_sample_ID == "KB22101") %>%
  select(Sample,species, genus, family, order, class, n_reads) %>%
  group_by(Sample) %>%
  mutate(
    proportion = n_reads / sum(n_reads)) %>%
  ungroup() %>%
  group_by(species, genus, family, order) %>%
  mutate(n_Sample = n_distinct(Sample)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(Sample,species, genus, family) %>%
  mutate(Sample = factor(Sample,
                         levels = c("pcrAK26011", "pcrAK26027", "pcrAK26025", "pcrAK26014", "pcrAK26033", "pcrAK26034", "pcrAK26035"))) %>%
  ggplot(aes(x = Sample, y = proportion, fill = factor(n_Sample))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "Proportion", fill = "species occurs in \nnumber of samples") +
  scale_fill_manual(values = cols8) +
  scale_x_discrete(
    labels = c(
      "pcrAK26011" = "verkuil_og",
      "pcrAK26025" = "verkuil_mod2",
      "pcrAK26027" = "verkuil_mod1",
      "pcrAK26014" = "leray_48",
      "pcrAK26033" = "leray_48",
      "pcrAK26034" = "leray_52",
      "pcrAK26035" = "leray_56"
    ))  + ylim(0,1)+
  ggtitle("Fecal sample taxonomic 'completeness'? of KB22101 \nArthropoda and Mollusca") 




# get most common species 

View(
  psmelt(sequence_data) %>% 
    as_tibble() %>% 
    rename(n_reads = Abundance) %>% 
    # select() %>%
    filter(phylum %in% c("Arthropoda", "Mollusca")) %>%
    group_by(Sample) %>% 
    mutate(rra = n_reads / sum(n_reads),
           total_reads = sum(n_reads)) %>%
    ungroup() %>%
    filter(rra > 0.01) %>%
  select(Sample, primer_ID, species, rra) %>%
  distinct()
)

