## Analyse primer test data

# Load packages
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(phyloseq)
library(ggplot2)
#library(iNEXT)
#library(phyloseqCompanion)

# Set ggplot theme
theme_replace(text = element_text(size = 14),
              strip.background = element_blank(),
              strip.text = element_text(margin = margin(3.5, 3.5, 3.5, 3.5)),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", linewidth = 0.5, fill = NA),
              panel.grid = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              legend.justification = c("left", "center"),
              legend.text = element_text(),
              legend.key = element_blank(),
              axis.ticks = element_line(linewidth = 0.25),
              axis.ticks.length = unit(0.075, "cm"),
              axis.line = element_blank(),
              axis.text = element_text())

# Set working directory
setwd("D:/Projects/Both-Potkamp/2026-diet_warblers/primer_tests/02-bioinformatics")


# Import taxatable
d.taxa <- read.csv("08-phyloseq/taxatable_20260423.csv") |> 
  select(c(asv_id, kingdom, phylum, class, order, family, genus, species))
rownames(d.taxa) <- d.taxa$asv_id
d.taxa <- d.taxa |> 
  select(-asv_id)
d.taxa <- as.matrix(d.taxa)
d.taxa <- tax_table(d.taxa)

# Import metadata
d.meta <- readxl::read_xlsx("../01-sample_shipment/sample_ids.xlsx") |> 
  as.data.frame()

rownames(d.meta) <- d.meta$pcr_id
d.meta <- sample_data(d.meta)

# Import asv tables
asv.tabs <-  tibble(file = list.files("03-asvs/", pattern = "*asv_tab*", full.names = TRUE)) |> 
  mutate(locus = str_extract(file, "coi_jusino|coi_leray|coi_verkuil"))

d.asv <- lapply(1:nrow(asv.tabs), function(i) read.table(asv.tabs$file[i], , comment.char = "", sep = "\t", header = TRUE) |> 
                  rename(asv_id = X.OTU.ID) |> 
                  mutate(asv_id = str_c(asv.tabs$locus[i], asv_id, sep = "-")) |> 
                  pivot_longer(cols = -1, names_to = "pcr_id", values_to = "nreads")) |> 
  do.call(what = rbind) |> 
  pivot_wider(names_from = "pcr_id", values_from = "nreads", values_fill = 0) |> 
  as.data.frame()
rownames(d.asv) <- d.asv[,1]
d.asv <- d.asv[,2:ncol(d.asv)]
d.asv <- as.matrix(d.asv)
d.asv[is.na(d.asv)] <- 0
d.asv <- otu_table(d.asv, taxa_are_rows = TRUE)

## Construct phyloseq object
d.phylo.all <-  phyloseq(d.asv, d.taxa, d.meta)


### What are big ASVs without an order or family?
d.all <- psmelt(d.phylo.all) |> 
  as_tibble() |> 
  rename(n_reads = Abundance) |> 
  #select(OTU, seq_id, sample_id, sample_type, kingdom, phylum, class, order, family, genus, species, n_reads) |> 
  group_by(pcr_id) |> 
  mutate(rra = n_reads / sum(n_reads),
         total_reads = sum(n_reads))

d.all |> 
  filter(is.na(order) & rra > 0.2) |> 
  ungroup() |> 
  distinct(OTU) 
d.all |> 
  filter(is.na(family) & rra > 0.2) |> 
  ungroup() |> 
  distinct(OTU)



## Plot read counts
readsums <- tibble(nreads = sort(sample_sums(d.phylo.all), decreasing = TRUE), sorted = 1:nsamples(d.phylo.all), type = "samples") |> 
  bind_rows(tibble(nreads = sort(taxa_sums(d.phylo.all), decreasing = TRUE), sorted = 1:ntaxa(d.phylo.all), type = "asvs"))

title = "Overview number of reads for all samples"
p = ggplot(readsums, aes(x = sorted, y = nreads)) + geom_bar(stat = "identity")
p + ggtitle(title) + scale_y_log10() + facet_wrap(~type, 1, scales = "free")

## Diversity as a function of number of reads
richness.all <- estimate_richness(d.phylo.all, measures =c("Observed", "Shannon"))
richness.all <- richness.all |> 
  mutate(seq_id = rownames(richness.all))

sample.sums.allopod <- sample_sums(d.phylo.all)
sample.sums.allopod <- sample.sums.allopod |> 
  as_tibble() |> 
  rename(total_reads = value) |> 
  mutate(seq_id = names(sample.sums.allopod))

richness.all <- richness.all |> 
  left_join(y = sample.sums.allopod, by = "seq_id") |> 
  pivot_longer(1:2, names_to = "metric", values_to = "diversity")


ggplot(richness.all, aes(total_reads, diversity)) +
  facet_wrap(~metric, scales = "free") +
  #geom_smooth(method = "lm") +
  geom_point() +
  scale_x_log10() 
# No pattern

## Do samples have reads from the correct primers?
d.all.adapter <- d.all |> 
  mutate(read_adapter = str_split_fixed(OTU, "-", 2)[,1]) |> 
  group_by(read_adapter, sample_id, primerpair) |> 
  summarise(n_reads = sum(n_reads)) 

# Plot
ggplot(d.all.adapter, aes(sample_id, n_reads, fill = read_adapter)) +
  facet_wrap(~ primerpair) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Verkuil has Jusino reads: why?

### Other plots to make:
### Relative read abundance in mocks in categories Fungi, Aves, other metazoan and other
# Agglomerate  taxa at the level of class
d.phylo.all.class <- tax_glom(d.phylo.all, taxrank = "class", NArm = FALSE)

# Transform readcounts to rra
d.phylo.all.class <- transform_sample_counts(d.phylo.all.class, fun = function(x) x / sum(x))

# 'Melt' phyloseq object
d.all.class <- psmelt(d.phylo.all.class) |> 
  as_tibble() |> 
  # Determine categories for plat
  mutate(class_cat = case_when(class == "Aves" ~ "Aves",
                               kingdom == "Metazoa" ~ "other metazoan",
                               kingdom == "Fungi" ~ "Fungi",
                               .default = "other"))

# Plot
ggplot(d.all.class, aes(primerpair, Abundance, fill = class_cat)) + 
  facet_grid(~ sample_id , scale = "free_x", space = "free_x") +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual(values = c("royalblue4", "orangered2", "grey25","dodgerblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Aves blocking primer seems to work! 

# What are the Aves reads?
d.all |> 
  filter(class == "Aves") |> 
  ungroup() |> 
  distinct(kingdom, phylum, class, order, family, genus, species)
# All Sedge warbler, correct!

## What are the proportions target taxa (other metazoan)?
d.all.class |> 
  group_by(sample_id, primerpair, class_cat) |> 
  summarise(rra = sum(Abundance)) |> 
  filter(class_cat == "other metazoan")
## In mocks: almost all target taxa (makes sense)
## In fecal samples: ~ 60-70% target for the Verkuil and Jusino primers
## For the Leray primers: < 1% without bocking primers (as expected), 7-11% with Aves blocking primer

#### Other plots to make:
### Composition of target taxa, in mocks and fecal sample, e.g. at the order level
### Compare fecal sample compostion for the different primers:
### Are there taxa missing from certain primers?
### How does the community differ across primers?
### Do the blocking primers affect species composition?
### Does diversity and readcount differ across primers (sample_sums(), estimate_richness() )?

# Useful function: subset_taxa()
d.phylo.metazoan <- subset_taxa(d.phylo.all, kingdom == "Metazoa" & class != "Aves" & order != "Primates")
# Tip: catergorise orders to only show the relevant ones and to avoid large legends:
d.metazoan.order <- d.metazoan.order |> 
  mutate(order.cat = ifelse(order %in% c("Araneae", "Diptera", "Hymenoptera", "Lepidoptera", "Orthoptera"),
                            order, "other"))

# Only show orders which have been put in the mock samples:
d.metazoan.order <- d.metazoan.order |> 
  mutate(order.cat = ifelse(order %in% c("Araneae", "Diptera", "Hymenoptera", "Lepidoptera", "Orthoptera"),
                            order, "other"))

## Other usefull function: subset_samples()
d.phylo.metazoan.mock <- subset_samples(d.phylo.metazoan,  str_detect(sample_id, "mock"))
d.phylo.metazoan.fecal <- subset_samples(d.phylo.metazoan.mock, sample_id == "KB22022")

# Tip: agglomerate at the species level:
tax_glom(d.phylo.metazoan, taxrank = "species", NArm = FALSE)

# Tip: NMDS (might not work though)
nmds.object <- ordinate(phyloseqobject, "NMDS", "bray")
plot_ordination(phyloseqobject, nmds.object, type = "samples", color = "primerpair")
plot_ordination(phyloseqobject, nmds.object, type = "taxa", color = "primerpair")

# Tip: direct comparison of taxa
# Agglomorate at species level and transform to rra
# Pivot the table and plot 2 primers
d.metazoan.species.fecal.rel <- d.metazoan.species.fecal.rel |> 
  filter(Abundance > 0) |> 
  select(-OTU, -Sample, -pcr_id) |> 
  pivot_wider(names_from = primerpair, values_from = Abundance, values_fill = 0)
ggplot(d.metazoan.species.fecal.rel, aes(coi_leray, coi_leray_blockFungi, colour = order.cat)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  coord_fixed()

# Tip: what are taxa that occur more than 5 percent in some primers, and 0 percent in other primers?
d.metazoan.species.fecal.rel |> 
  ungroup() |> 
  filter(if_any(10:15, ~ . == 0) & if_any(10:15, ~ . > 0.05)) |> 
  View()

