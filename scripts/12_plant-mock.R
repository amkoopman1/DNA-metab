## mock samples plants, combinations of mock community

database_link_plant <- "https://docs.google.com/spreadsheets/d/1ccuRi7O9shqvnJ13UYI18z3Mf5dBF5ZIkG9vlTvMBjc/edit"


database_plant <- read_gsdb(database_link_plant)
names(database_plant)

# visualize which plants are combined in which sample

# include 
# from MetaSample: Plant_species
# link with Sample_ID
# from DimInvertebrates: Feeding_guild, Plant_part, Expected_diet_DNA
# link with MixInd_ID
# from FactMix: PlantMix_ID
total_df_plant <- database_plant[["MetaSample"]] %>%
  left_join(database_plant[["DimInvertebrates"]], by = "Sample_ID") %>%
  select(c("Plant_species","Feeding_guild","Plant_part","Expected_diet_DNA", "Species","MixInd_ID","Plant_specificity","Sample_ID")) %>%
  inner_join(database_plant[["FactMix"]], by = c("Species", "Sample_ID")) %>%
  select(c("Plant_species","Feeding_guild","Plant_part","Expected_diet_DNA", "Species","Plant_specificity","Sample_ID", "PlantMix_ID","n")) %>%
  distinct()


# overview

total_df_plant %>%
  mutate(
    Plant_part = factor(Plant_part, levels = sort(unique(Plant_part))),
    PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
    Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
    Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
    Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
  ) %>%
  ggplot(aes(x = Plant_species, y = Species, fill = Plant_part)) +
  geom_tile() +   
  scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ggtitle("Overview sample set, plants by invertebrates")

total_df_plant %>%
  mutate(
    Plant_part = factor(Plant_part, levels = rev(sort(unique(Plant_part)))),
    PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
    Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
    Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
    Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
  ) %>%
  filter(PlantMix_ID %in% c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix")) %>%
  ggplot(aes(x = PlantMix_ID, y = Plant_species, fill = Expected_diet_DNA)) +
  geom_tile() +   
  scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ggtitle("Overview plants and mixes for DNA and niche")


# Does a sample collected from one plant, only have plant DNA of one plant?

total_df_plant %>%
  mutate(
    Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
    PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
    Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
    Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
  ) %>%
  filter(PlantMix_ID %in% c("reed_mix","grass_mix")) %>%
  ggplot(aes(x = Feeding_guild, y = Species, fill = Plant_specificity)) +
  geom_tile() +   
  scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
  theme_minimal()+facet_wrap(~PlantMix_ID) + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank())+
  ggtitle("Does a sample collected from one plant, only have plant DNA of one plant?")


# How does feeding-strategy affect found DNA?
total_df_plant %>%
  mutate(
    Plant_part = factor(Plant_part, levels = rev(sort(unique(Plant_part)))),
    Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
    PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
    Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
    Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
  ) %>%
  filter(PlantMix_ID %in% c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix")) %>%
  ggplot(aes(x = Plant_part, y = Plant_species, fill = Expected_diet_DNA)) +
  geom_tile() +   
  scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
  theme_minimal()+facet_wrap(~PlantMix_ID) + 
  theme(axis.title.y=element_blank(),
         axis.title.x=element_blank(),
         legend.position=c(.85, .2)) +
  ggtitle("How does diet-niche affect found plant DNA?")


# # What DNA do non-herbivore guilds contain?
# total_df_plant %>%
#   mutate(
#     Plant_part = factor(Plant_part, levels = rev(sort(unique(Plant_part)))),
#     PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
#     Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
#     Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
#     Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
#     Species = factor(Species, levels = rev(sort(unique(Species)))),
#     Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
#   ) %>%
#   filter(PlantMix_ID %in% c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix")) %>%
#   ggplot(aes(x = Feeding_guild, y = Plant_species, fill = Expected_diet_DNA)) +
#   geom_tile() +   
#   scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
#   theme_minimal()+ facet_wrap(~PlantMix_ID) + 
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         legend.position=c(.85, .2)) +
#   ggtitle("What DNA do non-herbivore guilds contain?")


# Do polyphagous species still contain plant DNA from previous dinners?
total_df_plant %>%
  mutate(
    Plant_part = factor(Plant_part, levels = rev(sort(unique(Plant_part)))),
    PlantMix_ID = factor(PlantMix_ID, levels = c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix", "grass_mix", "reed_mix")),
    Plant_species = factor(Plant_species, levels = rev(sort(unique(Plant_species)))),
    Feeding_guild = factor(Feeding_guild, levels = rev(sort(unique(Feeding_guild)))),
    Plant_specificity = factor(Plant_specificity, levels = c("monophagous", "oligophagous", "polyphagous","???", "predator")),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Expected_diet_DNA = factor(Expected_diet_DNA, levels = c("low", "mid", "high"))
  ) %>%
  ggplot(aes(x = PlantMix_ID, y = Species, fill = Plant_specificity)) +
  geom_tile() +   
  scale_fill_manual(values = c("mistyrose", "darkolivegreen3", "darkolivegreen4", "khaki2","#FFFED0")) + 
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ggtitle("Do polyphagous species still contain plant DNA from previous dinners?")


# write to sheet

# total_df_plant %>%
#   select(PlantMix_ID, Sample_ID, Species, n,MixInd_ID) %>%
#   arrange(PlantMix_ID,Sample_ID) %>%
#   sheet_write(ss = database_link_plant,
#               sheet = paste0("FactMixR"))



database_plant[["printMix"]] %>%
  filter("yes" == mash)%>%
  filter(PlantMix_ID %in% c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix")) %>%
  select(Species) %>%
  arrange(Species) %>%
  distinct()

View(database_plant[["printMix"]] %>%
       filter("no" == mash)%>%
       filter(PlantMix_ID %in% c("low_mix", "mid_mix", "high_mix","guild_mix", "phag_mix")) %>%
       select(Species, Sample_ID) %>%
       arrange(Species) %>%
       distinct())

