## investigate what invertebrates already came out of the previous analysis


# link to sedgewarbler_bil20260213 
link_inv1 <- "https://docs.google.com/spreadsheets/d/13ImnfdXIXchNJ7vbupZ8Oe8CyUrj5Cpr45j2NpVSlXk/edit"

database_inv1 <- read_gsdb(link_inv1)

names(database_inv1)

#
data_inv1 <- database_inv1[["Sheet1"]] %>%
  filter(rra >= 0.0, # only take reads with >= 'fraction' of resp. sample rra
         !is.na(host_age_group),
         !is.na(phylum)) %>% # filter that sample has age
  # give timebin of 21 days and 42 days
  mutate(day_of_year = yday(collection_date),  # Day of year (1-365)
         days21 = ceiling((day_of_year +0) / 21), # set for 21 days
         days42 = ceiling((day_of_year +0) / 42), # the + changes the window of time
         Year = substr(collection_date, 1,4)) # give year

names(data_inv1)



# unique names in each taxonomic level 
unique(data_inv1$kingdom) # Metazoa
unique(data_inv1$phylum) # Arhtropoda
unique(data_inv1$class) # for rra >= 0.01, n=6
unique(data_inv1$order) # rra >= 0.01, n=23
unique(data_inv1$family) # rra >= 0.01, n=121
unique(data_inv1$genus) # rra >= 0.01, n=251
unique(data_inv1$species) # rra >= 0.01, n=261


sum_data_inv1 <- data_inv1 %>%
  group_by(order) %>%
  summarise(avg_rra = mean(rra))


# get species and order
data_order2species <- data_inv1 %>% group_by(order) %>% distinct(species)

# highest rra species per order
data_order_max_rra <- data_inv1 %>% group_by(order, species) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

# highest rra family per order
data_family_max_rra <- data_inv1 %>% group_by(order, family) %>% summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% slice_max(avg_rra)

#View(data_order2species)
#View(data_order_max_rra)
#View(data_family_max_rra)



# compare nestling and adult 

# adult highest avg rra species per order 
n1_species_rra <-  data_inv1 %>% filter(
  host_age_group == 'adult') %>%
  group_by(species, order) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

#View(n1_species_rra)

# plot species change over season adult
data_inv1 %>%
  filter(
    host_age_group == 'adult') %>%
  group_by(species, days42) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(days42, avg_rra, color = species)) + 
  geom_point() + theme(legend.position = "none")+
  geom_line()
  
# nestling highest avg rra species per order 
kj_species_rra <- data_inv1 %>% 
  filter(host_age_group == 'nestling') %>%
  group_by(order, species) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>% 
  slice_max(avg_rra)

# plot species change over season nestling
data_inv1 %>%
  filter(
    host_age_group == 'nestling') %>%
  group_by(species, days42) %>% 
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(days42, avg_rra, color = species)) + 
  geom_point() + theme(legend.position = "none")+
  geom_line()

#View(kj_pecies_rra)


## look at seasons
names(data_inv1)

# look at number of samples per season per year
count_inv1 <- data_inv1 %>%
  group_by(host_age_group, days42, Year) %>%
  summarise(count = n()) 

#View(count_inv1)

ggplot(count_inv1, aes(days42, count, color = Year, shape = host_age_group)) + 
  geom_point() + ggtitle("Rietzanger samples per 42 days bin")

# compare for days42 time periods


# graph important diet sources compare for age and season
c1 <- c("#55D6BE", "#E57B40", "#7D5BA6", "#C95D63", "#AE8799", "#FC6471", "#496DDB")

data_inv1 %>%
  filter( # for readability
    order %in% c('Trombidiformes', 'Ephemeroptera', 'Trichoptera', 'Hemiptera', 'Lepidoptera', 'Coleoptera', 'Diptera'))%>%
  group_by(order, days42, host_age_group) %>%
  summarise(avg_rra = mean(rra, na.rm = TRUE)) %>%
  ggplot(aes(x = days42, y = avg_rra, color = order, linetype = host_age_group)) +
  geom_line() +
  geom_point() +
  labs(x = "days42", y = "Average RRA", color = "Order", linetype = "Age Class") +
  theme_bw() + scale_color_manual(values = c1)+ theme(#)
)


# ## calculate shannon biodiv per season and age, at species level 
# 
# data_div_sp <- data_inv1 %>%
#   filter(!is.na(species)) %>%
#   select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#   group_by(species, host_age_group, days42) %>%
#   filter(n_distinct(sample_id) >= 1) %>%
#   summarise(rra = mean(rra, na.rm = TRUE)) %>%
#   pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#   ungroup()
# 
# s_data_div_sp <- data_div_sp %>%
#   select(-host_age_group, -days42) 
# 
# 
# shannon_sp <- diversity(s_data_div_sp, index = "shannon")
# 
# result_sp <- data_div_sp %>%
#   select(host_age_group, days42) %>%
#   mutate(shannon_sp = shannon_sp)
# 
# 
# 
# # the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most possibly due to sample size bias
# result_sp %>%
#   ggplot(aes(x = days42, y = shannon_sp, linetype = host_age_group)) +
#   geom_line() +
#   geom_point() +
#   labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
#   theme_bw()
# 
# 
# 
# ## calculate shannon biodiv for family level
# 
# data_div_fa <- data_inv1 %>%
#   filter(!is.na(family)) %>%
#   select(c(rra, family, host_age_group, days42, Year, sample_id)) %>%
#   group_by(family, host_age_group, days42) %>%
#   filter(n_distinct(sample_id) >= 1) %>%
#   summarise(rra = mean(rra, na.rm = TRUE)) %>%
#   pivot_wider(names_from = family, values_from = rra, values_fill = 0) %>%
#   ungroup()
# 
# m_data_div_fa <- data_div_fa %>%
#   select(-host_age_group, -days42) 
# 
# 
# shannon_fa <- diversity(m_data_div_fa, index = "shannon")
# 
# result_fa <- data_div_fa %>%
#   select(host_age_group, days42) %>%
#   mutate(shannon_fa = shannon_fa)
# 
# 
# 
# # the biodiveristy index throughout the season, years collapsed and not really filtered for things; very uneven, drop most likely due to sample size bias
# result_fa %>%
#   ggplot(aes(x = days42, y = shannon_fa, linetype = host_age_group)) +
#   geom_line() +
#   geom_point() +
#   labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
#   theme_bw()
# 
# 
# 
# 
# ## now make a 'even' subset 
# # use days42 3-5 for all years for all ages
# 
# # filter and use 12 samples per group
# f_data_inv1 <- data_inv1 %>%
#   filter(days42 %in% c('3', '4', '5'),
#          !is.na(order)) %>%
#   select(
#     sample_id,host_age_group,species,family,order,rra,days42,days21,Year, day_of_year) %>%
#   group_by(days42, host_age_group, Year) %>% # take for each days21 to have more even distributed of samples
#   slice_sample(n=12) %>% 
#   ungroup()
# 
# # mean sample day of each season*age 
# f_samples_sum <- f_data_inv1 %>%
#   group_by(host_age_group, days42) %>%
#   summarise(avg_day = mean(day_of_year))
# #View(f_samples_sum)
# 
# # make sure that 12 to each group
# f_count_inv1 <- f_data_inv1 %>%
#   group_by(host_age_group, days42, Year) %>%
#   summarise(count = n()) 
# 
# 
# 
# ## calculate shannon biodiv for species level for filtered set
# 
# f_data_div_sp <- f_data_inv1 %>%
#   filter(!is.na(species)) %>%
#   select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#   group_by(species, days42, Year, host_age_group) %>%
#   filter(n_distinct(sample_id) >= 1) %>%
#   summarise(rra = mean(rra, na.rm = TRUE)) %>%
#   pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#   ungroup()
# 
# f_m_data_div_sp <- f_data_div_sp %>%
#   select(-days42, -Year, -host_age_group) 
# 
# 
# f_shannon_sp <- diversity(f_m_data_div_sp, index = "shannon")
# 
# f_result_sp <- f_data_div_sp %>%
#   select(days42, Year, host_age_group) %>%
#   mutate(f_shannon_sp = f_shannon_sp)
# 
# 
# 
# # the biodiveristy index throughout the season
# f_result_sp %>%
#   ggplot(aes(x = days42, y = f_shannon_sp, color = Year, linetype = host_age_group)) +
#   geom_line() +
#   geom_point() +
#   labs(x = "days42", y = "shannon biodiv", linetype = "Age Class") +
#   theme_bw() + ggtitle("Biodiversity for even subset of data for 12 samples per datapoint")
# 
# 
# ## kruskal testing for differences between groups
# 
# kruskal.test(f_shannon_sp ~ days42, data = f_result_sp) 
# 
# kruskal.test(f_shannon_sp ~ Year, data = f_result_sp)
# 
# 
# 
# ## for loop to see how stable results are for choosing n samples per group
# # based on this, choose sample size per group
# 
# results_list <- vector("list", 10)  # pre-allocate list of length 100
# 
# for (i in 1:20) {
#   
#   f_data_inv1 <- data_inv1 %>%
#     filter(days42 %in% c('3', '4', '5'), !is.na(order)) %>%
#     select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#     group_by(days42, host_age_group, Year) %>%
#     # Sample 10 unique sample_ids per group, then keep all their rows
#     filter(sample_id %in% {
#       unique_ids <- unique(sample_id)
#       sample(unique_ids, size = min(10, # number of samples
#                                     length(unique_ids)))
#     }) %>%
#     ungroup()
#   
#   f_data_div_sp <- f_data_inv1 %>%
#     filter(!is.na(species)) %>%
#     select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#     group_by(species, host_age_group, days42, Year) %>%
#     filter(n_distinct(sample_id) >= 1) %>%
#     summarise(rra = mean(rra, na.rm = TRUE)) %>%
#     pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#     ungroup()
#   
#   f_m_data_div_sp <- f_data_div_sp %>%
#     select(-host_age_group, -days42, -Year)
#   
#   f_shannon_sp <- diversity(f_m_data_div_sp, index = "shannon")
#   
#   f_result_sp <- f_data_div_sp %>%
#     select(host_age_group, days42, Year) %>%
#     mutate(shannon = f_shannon_sp,
#            run = i)  # <-- tag each row with which run it came from
#   
#   results_list[[i]] <- f_result_sp
# }
# 
# all_results <- bind_rows(results_list)
# 
# # compare whether runs are different
# kruskal.test(shannon ~ run, data = all_results) 
# 
# # mu, sd and variance of groups after 20 runs on random pulls, with now n=10
# # ideally, cv < 0.1. otherwise, < 0.2 still acceptable.
# all_results %>%
#   group_by(host_age_group, days42, Year) %>%
#   summarise(
#     mean_shannon = mean(shannon), # mean
#     sd_shannon   = sd(shannon), # standard deviation
#     cv           = sd(shannon) / mean(shannon) # coefficient of variation
#   )
# 
# 
# 
# 
# ## for loop to see how stable results are when choosing n random samples
# # based on this, choose sample size per group
# 
# results_list <- vector("list", 10)  # pre-allocate list of length 100
# 
# for (i in 1:20) {
#   
#   r_data_inv1 <- data_inv1 %>%
#     select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#     # Sample 10 unique sample_ids per group, then keep all their rows
#     filter(sample_id %in% {
#       unique_ids <- unique(sample_id)
#       sample(unique_ids, size = min(20, # number of samples
#                                     length(unique_ids)))
#     }) %>%
#     ungroup()
#   
#   r_data_div_sp <- r_data_inv1 %>%
#     filter(!is.na(species)) %>%
#     select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#     group_by(species) %>%
#     filter(n_distinct(sample_id) >= 1) %>%
#     summarise(rra = mean(rra, na.rm = TRUE)) %>%
#     pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#     ungroup()
#   
#   r_shannon_sp <- diversity(r_data_div_sp, index = "shannon")
#   
#   r_result_sp <- r_data_div_sp %>%
#     mutate(shannon = r_shannon_sp,
#            run = i)  # <-- tag each row with which run it came from
#   
#   results_list[[i]] <- r_result_sp
# }
# 
# r_all_results <- bind_rows(results_list)
# 
# # compare whether runs are different
# kruskal.test(shannon ~ run, data = r_all_results) 
# 
# # mu, sd and variance of groups after 20 runs on random pulls, with now n=10
# # ideally, cv < 0.1. otherwise, < 0.2 still acceptable.
# r_all_results %>%
#   summarise(
#     mean_shannon = mean(shannon), # mean
#     sd_shannon   = sd(shannon), # standard deviation
#     cv           = sd(shannon) / mean(shannon) # coefficient of variation
#   )
# 
# 
# 
# 
# 
# 
# 
# ## test different sample sizes for a random pull
# # this is under the assumption that there is no difference between groups
# 
# # Define the range of sample sizes to test
# sample_sizes <- c(seq(5,200))
# 
# n_iterations <- 20
# 
# data_inv1 %>% 
#   filter(!is.na(species))%>%
#   summarise(count = n_distinct(sample_id))
# 
# # Create a dataframe to store all results
# all_results <- data.frame()
# 
# # Loop through each sample size
# for (sample_size in sample_sizes) {
#   
#   results_list <- vector("list", n_iterations)
#   
#   # Run iterations for current sample size
#   for (i in 1:n_iterations) {
#     
#     r_data_inv1 <- data_inv1 %>%
#       select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#       # Sample n unique sample_ids per group
#       filter(sample_id %in% {
#         unique_ids <- unique(sample_id)
#         sample(unique_ids, size = min(sample_size, length(unique_ids)))
#       }) %>%
#       ungroup()
#     
#     r_data_div_sp <- r_data_inv1 %>%
#       filter(!is.na(species)) %>%
#       select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#       group_by(species) %>%
#       filter(n_distinct(sample_id) >= 1) %>%
#       summarise(rra = mean(rra, na.rm = TRUE)) %>%
#       pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#       ungroup()
#     
#     r_shannon_sp <- diversity(r_data_div_sp, index = "shannon")
#     
#     r_result_sp <- r_data_div_sp %>%
#       mutate(shannon = r_shannon_sp,
#              run = i,
#              sample_size = sample_size)
#     
#     results_list[[i]] <- r_result_sp
#   }
#   
#   # Bind results for this sample size
#   r_sample_results <- bind_rows(results_list)
#   
#   # Append to overall results
#   all_results <- bind_rows(all_results, r_sample_results)
# }
# 
# # Summary statistics by sample size
# summary_stats <- all_results %>%
#   group_by(sample_size) %>%
#   summarise(
#     mean_shannon = mean(shannon),
#     sd_shannon = sd(shannon),
#     cv = sd(shannon) / mean(shannon),
#     n_runs = n()
#   ) %>%
#   arrange(sample_size)
# 
# print(summary_stats)
# 
# # Kruskal-Wallis test for each sample size
# kruskal_results <- all_results %>%
#   group_by(sample_size) %>%
#   summarise(
#     kruskal_p = kruskal.test(shannon ~ run)$p.value
#   )
# 
# print(kruskal_results)
# 
# # visualize stability across sample sizes
# # CV plot with vertical line at 184
# ggplot(summary_stats, aes(x = sample_size, y = cv)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange") +
#   geom_vline(xintercept = 184, linetype = "dashed", color = "red") +
#   labs(title = "Coefficient of Variation by Sample Size",
#        x = "Sample Size",
#        y = "CV",
#        caption = "Green line: CV = 0.1 (ideal)\nOrange line: CV = 0.2 (acceptable)\nRed line: sample size = 184") +
#   theme_minimal()
# 
# # Shannon plot with vertical line at 184
# ggplot(summary_stats, aes(x = sample_size, y = mean_shannon)) +
#   geom_line() +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = mean_shannon - sd_shannon, 
#                     ymax = mean_shannon + sd_shannon), 
#                 width = 1) +
#   geom_vline(xintercept = 184, linetype = "dashed", color = "red") +
#   labs(title = "Shannon diversity by sample size",
#        x = "Sample size",
#        y = "Shannon diversity (mean ± SD)",
#        caption = "Red line: sample size = 184") +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# ## look at shannon index and groups
# 
# # Define the range of sample sizes to test
# sample_sizes <- c(2,4,6,8,10,12,14, 16,18, 20, 25,30,40,50,60)
# n_iterations <- 20
# 
# 
# 
# 
# # Define grouping types
# grouping_types <- list(
#   "days42" = c("days42"),
#   "days21" = c("days21"),
#   "age only" = c("host_age_group"),
#   "year only" = c("Year"))
# 
# 
# # Create a dataframe to store all results
# all_results <- data.frame()
# 
# # Loop through each grouping type
# for (grouping_name in names(grouping_types)) {
#   
#   grouping_vars <- grouping_types[[grouping_name]]
#   
#   # Loop through each sample size
#   for (sample_size in sample_sizes) {
#     
#     results_list <- vector("list", n_iterations)
#     
#     # Run iterations for current sample size and grouping
#     for (i in 1:n_iterations) {
#       
#       g_data_inv1 <- data_inv1 %>%
#         filter(days42 %in% c('3', '4', '5'), days21 != '5', !is.na(order)) %>%
#         select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#         group_by(across(all_of(grouping_vars))) %>%
#         # Sample n unique sample_ids per group
#         filter(sample_id %in% {
#           unique_ids <- unique(sample_id)
#           sample(unique_ids, size = min(sample_size, length(unique_ids)))
#         }) %>%
#         ungroup()
#       
#       # Calculate diversity - pivot species to columns
#       g_data_div_sp <- g_data_inv1 %>%
#         filter(!is.na(species)) %>%
#         select(c(rra, species, all_of(grouping_vars), sample_id)) %>%
#         group_by(across(all_of(c("species", grouping_vars)))) %>%
#         filter(n_distinct(sample_id) >= 1) %>%
#         summarise(rra = mean(rra, na.rm = TRUE), .groups = "drop") %>%
#         pivot_wider(names_from = species, 
#                     values_from = rra, 
#                     values_fill = 0) %>%
#         ungroup()
#       
#       # Calculate Shannon diversity (excluding grouping columns)
#       diversity_cols <- g_data_div_sp %>% 
#         select(-all_of(grouping_vars))
#       
#       g_shannon_sp <- diversity(diversity_cols, index = "shannon")
#       
#       # Store results - one shannon value per demographic group
#       g_result_sp <- g_data_div_sp %>%
#         select(all_of(grouping_vars)) %>%
#         mutate(
#           shannon = g_shannon_sp,
#           run = i,
#           sample_size = sample_size,
#           grouping = grouping_name
#         )
#       
#       results_list[[i]] <- g_result_sp
#     }
#     
#     # Bind results for this sample size
#     g_sample_results <- bind_rows(results_list)
#     
#     # Append to overall results
#     all_results <- bind_rows(all_results, g_sample_results)
#   }
# }
# 
# # Summary statistics by sample size and grouping
# summary_stats <- all_results %>%
#   group_by(sample_size, grouping) %>%
#   summarise(
#     mean_shannon = mean(shannon, na.rm = TRUE),
#     sd_shannon = sd(shannon, na.rm = TRUE),
#     cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
#     n_runs = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(grouping, sample_size)
# 
# print(summary_stats)
# 
# # Kruskal-Wallis test for each sample size and grouping
# kruskal_results <- all_results %>%
#   group_by(sample_size, grouping) %>%
#   summarise(
#     kruskal_p = tryCatch(
#       kruskal.test(shannon ~ run)$p.value,
#       error = function(e) NA
#     ),
#     .groups = "drop"
#   )
# 
# print(kruskal_results)
# 
# 
# # Faceted plots for each grouping
# p3 <- ggplot(summary_stats, aes(x = sample_size, y = mean_shannon)) +
#   geom_line(linewidth = 1, color = "blue") +
#   geom_point(size = 3, color = "blue") +
#   geom_errorbar(aes(ymin = mean_shannon - sd_shannon, 
#                     ymax = mean_shannon + sd_shannon), 
#                 width = 1, alpha = 0.5, color = "blue") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Shannon diversity by sample size",
#        subtitle = "For 4 groupings",
#        x = "Sample size",
#        y = "Shannon diversity (mean ± SD)") +
#   theme_minimal()
# 
# #shannon biodiversity
# print(p3)
# 
# p4 <- ggplot(summary_stats, aes(x = sample_size, y = cv)) +
#   geom_line(linewidth = 1, color = "red") +
#   geom_point(size = 3, color = "red") +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Coefficient of Variation by Sample Size",
#        subtitle = "Faceted by demographic grouping",
#        x = "Sample Size",
#        y = "CV") +
#   theme_minimal()
# 
# #coefficient of variance
# print(p4)
# 
# data_inv1 %>% 
#   filter(days42 %in% c('3', '4', '5'), days21 != '5', !is.na(species))%>%
#   group_by(Year) %>% 
#   summarise(count = n_distinct(sample_id))
# data_inv1 %>% 
#   filter(days42 %in% c('3', '4', '5'), days21 != '5', !is.na(species))%>%
#     group_by(host_age_group) %>% 
#   summarise(count = n_distinct(sample_id))
# data_inv1 %>% 
#   filter(days42 %in% c('3', '4', '5'), days21 != '5', !is.na(species))%>%
#     group_by(days21) %>% 
#   summarise(count = n_distinct(sample_id))
# data_inv1 %>% 
#   filter(days42 %in% c('3', '4', '5'), days21 != '5', !is.na(species))%>%
#     group_by(days42) %>% 
#   summarise(count = n_distinct(sample_id))
# 
# 
# 
# ## plots with lowest sample count per grouping
# 
# # Define maximum samples for each grouping (based on your counts)
# max_samples_per_group <- data.frame(
#   grouping = c("days42", "days21", "age only", "year only"),
#   max_samples = c(19, 19, 55, 25)  # Replace with your actual minimum counts
# )
# 
# # Merge with summary stats
# summary_stats <- summary_stats %>%
#   left_join(max_samples_per_group, by = "grouping")
# 
# # Shannon plot with vertical lines
# p3 <- ggplot(summary_stats, aes(x = sample_size, y = mean_shannon)) +
#   geom_line(linewidth = 1, color = "blue") +
#   geom_point(size = 3, color = "blue") +
#   geom_errorbar(aes(ymin = mean_shannon - sd_shannon, 
#                     ymax = mean_shannon + sd_shannon), 
#                 width = 1, alpha = 0.5, color = "blue") +
#   geom_vline(aes(xintercept = max_samples), 
#              linetype = "dashed", color = "red", linewidth = 0.8) +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Shannon diversity by sample size",
#        subtitle = "For 4 groupings (red line = min available samples)",
#        x = "Sample size",
#        y = "Shannon diversity (mean ± SD)",
#        caption = "Red dashed line: minimum available samples per group") +
#   theme_minimal()
# 
# print(p3)
# 
# # CV plot with vertical lines
# p4 <- ggplot(summary_stats, aes(x = sample_size, y = cv)) +
#   geom_line(linewidth = 1, color = "red") +
#   geom_point(size = 3, color = "red") +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange") +
#   geom_vline(aes(xintercept = max_samples), 
#              linetype = "dashed", color = "black", linewidth = 0.8) +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Coefficient of variation by sample size",
#        subtitle = "Faceted by grouping (black line = min available samples)",
#        x = "Sample size",
#        y = "cv",
#        caption = "Red dashed line: minimum available samples per group") +
#   theme_minimal()
# 
# print(p4)
# 
# 
# 
# 
# 
# 
# # for different rra cutoffs, grouped only by year
# 
# # Define the range of sample sizes to test
# sample_sizes <- c(seq(5,50))
# n_iterations <- 10
# 
# # Define RRA thresholds to test
# rra_thresholds <- c(0, 0.025, 0.05,0.10,0.25)
# 
# # Create a dataframe to store all results
# rra_all_results <- data.frame()
# 
# # Loop through each RRA threshold
# for (rra_threshold in rra_thresholds) {
#   
#   # Loop through each sample size
#   for (sample_size in sample_sizes) {
#     
#     rra_results_list <- vector("list", n_iterations)
#     
#     # Run iterations for current sample size and threshold
#     for (i in 1:n_iterations) {
#       
#       rra_data_inv1 <- data_inv1 %>%
#         select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#         # ADD GROUPING HERE before sampling
#         group_by(Year) %>%
#         # Sample n unique sample_ids per group
#         filter(sample_id %in% {
#           unique_ids <- unique(sample_id)
#           sample(unique_ids, size = min(sample_size, length(unique_ids)))
#         }) %>%
#         filter(rra >= rra_threshold) %>%  # Apply RRA threshold filter
#         ungroup()
#       
#       rra_data_div_sp <- rra_data_inv1 %>%
#         filter(!is.na(species)) %>%
#         select(c(rra, species, host_age_group, days42, Year, sample_id)) %>%
#         group_by(species) %>%
#         filter(n_distinct(sample_id) >= 1) %>%
#         summarise(rra = mean(rra, na.rm = TRUE), .groups = "drop") %>%
#         pivot_wider(names_from = species, values_from = rra, values_fill = 0) %>%
#         ungroup()
#       
#       rra_shannon_sp <- diversity(rra_data_div_sp, index = "shannon")
#       
#       rra_result_sp <- rra_data_div_sp %>%
#         mutate(shannon = rra_shannon_sp,
#                run = i,
#                sample_size = sample_size,
#                rra_threshold = rra_threshold)
#       
#       rra_results_list[[i]] <- rra_result_sp
#     }
#     
#     # Bind results for this sample size
#     rra_sample_results <- bind_rows(rra_results_list)
#     
#     # Append to overall results
#     rra_all_results <- bind_rows(rra_all_results, rra_sample_results)
#   }
# }
# 
# # Summary statistics by sample size and RRA threshold
# rra_summary_stats <- rra_all_results %>%
#   group_by(sample_size, rra_threshold) %>%
#   summarise(
#     mean_shannon = mean(shannon),
#     sd_shannon = sd(shannon),
#     cv = sd(shannon) / mean(shannon),
#     n_runs = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(rra_threshold, sample_size)
# 
# print(rra_summary_stats)
# 
# # Kruskal-Wallis test for each sample size and threshold
# rra_kruskal_results <- rra_all_results %>%
#   group_by(sample_size, rra_threshold) %>%
#   summarise(
#     kruskal_p = kruskal.test(shannon ~ run)$p.value,
#     .groups = "drop"
#   )
# 
# print(rra_kruskal_results)
# 
# # Create a label for thresholds for better legend
# rra_summary_stats <- rra_summary_stats %>%
#   mutate(threshold_label = paste0("RRA > ", rra_threshold))
# 
# # Plot 1: CV by sample size, colored by RRA threshold - ALL IN ONE GRAPH
# p1 <- ggplot(rra_summary_stats, aes(x = sample_size, y = cv, color = threshold_label)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "darkgreen", alpha = 0.7, linewidth = 0.8) +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkorange", alpha = 0.7, linewidth = 0.8) +
#   scale_color_viridis_d(option = "plasma", end = 0.9) +
#   labs(title = "Coefficient of Variation by Sample Size",
#        subtitle = "Effect of different RRA thresholds",
#        x = "Sample Size (per demographic group)",
#        y = "CV",
#        color = "RRA Threshold",
#        caption = "Green line: CV = 0.1 (ideal) | Orange line: CV = 0.2 (acceptable)") +
#   theme_minimal() +
#   theme(legend.position = "right",
#         plot.caption = element_text(hjust = 0))
# 
# # Plot 2: Shannon diversity by sample size, colored by RRA threshold - ALL IN ONE GRAPH
# p2 <- ggplot(rra_summary_stats, aes(x = sample_size, y = mean_shannon, color = threshold_label)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   geom_ribbon(aes(ymin = mean_shannon - sd_shannon, 
#                   ymax = mean_shannon + sd_shannon,
#                   fill = threshold_label), 
#               alpha = 0.15, color = NA) +
#   scale_color_viridis_d(option = "plasma", end = 0.9) +
#   scale_fill_viridis_d(option = "plasma", end = 0.9) +
#   labs(title = "Shannon diversity by sample size",
#        subtitle = "Mean ± SD for different RRA thresholds",
#        x = "Sample s",
#        y = "Shannon Diversity",
#        color = "RRA Threshold",
#        fill = "RRA Threshold") +
#   theme_minimal() +
#   theme(legend.position = "right")
# 
# # Print individual plots
# print(p1)
# print(p2)



## compare between and within group variance 



### between groups 

# select 15 random samples of each group

# data_inv1 <- data_inv1 %>%
#   filter(
#     days42 %in% c('4', '5'),
#     Year %in% c('2021', '2022'),
#     !is.na(order)) %>%
#   group_by(host_age_group, days42, Year) %>%
#   # Sample n unique sample_ids per group
#   filter(sample_id %in% {
#     unique_ids <- unique(sample_id)
#     sample(unique_ids, size = min(15, length(unique_ids)))  # ← Added min()
#   })
# 
# data_inv1 %>% 
#   summarise(n_distinct = n_distinct(sample_id))
#   
# 
# 
# ## look at shannon index and groups
# 
# # Define the range of sample sizes to test
# sample_sizes <- c(1,2,3,4,5,6)
# n_iterations <- 50
# 
# 
# # ## List your dataframes
# # dataframes <- list(samples_species_season_year, samples_season_year_age,samples_species_age, samples_species_year)
# # Define grouping types
# grouping_types <- list(
#   "days42" = c("days42"),
#   "age_days42" = c("days42", "host_age_group"),
#   "age" = c("host_age_group"),
#   "age_year" = c("Year", "host_age_group"))
# 
# # Create a dataframe to store all results
# all_results <- data.frame()
# 
# # Loop through each grouping type
# for (grouping_name in names(grouping_types)) {
#   
#   grouping_vars <- grouping_types[[grouping_name]]
#   
#   # Loop through each sample size
#   for (sample_size in sample_sizes) {
#     
#     results_list <- vector("list", n_iterations)
#     
#     # Run iterations for current sample size and grouping
#     for (i in 1:n_iterations) {
#       
#       g_data_inv1 <- data_inv1 %>%   filter(
#         days21 %in% c('8', '9', '7', '10'), Year %in% c('2021', '2022'), !is.na(order)) %>%
#         select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#         group_by(across(all_of(grouping_vars))) %>%
#         # Sample n unique sample_ids per group
#         filter(sample_id %in% {
#           unique_ids <- unique(sample_id)
#           sample(unique_ids, size = min(sample_size, length(unique_ids)))
#         }) %>%
#         ungroup()
#       
#       # Calculate diversity - pivot species to columns
#       g_data_div_sp <- g_data_inv1 %>%
#         filter(!is.na(species)) %>%
#         select(c(rra, species, all_of(grouping_vars), sample_id)) %>%
#         group_by(across(all_of(c("species", grouping_vars)))) %>%
#         filter(n_distinct(sample_id) >= 1) %>%
#         summarise(rra = mean(rra, na.rm = TRUE), .groups = "drop") %>%
#         pivot_wider(names_from = species, 
#                     values_from = rra, 
#                     values_fill = 0) %>%
#         ungroup()
#       
#       # Calculate Shannon diversity (excluding grouping columns)
#       diversity_cols <- g_data_div_sp %>% 
#         select(-all_of(grouping_vars))
#       
#       g_shannon_sp <- diversity(diversity_cols, index = "shannon")
#       
#       # Store results - one shannon value per demographic group
#       g_result_sp <- g_data_div_sp %>%
#         select(all_of(grouping_vars)) %>%
#         mutate(
#           shannon = g_shannon_sp,
#           run = i,
#           sample_size = sample_size,
#           grouping = grouping_name
#         )
#       
#       results_list[[i]] <- g_result_sp
#     }
#     
#     # Bind results for this sample size
#     g_sample_results <- bind_rows(results_list)
#     
#     # Append to overall results
#     all_results <- bind_rows(all_results, g_sample_results)
#   }
# }
# 
# # Summary statistics by sample size, grouping, AND group level
# # This calculates mean/sd/cv ACROSS runs for each specific group (e.g., each year, each age, etc.)
# summary_stats_by_group <- all_results %>%
#   group_by(sample_size, grouping, across(any_of(c("Year", "host_age_group", "days21", "days42")))) %>%
#   summarise(
#     mean_shannon = mean(shannon, na.rm = TRUE),
#     sd_shannon = sd(shannon, na.rm = TRUE),
#     cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
#     n_runs = n(),
#     .groups = "drop"
#   )
# 
# print(summary_stats_by_group)
# 
# # Summary statistics showing VARIATION BETWEEN GROUPS (not runs)
# # This is the key difference - we look at SD across different groups
# summary_stats_between_groups <- all_results %>%
#   group_by(sample_size, grouping, run) %>%
#   summarise(
#     mean_shannon = mean(shannon, na.rm = TRUE),
#     sd_shannon = sd(shannon, na.rm = TRUE),
#     cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
#     n_groups = n(),
#     .groups = "drop"
#   ) %>%
#   group_by(sample_size, grouping) %>%
#   summarise(
#     mean_of_means = mean(mean_shannon, na.rm = TRUE),
#     mean_sd_between_groups = mean(sd_shannon, na.rm = TRUE),
#     mean_cv_between_groups = mean(cv, na.rm = TRUE),
#     sd_of_means = sd(mean_shannon, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# print("Between-group variation:")
# print(summary_stats_between_groups)
# 
# 
# 
# # Plot: Shannon diversity showing BETWEEN-GROUP variation
# be_p3 <- ggplot(summary_stats_between_groups, aes(x = sample_size, y = mean_of_means)) +
#   geom_line(linewidth = 1, color = "blue") +
#   geom_point(size = 3, color = "blue") +
#   geom_errorbar(aes(ymin = mean_of_means - mean_sd_between_groups, 
#                     ymax = mean_of_means + mean_sd_between_groups), 
#                 width = 1, alpha = 0.5, color = "blue") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Shannon diversity by sample size",
#        subtitle = "Variation BETWEEN groups (e.g., between years, ages, etc.)",
#        x = "Sample size",
#        y = "Shannon diversity (mean ± SD between groups)") +
#   theme_minimal()
# 
# print(be_p3)
# 
# # Plot: CV showing between-group variation
# be_p4 <- ggplot(summary_stats_between_groups, aes(x = sample_size, y = mean_cv_between_groups)) +
#   geom_line(linewidth = 1, color = "red") +
#   geom_point(size = 3, color = "red") +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Coefficient of Variation Between Groups by Sample Size",
#        subtitle = "CV of Shannon diversity across different groups",
#        x = "Sample Size",
#        y = "CV (between groups)") +
#   theme_minimal()+ ylim(0, NA)  # 0 to automatic maximum
# 
# print(be_p4)
# 
# 
# 
# 
# 
# 
# ## within group variability
# 
# 
# # ## List your dataframes
# # dataframes <- list(samples_species_season_year, samples_season_year_age,samples_species_age, samples_species_year)
# # Define grouping types
# grouping_types <- list(
#   "days42" = c("days42"),
#   "age_days42" = c("days42", "host_age_group"),
#   "age" = c("host_age_group"),
#   "age_year" = c("Year", "host_age_group"))
# 
# # Create a dataframe to store all results
# all_results <- data.frame()
# 
# # Loop through each grouping type
# for (grouping_name in names(grouping_types)) {
#   
#   grouping_vars <- grouping_types[[grouping_name]]
#   
#   # Loop through each sample size
#   for (sample_size in sample_sizes) {
#     
#     results_list <- vector("list", n_iterations)
#     
#     # Run iterations for current sample size and grouping
#     for (i in 1:n_iterations) {
#       
#       g_data_inv1 <- data_inv1 %>%
#         filter(
#           days21 %in% c('8', '9', '7', '10'), Year %in% c('2021', '2022'), !is.na(order)) %>%
#         select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
#         group_by(across(all_of(grouping_vars))) %>%
#         # Sample n unique sample_ids per group
#         filter(sample_id %in% {
#           unique_ids <- unique(sample_id)
#           sample(unique_ids, size = min(sample_size, length(unique_ids)))
#         }) %>%
#         ungroup()
#       
#       # Calculate diversity - pivot species to columns
#       g_data_div_sp <- g_data_inv1 %>%
#         filter(!is.na(species)) %>%
#         select(c(rra, species, all_of(grouping_vars), sample_id)) %>%
#         group_by(across(all_of(c("species", grouping_vars)))) %>%
#         filter(n_distinct(sample_id) >= 1) %>%
#         summarise(rra = mean(rra, na.rm = TRUE), .groups = "drop") %>%
#         pivot_wider(names_from = species, 
#                     values_from = rra, 
#                     values_fill = 0) %>%
#         ungroup()
#       
#       # Calculate Shannon diversity (excluding grouping columns)
#       diversity_cols <- g_data_div_sp %>% 
#         select(-all_of(grouping_vars))
#       
#       g_shannon_sp <- diversity(diversity_cols, index = "shannon")
#       
#       # Store results - one shannon value per demographic group
#       g_result_sp <- g_data_div_sp %>%
#         select(all_of(grouping_vars)) %>%
#         mutate(
#           shannon = g_shannon_sp,
#           run = i,
#           sample_size = sample_size,
#           grouping = grouping_name
#         )
#       
#       results_list[[i]] <- g_result_sp
#     }
#     
#     # Bind results for this sample size
#     g_sample_results <- bind_rows(results_list)
#     
#     # Append to overall results
#     all_results <- bind_rows(all_results, g_sample_results)
#   }
# }
# 
# # Summary statistics showing VARIATION WITHIN GROUPS (across runs)
# # This calculates mean/sd/cv ACROSS runs for each specific group
# summary_stats_within_groups <- all_results %>%
#   group_by(sample_size, grouping, across(any_of(c("Year", "host_age_group", "days21", "days42")))) %>%
#   summarise(
#     mean_shannon = mean(shannon, na.rm = TRUE),
#     sd_shannon = sd(shannon, na.rm = TRUE),
#     cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
#     n_runs = n(),
#     .groups = "drop"
#   )
# 
# print("Within-group variation (by specific group):")
# print(summary_stats_within_groups)
# 
# # Average within-group variation across all groups
# summary_stats_avg_within <- summary_stats_within_groups %>%
#   group_by(sample_size, grouping) %>%
#   summarise(
#     mean_shannon = mean(mean_shannon, na.rm = TRUE),
#     mean_sd_within_groups = mean(sd_shannon, na.rm = TRUE),
#     mean_cv_within_groups = mean(cv, na.rm = TRUE),
#     sd_of_shannon = sd(mean_shannon, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# print("Average within-group variation:")
# print(summary_stats_avg_within)
# 
# # Kruskal-Wallis test - comparing RUNS within groups
# kruskal_results_within_groups <- all_results %>%
#   group_by(sample_size, grouping, across(any_of(c("Year", "host_age_group", "days21", "days42")))) %>%
#   summarise(
#     kruskal_p = tryCatch(
#       kruskal.test(shannon ~ run)$p.value,
#       error = function(e) NA
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(sample_size, grouping) %>%
#   summarise(
#     mean_kruskal_p = mean(kruskal_p, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# print("Kruskal-Wallis test within groups (comparing runs):")
# print(kruskal_results_within_groups)
# 
# # Plot: Shannon diversity showing WITHIN-GROUP variation
# in_p3 <- ggplot(summary_stats_avg_within, aes(x = sample_size, y = mean_shannon)) +
#   geom_line(linewidth = 1, color = "blue") +
#   geom_point(size = 3, color = "blue") +
#   geom_errorbar(aes(ymin = mean_shannon - mean_sd_within_groups, 
#                     ymax = mean_shannon + mean_sd_within_groups), 
#                 width = 1, alpha = 0.5, color = "blue") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Shannon diversity by sample size",
#        subtitle = "Average variation WITHIN groups (across random samples)",
#        x = "Sample size",
#        y = "Shannon diversity (mean ± avg SD within groups)") +
#   theme_minimal()
# 
# print(in_p3)
# 
# # Plot: CV showing within-group variation
# in_p4 <- ggplot(summary_stats_avg_within, aes(x = sample_size, y = mean_cv_within_groups)) +
#   geom_line(linewidth = 1, color = "red") +
#   geom_point(size = 3, color = "red") +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange") +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   labs(title = "Coefficient of Variation Within Groups by Sample Size",
#        subtitle = "Average CV across runs for each group",
#        x = "Sample Size",
#        y = "Average CV (within groups)") +
#   theme_minimal()+ ylim(0, NA)  # 0 to automatic maximum
# 
# print(in_p4)
# 
# 
# # Combine both datasets with a label
# combined_stats <- bind_rows(
#   summary_stats_avg_within %>% 
#     select(sample_size, grouping, mean_cv = mean_cv_within_groups) %>%
#     mutate(variance_type = "Within-group"),
#   summary_stats_between_groups %>% 
#     select(sample_size, grouping, mean_cv = mean_cv_between_groups) %>%
#     mutate(variance_type = "Between-group")
# )
# 
# # Check the data
# print(head(combined_stats))
# print(table(combined_stats$variance_type))
# 
# # Define maximum samples for each grouping (based on your counts)
# max_samples_per_group <- data.frame(
#   grouping = c("days21", "age_year", "age_days42"),
#   max_samples = c(6,6, 6)  # Replace with your actual minimum counts
# )
# 
# # Merge with combined stats
# combined_stats <- combined_stats %>%
#   left_join(max_samples_per_group, by = "grouping")
# 
# # Combined plot with vertical lines
# combined_p4 <- ggplot(combined_stats, aes(x = sample_size, y = mean_cv, color = variance_type)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0.1, linetype = "dashed", color = "green", alpha = 0.5) +
#   geom_hline(yintercept = 0.2, linetype = "dashed", color = "orange", alpha = 0.5) +
#   geom_vline(aes(xintercept = max_samples), 
#              linetype = "dashed", color = "black", linewidth = 0.8) +
#   scale_color_manual(values = c("Within-group" = "blue", "Between-group" = "red")) +
#   facet_wrap(~grouping, scales = "free_y", ncol = 2) +
#   ylim(0, NA) +
#   labs(title = "Coefficient of Variation by Sample Size",
#        subtitle = "Within-group (sampling stability) vs Between-group (true variation)",
#        x = "Sample Size",
#        y = "CV",
#        color = "Variance Type",
#        caption = "Black dashed line: minimum available samples per group\nGreen = CV 0.1 (ideal), Orange = CV 0.2 (acceptable)") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# print(combined_p4)
# 
# 
# 
# 
# 
# data_inv1  %>%
#   filter(!is.na(order))%>%  group_by(Year) %>% 
#   summarise(count = n_distinct(sample_id))
# 
# data_inv1 %>%
#   filter(!is.na(order))%>% group_by(days21) %>% 
#   summarise(count = n_distinct(sample_id))
# 
# data_inv1  %>%
#   filter(!is.na(order))%>%  group_by(days42) %>% 
#   summarise(count = n_distinct(sample_id))
# 
# data_inv1  %>%
#   filter(!is.na(order))%>%  group_by(host_age_group) %>% 
#   summarise(count = n_distinct(sample_id))
# 
# 






## have to do it four seperate times for different filters???



## compare between and within group variance 

# Define the range of sample sizes to test
sample_sizes <- c(seq(2,24))
n_iterations <- 50

# on choosing safe iteration

# chance of duplication (birthday paradox)
#1 - exp(-iterations^2 / (2 * choose(pool_size, target sample_size)))
1 - exp(-75^2 / (2 * choose(24, 18)))

# Define grouping types - each has only ONE grouping variable
grouping_types <- list(
  "days42" = c("days42"),
  "age" = c("host_age_group"),
  "days21" = c("days21"),
  "year" = c("Year"))

# STEP 1: Create separate sample pools for each grouping type
# Select 25 random samples total for each grouping type
sample_pools <- list()

for (grouping_name in names(grouping_types)) {
  
  # Get 25 random unique sample_ids from data_inv1
  all_sample_ids <- unique(data_inv1$sample_id)
  selected_samples <- sample(all_sample_ids, size = 25)
  
  # Create pool with those 25 samples
  pool_data <- data_inv1 %>%
    filter(sample_id %in% selected_samples)
  
  # Verify the pool
  n_samples <- n_distinct(pool_data$sample_id)
  print(paste(grouping_name, "pool has", n_samples, "unique samples"))
  
  sample_pools[[grouping_name]] <- pool_data
}

# STEP 2: Run analyses using the pre-created pools
all_results <- data.frame()

for (grouping_name in names(sample_pools)) {
  
  grouping_vars <- grouping_types[[grouping_name]]
  pool_data <- sample_pools[[grouping_name]]
  
  # Loop through each sample size
  for (sample_size in sample_sizes) {
    
    results_list <- vector("list", n_iterations)
    
    # Run iterations for current sample size and grouping
    for (i in 1:n_iterations) {
      
      g_data_inv1 <- pool_data %>%
        select(sample_id, host_age_group, species, family, order, rra, days42, days21, Year, day_of_year) %>%
        # Sample n unique sample_ids from the pool (not grouped)
        filter(sample_id %in% {
          unique_ids <- unique(sample_id)
          sample(unique_ids, size = min(sample_size, length(unique_ids)))
        })
      
      # Calculate diversity - pivot species to columns
      g_data_div_sp <- g_data_inv1 %>%
        filter(!is.na(species)) %>%
        select(c(rra, species, all_of(grouping_vars), sample_id)) %>%
        group_by(across(all_of(c("species", grouping_vars)))) %>%
        filter(n_distinct(sample_id) >= 1) %>%
        summarise(rra = mean(rra, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = species, 
                    values_from = rra, 
                    values_fill = 0) %>%
        ungroup()
      
      # Calculate Shannon diversity (excluding grouping columns)
      diversity_cols <- g_data_div_sp %>% 
        select(-all_of(grouping_vars))
      
      g_shannon_sp <- diversity(diversity_cols, index = "shannon")
      
      # Store results - one shannon value per demographic group
      g_result_sp <- g_data_div_sp %>%
        select(all_of(grouping_vars)) %>%
        mutate(
          shannon = g_shannon_sp,
          run = i,
          sample_size = sample_size,
          grouping = grouping_name
        )
      
      results_list[[i]] <- g_result_sp
    }
    
    # Bind results for this sample size
    g_sample_results <- bind_rows(results_list)
    
    # Append to overall results
    all_results <- bind_rows(all_results, g_sample_results)
    print(paste("Completed:", grouping_name, "- Sample size:", sample_size, "- All", n_iterations, "iterations"))
  }
}

# STEP 3: Calculate summary statistics

# Summary statistics showing VARIATION BETWEEN GROUPS
summary_stats_between_groups <- all_results %>%
  group_by(sample_size, grouping, run) %>%
  summarise(
    mean_shannon = mean(shannon, na.rm = TRUE),
    sd_shannon = sd(shannon, na.rm = TRUE),
    cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
    n_groups = n(),
    .groups = "drop"
  ) %>%
  group_by(sample_size, grouping) %>%
  summarise(
    mean_of_means = mean(mean_shannon, na.rm = TRUE),
    mean_sd_between_groups = mean(sd_shannon, na.rm = TRUE),
    mean_cv_between_groups = mean(cv, na.rm = TRUE),
    sd_of_means = sd(mean_shannon, na.rm = TRUE),
    .groups = "drop"
  )

print("Between-group variation:")
print(summary_stats_between_groups)

# Summary statistics showing VARIATION WITHIN GROUPS
summary_stats_within_groups <- all_results %>%
  group_by(sample_size, grouping, across(any_of(c("Year", "host_age_group", "days21", "days42")))) %>%
  summarise(
    mean_shannon = mean(shannon, na.rm = TRUE),
    sd_shannon = sd(shannon, na.rm = TRUE),
    cv = sd(shannon, na.rm = TRUE) / mean(shannon, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

print("Within-group variation (by specific group):")
print(summary_stats_within_groups)

# Average within-group variation across all groups
summary_stats_avg_within <- summary_stats_within_groups %>%
  group_by(sample_size, grouping) %>%
  summarise(
    mean_shannon = mean(mean_shannon, na.rm = TRUE),
    mean_sd_within_groups = mean(sd_shannon, na.rm = TRUE),
    mean_cv_within_groups = mean(cv, na.rm = TRUE),
    sd_of_shannon = sd(mean_shannon, na.rm = TRUE),
    .groups = "drop"
  )

print("Average within-group variation:")
print(summary_stats_avg_within)

# Plot: Shannon diversity showing BETWEEN-GROUP variation
be_p3 <- ggplot(summary_stats_between_groups, aes(x = sample_size, y = mean_of_means)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = mean_of_means - mean_sd_between_groups, 
                    ymax = mean_of_means + mean_sd_between_groups), 
                width = 1, alpha = 0.5, color = "blue") +
  facet_wrap(~grouping, scales = "free_y", ncol = 2) +ylim(0.5,3.5)+
  labs(title = "Shannon diversity by sample size",
       subtitle = "Variation BETWEEN groups (25 samples per grouping type)",
       x = "Sample size",
       y = "Shannon diversity (mean ± SD between groups)") +
  theme_minimal()

print(be_p3)

# Plot: Shannon diversity showing WITHIN-GROUP variation
in_p3 <- ggplot(summary_stats_avg_within, aes(x = sample_size, y = mean_shannon)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = mean_shannon - mean_sd_within_groups, 
                    ymax = mean_shannon + mean_sd_within_groups), 
                width = 1, alpha = 0.5, color = "blue") +
  facet_wrap(~grouping, scales = "free_y", ncol = 2) + ylim(0.5,3.5)+
  labs(title = "Shannon diversity by sample size",
       subtitle = "Average variation WITHIN groups (across random samples)",
       x = "Sample size",
       y = "Shannon diversity (mean ± avg SD within groups)") +
  theme_minimal()

print(in_p3)


# Combine both datasets with a label
combined_stats <- bind_rows(
  summary_stats_avg_within %>% 
    select(sample_size, grouping, mean_cv = mean_cv_within_groups) %>%
    mutate(variance_type = "Within-group"),
  summary_stats_between_groups %>% 
    select(sample_size, grouping, mean_cv = mean_cv_between_groups) %>%
    mutate(variance_type = "Between-group")
)


facet_labels <- c(
  "days42" = "Days 42 (groups: 3-5)",
  "age" = "Age (nestling-adult)",
  "days21" = "Days 21 (groups: 8-10)",
  "year" = "Years (groups: 2020-2022)"
)

# Combined plot
combined_p4 <- ggplot(combined_stats, aes(x = sample_size, y = mean_cv, color = variance_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "forestgreen", alpha = 0.5) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "purple", alpha = 0.5) +
  scale_color_manual(values = c("Within-group" = "blue", "Between-group" = "red")) +
  facet_wrap(~grouping, scales = "free_y", ncol = 2, 
             labeller = labeller(grouping = facet_labels)) +  ylim(0, NA) +
  labs(title = "Coefficient of Variation by Sample Size",
       subtitle = "Between-group (true variation) vs Within-group (sampling stability) \n25 samples per grouping type, 50 iterations",
       x = "Sample Size",
       y = "CV",
       color = "Variance Type",
       caption = "Green < CV 0.1 (ideal), purple < CV 0.2 (acceptable)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(combined_p4)
