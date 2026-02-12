## inspect distribution of samples

gsheets_auth()

# metadata samples Wender
database_link <- "https://docs.google.com/spreadsheets/d/1ocuMq1ENjgwsVZ4uvhgyqwzb7D1d6v8HvuZTSrQ3kY0/edit"
database <- read_gsdb(database_link)
names(database)

# analysed samples by Wender
database_link2 <- "https://docs.google.com/spreadsheets/d/1KV5IShV1a_4ZouF-I6KCRetfNwX5Zx_rwlCrMGk103E/edit"
database_2 <- read_gsdb(database_link2)
names(database_2)


# 
# 2- Read two specific sheets:
# subset <- read_gsdb(database, sheets = c("Species", "Transects"))
# 
# 3 - Read three specific sheets, with a specific filter for each sheet
# filtered <- read_gsdb(database_link,sheets = c("Species", "Transects", "Observations"),
#    filter = list(
#                  Species      = ~ !is.na(Species_ID),
#                  Transects    = ~ Region == "Loita",
#                  Observations = ~ cover > 0))

## inspect Captures

cap_data <- database[["Captures"]]
names(cap_data)

# subset and give new column for month and year
sub_cap_data <- cap_data %>%
  select(Date, Species, Age) %>%
  mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  select(-c(Date))

count_cap_data <- sub_cap_data %>%
  filter(Age == 'N1')%>%
  group_by(Species, Age, Month, Year) %>%
  summarise(count = n()) 

#View(count_cap_data)

c_karekiet_count <- count_cap_data %>%
  filter(Species == 'Kleine karekiet')
#View(c_karekiet_count)
ggplot(c_karekiet_count, aes(Month, count, color = Year)) + geom_point()+
  ggtitle("c_karekiet_count <- count_cap_data")

#ggplot(c_karekiet_count, aes(Year, count, color = Month)) + geom_point()

c_rietz_count <- count_cap_data %>%
  filter(Species == 'Rietzanger')
#View(c_rietz_count)
ggplot(c_rietz_count, aes(Month, count, color = Year)) + geom_point()+
  ggtitle("c_rietz_count <- count_cap_data")

c_bosr_count <- count_cap_data %>%
  filter(Species == 'Bosrietzanger')
#View(c_bosr_count)
ggplot(c_bosr_count, aes(Month, count, color = Year)) + geom_jitter()+
  ggtitle("c_bosr_count <- count_cap_data ")



## inspect Nestlings

nling_data <- database[["Nestlings"]]
names(nling_data)

# subset and give new column for month and year
sub_nling_data <- nling_data %>%
  select(Date, Species) %>%
  mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  select(-c(Date))

count_nling_data <- sub_nling_data %>%
  group_by(Species, Month, Year) %>%
  summarise(count = n()) 

#View(count_nling_data)

n_karekiet_count <- count_nling_data %>%
  filter(Species == 'Kleine karekiet')
#View(n_karekiet_count)
ggplot(n_karekiet_count, aes(Month, count, color = Year)) + geom_point()+
  ggtitle("n_karekiet_count <- count_nling_data")

n_rietz_count <- count_nling_data %>%
  filter(Species == 'Rietzanger')
#View(n_rietz_count)
ggplot(n_rietz_count, aes(Month, count, color = Year)) + 
  geom_jitter() +
  ggtitle("n_rietz_count <- count_nling_data")

n_bosr_count <- count_nling_data %>%
  filter(Species == 'Bosrietzanger')
#View(n_bosr_count)
ggplot(n_bosr_count, aes(Month, count, color = Year)) + geom_point() +
  ggtitle("n_bosr_count <- count_nling_data")



## inspect Nestlevel

nlev_data <- database[["Nestlevel"]]
names(nlev_data)

# subset and give new column for month and year
sub_nlev_data <- nlev_data %>%
  select(Date, Species) %>%
  mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  select(-c(Date))

count_nlev_data <- sub_nlev_data %>%
  group_by(Species, Month, Year) %>%
  summarise(count = n()) 

#View(count_nlev_data)

nl_karekiet_count <- count_nlev_data %>%
  filter(Species == 'Kleine karekiet')
#View(nl_karekiet_count)
ggplot(nl_karekiet_count, aes(Month, count, color = Year)) + geom_point()+
  ggtitle("nl_karekiet_count <- count_nlev_data")

nl_rietz_count <- count_nlev_data %>%
  filter(Species == 'Rietzanger')
#View(nl_rietz_count)
ggplot(nl_rietz_count, aes(Month, count, color = Year)) + 
  geom_jitter() +
  ggtitle("nl_rietz_count <- count_nlev_data")

nl_bosr_count <- count_nlev_data %>%
  filter(Species == 'Bosrietzanger')
#View(nl_bosr_count)
ggplot(nl_bosr_count, aes(Month, count, color = Year)) + geom_point()+
  ggtitle("nl_bosr_count <- count_nlev_data")



## inspect what has already been analysed

an_data1 <- database_2[["Sheet1"]]

an_data <- bind_rows(
    cap_data %>% select(Date, Species, Feces) %>% mutate(source = "captures"),
    nling_data %>% select(Date, Species, Feces) %>% mutate(source = "nest"),
    nlev_data %>% select(Date, Species, Feces) %>% mutate(source = "nest")
  )

count_an_data <-  an_data %>%
  mutate(Feces = str_sub(Feces, -6, -1))%>%
  mutate(processed = ifelse(Feces %in% an_data1$sample_id, "yes", "no"),
         Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  select(-c(Date)) %>%
  group_by(Species, Month, Year, source, processed) %>%
  summarise(count = n()) 

#View(count_an_data)

yes_an_data <- count_an_data %>%
  filter(processed == 'yes')

ggplot(yes_an_data, aes(Month, count, color = source)) + geom_jitter()



# for Rietzanger captures, check how many samples are already processed.
# suitable years: '20, '21 and '22, months 4, 5, 6, 7. 15 samples per group. 
# only for adults. 

yn_rietz <- cap_data %>% 
  filter(Species == 'Rietzanger' & Age == 'N1') %>%
  mutate(Feces = str_sub(Feces, -6, -1))%>%
  mutate(processed = ifelse(Feces %in% an_data$sample_id, "yes", "no"),
         Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  select(-c(Date)) %>%
  group_by(Species, Month, Year, processed) %>%
  summarise(count = n())

#View(yn_rietz)


count_an_data_20_22 <- count_an_data %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Year == '2022')

ggplot(count_an_data_20_22, aes(Month, count, color = Species, shape = source)) + geom_jitter()


# look at year 2022 month 6 captures + nest of karekiet + rietzanger
rk_22_an_data <- an_data %>%
mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Year == '2022') %>%
  group_by(Species, Month, Year, source) %>%
  summarise(count = n())

ggplot(rk_22_an_data, aes(Month, count, color = Species, shape = source)) + geom_jitter()

# rietzanger  + karekiet for months 5-7
rk_57_an_data <- an_data %>%
  mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger'), Month %in% c('05', '06', '07'), Year != '2023') %>%
  group_by(Species, Month, Year, source) %>%
  summarise(count = n())

ggplot(rk_57_an_data, aes(Month, count, color = Species, shape = source)) + geom_jitter()

# bosrietzanger 

bosr_an_data <- an_data %>%
  mutate(Month = substr(Date, 6, 7), Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Bosrietzanger')) %>%
  group_by(Month, source, Year) %>%
  summarise(count = n())

ggplot(bosr_an_data, aes(Month, count,shape = source, color = Year)) + geom_point()




# using time bins instead of months

# rietzanger + karekiet, years collapsed, 10 days per bin
rk_tb_an_data <- an_data %>% 
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         hectad = ceiling(day_of_year / 10), 
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Kleine karekiet', 'Rietzanger')) %>%
  group_by(Species, hectad, source) %>%
  summarise(count = n())

ggplot(rk_tb_an_data, aes(hectad, count, color = Species, shape = source)) + 
  geom_jitter()

# rietzanger adult + nestling time bin days 
r_tb_data <- an_data %>% 
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         hectad = ceiling((day_of_year +0) / 21), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species %in% c('Rietzanger')) %>%
  group_by(Species, hectad, source, Year) %>%
  summarise(count = n())


# if 2023 is excluded, for every 21 days, bins 6-10 (= 105 days) each have at least 10 
# samples per adult and nestling per year. 
ggplot(r_tb_data, aes(hectad, count, color = Year, shape = source)) + 
  geom_jitter()



# if species and years are collapsed, can capture + nest per week for about 100 days, but likely severe unevenness of data 
a_tb_data <- an_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         hectad = ceiling((day_of_year +0) / 7), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  group_by(hectad, source) %>%
  summarise(count = n())

ggplot(a_tb_data, aes(hectad, count, shape = source)) + 
  geom_jitter()


# two species 42 days bins 
rk_35_data <- an_data %>%
  mutate(day_of_year = yday(Date),  # Day of year (1-365)
         timebin = ceiling((day_of_year +0) / 42), # the + changes the window of time
         Year = substr(Date, 1,4)) %>%
  filter(Species != "Bosrietzanger", Year != '2023')%>%
  group_by(timebin, source, Year, Species) %>%
  summarise(count = n())

ggplot(rk_35_data, aes(timebin, count, shape = source)) + 
  geom_jitter()
