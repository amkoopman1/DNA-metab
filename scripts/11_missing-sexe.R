### missing sexe value

database_link_extraction <- "https://docs.google.com/spreadsheets/d/1kRjM-joPMJhcgCTg3kDBM7eO6BX38MmkYNYHS6QoDIk/edit"


database_extraction <- read_gsdb(database_link_extraction)
total_df_samples <- database_extraction[["total_list_samples"]]

database_link3 <- "https://docs.google.com/spreadsheets/d/1bzJzrihGSMf62R97TgQrshgoHXCM8cqVKMqIYb29ebY/edit"

database3 <- read_gsdb(database_link3)
names(database3)

database_link_sex_nest <- "https://docs.google.com/spreadsheets/d/11_BI9-guOv75FAJ0vcZw6G3VJVEjMxx2D7wVAYdfo7E/"

database_sex_nest <- read_gsdb(database_link_sex_nest)

# sexe missing

# first split into adult and nestling

# for nestling, take out those who are nestlevel. then join to wender data to find ringnummer 
nestlings_df <- database3[["Nestlings"]] %>%
  mutate(Feces = str_sub(Feces, -6, -1))

nestlevel_df <- database3[["Nestlevel"]] %>%
  mutate(Feces = str_sub(Feces, -6, -1))

captures_df <- database3[["Nestlevel"]] %>%
  mutate(Feces = str_sub(Feces, -6, -1))


total_df_samples %>% 
  filter(Age == "nestling") %>%
  mutate(Feces = Sample_ID ) %>%
  anti_join(nestlevel_df, "Feces") %>%
  select(Feces) %>%
  left_join(nestlings_df, "Feces") %>%
  select(Ringnumber, Date, Nest_ID, Feces)%>%
  sheet_write(ss = database_link_sex_nest,
            sheet = paste0("sexe_nestlings"))

# for adult, join to captures data and filter for sexe unknown

total_df_samples %>% 
  filter(Age == "N1") %>%
  mutate(Feces = Sample_ID ) %>%
  select(Feces) %>%
  left_join(captures_df, "Feces") %>%
  filter(Sexe == "Onbekend") %>%
  select(Ringnumber, Date, Feces, Location)%>%
  sheet_write(ss = database_link_sex_nest,
              sheet = paste0("sexe_captures"))
 

# nest id 

rbind(
  (total_df_samples %>% 
    filter(Age == "nestling") %>%
    mutate(Feces = Sample_ID ) %>%
    select(Feces) %>%
  inner_join(nestlevel_df, "Feces") %>%
    select(Nest_ID)),
  
  (total_df_samples %>% 
    filter(Age == "nestling") %>%
    mutate(Feces = Sample_ID ) %>%
    select(Feces) %>%
  inner_join(nestlings_df, "Feces") %>%
    select(Nest_ID))) %>%
  unique() %>%
  sheet_write(ss = database_link_sex_nest,
              sheet = paste0("nest_ids"))
