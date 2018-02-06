
# install.packages("devtools")
# devtools::install_github("langcog/wordbankr")
library("dplyr")
library("wordbankr")
library("stringr")
library("ggplot2")

ls("package:wordbankr")

admins <- 
  get_administration_data() %>%
  select(data_id, age, language, form)

items <- 
  get_item_data() %>%
  mutate(num_item_id = as.numeric(substr(item_id, 6, nchar(item_id))),
         definition = tolower(definition))

words <- 
  items %>%
  filter(type == "word", !is.na(uni_lemma), form == "WG")

unique(items$language)

dim(items)

dim(words)

starting_sound <-
  words %>%
  mutate(start = str_extract(definition, "^[a-zA-Z]")) %>%
  filter(!is.na(start))

dim(starting_sound)

start_freq <- table(starting_sound$start)
start_freq

head(sort(start_freq, decreasing = TRUE))

barplot(as.vector(start_freq))

english_ws_admins <- get_administration_data("English (American)", "WS")

df <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   administrations = english_ws_admins)

start_produce <-
  df %>%
  select(num_item_id, value, age, language) %>%
  filter(value == "produces") %>%
  inner_join(starting_sound, by = "num_item_id") %>%
  mutate(start = factor(start, letters))

mean_age <-
  start_produce %>%
  group_by(start) %>%
  summarize(mean = mean(age))

head(arrange(mean_age, mean))

qplot(mean_age$start, mean_age$mean)
