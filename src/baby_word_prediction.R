
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
  mutate(produces = value == "produces") %>%
  group_by(num_item_id) %>%
  inner_join(words, by = "num_item_id") %>%
  summarize(median = median(age))

plot(start_produce$median, start_produce$start)

words

df
