# Let the Coding Begin! ----
suppressPackageStartupMessages(
  suppressWarnings({
    library(DBI)
    library(gsheet)
    library(tidyverse)
    library(yaml)
  })
)


# Make FUN! ----
# swap columns
swap_if <- function(cond, x, y) {
  
  out_x <- if_else(cond, y, x)
  out_y <- if_else(cond, x, y)
  setNames(
    tibble(out_x, out_y), 
    c(deparse(substitute(x)), deparse(substitute(y)))
  )
}

# word pre-processing
clear_expression <- function(txt) {
  txt %>% 
    tolower() %>% 
    str_squish() %>% 
    str_replace_all("([:space:])[:space:]", "\\1") %>% 
    str_replace_all("i([:space:]|\\')", "I\\1") %>% 
    str_remove_all("^(to|a|an|the)[:space:]") %>% 
    str_replace_all("something", "sth") %>% 
    str_replace_all("somebody|someone", "sb")
}

# DB: Time to Wake Up ----
conn <- dbConnect(RSQLite::SQLite(), dbname = "eng_auto_dict.db")
word_dist <- dbGetQuery(conn, "select word from word_dict")
word_black_list <- dbReadTable(conn, "word_black_list")


# Sheet Happens ----
gsheet_secrets <- read_yaml("gsheet.yaml")
dict_google_link <- gsheet_secrets$link
sheetid_gtranslate <- gsheet_secrets$sheetid
gtranslate_raw <- gsheet2tbl(paste0(dict_google_link, sheetid_gtranslate))
source <- gsheet_secrets$owner[1]
source

# Scrub-a-Dub-Data ----
# pre-processing
google_names <- c("from", "to", "word", "translation")
clear_dict <-
  gtranslate_raw %>%
  rbind(names(.)) %>% 
  set_names(google_names) %>% 
  filter_at(vars(from, to), ~ . %in% c("английский", "русский")) %>% 
  filter(from != to) %>% 
  mutate(swap_if(from == "русский", translation, word)) %>% 
  mutate_at(vars(word), clear_expression) %>% 
  distinct(word, translation) %>% 
  group_by(word) %>% 
  top_n(1, wt = translation) %>% 
  ungroup() %>% 
  anti_join(word_black_list, by = "word") %>% 
  anti_join(word_dist, by = "word") %>% 
  add_column(source = !!source)

# adding new words in DB
dbAppendTable(conn, "word_dict", clear_dict)

# logging hisory
just_added_words <- 
  dbGetQuery(
    conn, 
    "
    SELECT word_id 
    FROM word_dict
    WHERE word = ?
    ",
    params = list(clear_dict$word)
  )

just_added_words %>% 
  mutate(
    action = "adding",
    action_dt = format(Sys.Date(), "%Y-%m-%d")
  ) %>%
  dbAppendTable(conn, "word_history", .)
  
# Happy End ----
dbDisconnect(conn)
