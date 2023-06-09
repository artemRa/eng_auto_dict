# Let the Coding Begin! ----
suppressPackageStartupMessages(
  suppressWarnings({
    library(DBI)
    library(tidyverse)
  })
)


# Make FUN! ----
# find word in word_dict table
find_this_word <- function(word, dbname = "eng_auto_dict.db") {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  output <- dbGetQuery(
    conn,
    "
    SELECT *
    FROM word_dict
    WHERE word = ?
    ",
    params = list(word)
  ) 
  dbDisconnect(conn)
  output
}

# send word to blacklist
send_to_blacklist <- function(word_id, dbname = "eng_auto_dict.db") {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  selected <- dbGetQuery(
    conn,
    "
    SELECT word
    FROM word_dict
    WHERE word_id = ?
    ",
    params = list(word_id)
  ) %>% pull()
  dbSendQuery(
    conn,
    "
    INSERT INTO word_black_list 
    VALUES (?)
    ",
    params = list(selected)
  )
  dbSendQuery(
    conn,
    "
    DELETE FROM word_dict
    WHERE word_id = ?
    ",
    params = list(word_id)
  )
  dbDisconnect(conn)
}

# send word to blacklist
word_fix <- function(word_id, wrd = NULL, trs = NULL, dbname = "eng_auto_dict.db") {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  selected <- dbGetQuery(
    conn,
    "
    SELECT word, translation
    FROM word_dict
    WHERE word_id = ?
    ",
    params = list(word_id)
  )
  
  # translation bug
  if (!is_null(trs)) {
    dbSendQuery(
      conn,
      glue::glue_sql(
        "
        UPDATE word_dict
        SET translation = {trs}
        WHERE word_id = {word_id}
        ",
        .con = conn
      )
    )
  }
  
  # word bug
  if (!is_null(wrd)) {
    dbSendQuery(
      conn,
      glue::glue_sql(
        "
        UPDATE word_dict
        SET word = {wrd}
        WHERE word_id = {word_id}
        ",
        .con = conn
      )
    )
    dbSendQuery(
      conn,
      "
      INSERT INTO word_black_list 
      VALUES (?)
      ",
      params = list(selected$word)
    )
  }
  
  dbDisconnect(conn)
}


# Manual fixing ----
conn <- dbConnect(RSQLite::SQLite(), dbname = "eng_auto_dict.db")
word_dict <- dbReadTable(conn, "word_dict") %>% as_tibble()
view(word_dict)

# find_this_word()
# word_fix()
# word_fix()
# send_to_blacklist()

source("lib.R")
source("web_fun.R")

words_to_check <- 
  dbGetQuery(
    conn,
    "
    SELECT * FROM word_dict
    WHERE word_id not in (SELECT word_id FROM word_cambridge_examples)
    "
  )

words_to_check_test <- words_to_check %>% 
  filter(str_detect(word, "[[:space:]]"))

cmb <- list()
for (i in 1:nrow(words_to_check_test)) {
  cmb[[i]] <- cambridge_express(words_to_check_test$word[i])
}
bad_match_tbl <- map(cmb, as_tibble) %>% 
  reduce(rbind) %>% 
  filter(!success) %>% 
  inner_join(words_to_check_test, by = "word")

view(select(bad_match_tbl, -c(link, success)))
