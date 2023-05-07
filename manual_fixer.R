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


# Manual fixing ----
# find_this_word("i feel it (conversation)")
# send_to_blacklist(63)
