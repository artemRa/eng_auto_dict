library(tidyverse)
library(DBI)

web_link <- "https://dictionary.cambridge.org/help/codes.html"
labels_dict_raw <- xml2::read_html(web_link)

labels_dict_list <- labels_dict_raw %>% 
  rvest::html_table() %>% 
  map(set_names, nm = c("label", "description"))

tbl_names <- labels_dict_list %>% purrr::map_chr(~ .x$label[1])
clear_dict <- purrr::map2_dfr(tbl_names, labels_dict_list, ~ tibble(block = .x, .y))

conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = "eng_auto_dict.db")
DBI::dbSendQuery(
  conn,
  "
    CREATE TABLE labels_and_codes_dict (
      block TEXT,
      label TEXT,
      description TEXT
    )
  "
  )
DBI::dbAppendTable(conn, "labels_and_codes_dict", clear_dict)

# manual fix
DBI::dbSendQuery(
  conn, 
  "
  DELETE FROM labels_and_codes_dict
  WHERE block = 'verb' and label in ('[+ question word]', '[+ to infinitive]')
  "
)

DBI::dbSendQuery(
  conn, 
  "
  UPDATE labels_and_codes_dict
  SET label = '[+ -ing verb]'
  WHERE label = '[+ -ing] verb'
  "
)

DBI::dbDisconnect(conn)
