# Auto Dict
# v 1.0.


# Logs are above all  ----
log_it <- function(text) message(paste(Sys.time(), text))
log_it("Lets start!")


# Libraries hangout ----
log_it("Loading libraries")
suppressPackageStartupMessages(
  suppressWarnings({
    library(DBI)
    library(tidyverse)
    library(rvest)
    library(blastula)
    library(glue)
    library(reticulate)
    library(tidytext)
    library(praise)
  })
)


# Configs time ----
log_it("Configs setting")
max_repeat_cnt = 7L # max iter of word repeating
meaning_length = 3L # max number of word meanings
max_attempt = 5L # max attempts of email sending
email_secrect <- yaml::read_yaml("email.yaml") # secret config based on list() structure
invisible(Sys.setlocale("LC_TIME", "English")) # ignoring local time setting


# Make FUN! ----
log_it("Loading custom FUN")

# any word to simple web link
word_to_link <- function(word, domen = "https://dictionary.cambridge.org/dictionary/english") {
  word_for_link <- str_replace_all(word, pattern = "[:space:]|[:punct:]", replacement = "-")
  paste(domen, word_for_link, sep = "/")
}


# read html ignoring errors
read_html_iter <- function(web_link, max_attempt = 10) {
  
  for (i in 1:max_attempt) {
    export_html <- try(xml2::read_html(web_link), silent = TRUE)
    if (class(export_html)[1] != "try-error") break
  }
  
  export_html
}


# word to web link by Cambridge spellchecker
cambridge_query_link <- function(
    word, 
    site = "https://dictionary.cambridge.org", 
    query = "/spellcheck/english/?q=") {
  
  export_html <- word_to_link(word, domen = paste0(site, query)) %>% 
    read_html_iter()
  
  web_links <- export_html %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset(pattern = site) %>% 
    str_subset(pattern = "/?q") %>% 
    str_replace_all(pattern = " ", replacement = "+")
  
  web_links[1] # first link from spellchecker
}


# any word main form, web link and short definition
cambridge_express <- function(word) {
  
  success <- T 
  
  # load html page by simple word link
  web_link <- word_to_link(word)
  export_html <- read_html_iter(web_link)
  
  # exploring page meta data
  meta_data <- export_html %>% html_elements("meta")
  meta_attr <- c("name", "content", "property")
  meta_df <- map(meta_attr, ~ html_attr(meta_data, .)) %>% 
    set_names(meta_attr) %>% 
    as_tibble()
  
  # extracting name of found page
  found_word <- meta_df %>% 
    filter(property == "og:title") %>% 
    pull(content)
  
  # using Cambridge speller to find word similar to original one
  if (str_detect(found_word, "Cambridge")) {
    
    success <- F
    
    web_link <- cambridge_query_link(word)
    export_html <- read_html_iter(web_link)
    
    meta_data <- export_html %>% html_elements("meta")
    meta_attr <- c("name", "content", "property")
    meta_df <- map(meta_attr, ~ html_attr(meta_data, .)) %>% 
      set_names(meta_attr) %>% 
      as_tibble()
    
    found_word <- meta_df %>% 
      filter(property == "og:title") %>% 
      pull(content)
  }
  
  # page link
  link <- meta_df %>% 
    filter(property == "og:url") %>% 
    pull(content)
  
  # word description
  descr <- meta_df %>% 
    filter(property == "og:description") %>% 
    pull(content) %>% 
    gsub("2.*","",.) %>% 
    gsub(":*","",.) %>% 
    str_replace("1. ", "") %>% 
    str_squish()
  
  # getting html page body data
  body <- html_elements(export_html, "body")
  body_div_class <- html_elements(body, "div")
  div_names <- map_chr(body_div_class, html_attr, name = "class")
  title_line <- body_div_class[which(div_names == "ddef_h")][1]
  title_span <- html_elements(title_line, "span")
  title_class <- html_attr(title_span, "class")
  
  # getting info about main word form (word & link)
  if("usage dusage" %in% title_class && "x dx" %in% title_class) {
    
    next_page <- title_span[which(title_class == "x dx")] %>% 
      html_nodes("a")
    
    link <- next_page %>% 
      html_attr("href") %>% 
      paste0("https://dictionary.cambridge.org", .)
    
    found_word <- html_text2(next_page)
  }
  
  # output
  list(
    word = word, 
    original = found_word, # founded word
    link = link, 
    descr = descr, 
    success = success
  )
}


# extract all possible word forms
py_word_forms <- function(word) {
  word_forms <- reticulate::import("word_forms.word_forms")
  word_list <- word_forms$get_word_forms(word)
  map(word_list, iterate)
}


# all word forms and their definition
exploring_word_forms <- function(word) {
  
  # clearing input word from stop-words
  unnest_words <- word %>% 
    as_tibble() %>% 
    unnest_tokens(word, value) %>% 
    anti_join(get_stopwords("en", "snowball"), by = "word") %>% 
    filter(!word %in% c("sth", "sb"))
  
  # empty output tables
  word_tbl <- 
    tibble(
      type = character(),
      word = character()
    )
  
  tbl <- 
    tibble(
      type = character(),
      word = character(),
      original = character(),
      link = character(),
      descr = character(),
    )
  
  cambridge_tbl <- 
    tibble(
      word = character(),
      original = character(),
      link = character(),
      descr = character(),
      success = logical()
    )
  
  show_it <- F
  
  # block only for single word
  if (nrow(unnest_words) == 1) {
    
    only_word <- pull(unnest_words)
    word_forms <- py_word_forms(only_word)
    word_tbl <- word_forms %>% 
      purrr::compact() %>%
      map2_dfr(names(.), ., ~ tibble(type = .x, word = .y))
    
    if (nrow(word_tbl) > 0) {
      
      # searching words in Cambridge.org
      cambridge_tbl <- word_tbl %>% 
        distinct(word) %>% 
        pull() %>% 
        map_df(cambridge_express)
      
      cambridge_success_tbl <- cambridge_tbl %>% 
        filter(success) %>% 
        select(-success)
      
      # save only words which was correctly found in cambridge.org
      word_ext_tbl <- word_tbl %>% 
        inner_join(cambridge_success_tbl, by = "word")
      
      # simple -s forms of nouns and verbs
      sufix_tbl <- word_ext_tbl %>% 
        filter(original != word) %>% 
        filter(type %in% c("n", "v")) %>% 
        filter(str_detect(word, original)) %>% 
        filter(str_replace(word, original, "") %in% c("s", "es")) %>% 
        mutate(word_fix = glue("{original}({sufix})", sufix = str_replace(word, original, ""))) %>% 
        select(type, word, original, word_fix)
      
      # pre tbl for print
      tbl <- word_ext_tbl %>% 
        filter(word == original) %>% 
        anti_join(sufix_tbl, by = c("type", "word")) %>%
        left_join(select(sufix_tbl, -word), by = c("type", "original")) %>% 
        arrange(type, word = original, str_length(word)) %>% 
        mutate(word = coalesce(word_fix, word), .keep = "unused") %>% 
        add_row(filter(word_ext_tbl, word != original)) %>% 
        anti_join(sufix_tbl, by = c("type", "word"))
      
      show_it <- T
      
    }
  }
  
  list(
    show_it = show_it,
    py_forms = word_tbl,
    lv1_forms = tbl,
    lv2_forms = cambridge_tbl
  )
  
}


# transform html obj into tibble with span names
tags_to_tibble <- function(html, type = "span"){
  
  tags <- html %>% html_elements(type)
  name <- tags %>% map_chr(html_attr, name = "class")
  tibble(name, tags = map_chr(tags, html_text2))
  
}


# simplify tag tibble
tag_tibble_to_clear <- function(input, word) {
  
  tibble(
    word = 
      input %>% 
      filter(name == "hw dhw") %>% 
      head(1L) %>% 
      pull(tags) %>% 
      paste(collapse = " "),
    name = 
      input %>% 
      filter(name %in% c("hw dhw", "pos dpos")) %>% 
      pull(tags) %>% 
      paste(collapse = " "),
    data = 
      input %>% 
      filter(name %in% c("hw dhw", "pos dpos", # word and part of speech 
                         "gram dgram", # label
                         "usage dusage", # usage
                         # "region dreg", # pronunciation: region
                         # "pron dpron", # pronunciation: transcription
                         "var dvar", "v dv lmr-0", # additional word
                         "irreg-infls dinfls ") # irregular forms
      ) %>% 
      filter(!(name == "hw dhw" & tags == word)) %>% 
      mutate_at("tags", ~if_else(name == "var dvar", "<br>", .)) %>% # br in case of additional forms
      pull(tags) %>% 
      paste(collapse = " ")
  )
}


# convert tag to sentence
tags_to_sentences <- function(data) {
  
  extracted_text <- data %>% 
    html_text2() %>% 
    map_chr(str_replace_all, pattern = "\n", replacement = "<br>")
  
  ifelse(is_empty(extracted_text), "", extracted_text)
}


# get random emoji by iter number
title_emoji <- function(x, emoji_types = c("fruit", "beard", "older_man", "undead", "ghost", "djinn", "stone")) {
  
  level_number <- length(emoji_types)
  emoji_number <- min(x, level_number)
  emo::ji(emoji_types[emoji_number])
  
}


# find and replace by pattern words in vector of text
find_and_replace_words <- function(text_vector, replacement_words, replacement_pattern = '<span style="color: #BA2649">{wf}</span>') {
  
  replacement_vect <- rep(as.character(NA), length(text_vector))
  replacement_count <- rep(0L, length(text_vector))
  
  for (j in 1:length(text_vector)) {
    for (k in 1:length(replacement_words)) {
      wf <- replacement_words[k]
      detec <- str_detect(text_vector[j], glue('\\b{wf}\\b'))
      if (detec & !is.na(detec)) {
        extract <- str_extract(text_vector[j], glue('\\b{wf}\\b'))
        replacement_vect[j] <- wf
        replacement_count[j] <- replacement_count[j] + 1L
        text_vector[j] <- 
          str_replace_all(
            text_vector[j],
            glue('\\b{wf}\\b'),
            glue(replacement_pattern)
          )
      }
    }
  }
  
  tibble(
    txt = text_vector,
    wrd = replacement_vect,
    cnt = replacement_count
  )
  
}