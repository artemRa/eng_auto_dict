# Auto Dict
# v4.6


# Logs Above All  ----
log_it <- function(text) message(paste(Sys.time(), text))
log_it("Lets start!")


# Libraries Hangout ----
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


# Config-tastic Adventures ----
log_it("Configs setting")
max_repeat_cnt = 7L # max iter of word repeating
meaning_length = 3L # max number of word meanings
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


# Dice Roll in the Pocket DB ----
log_it("Loading data from DB")
conn <- dbConnect(RSQLite::SQLite(), dbname = "eng_auto_dict.db")
sql_query <-
  "
  SELECT word_id, word, translation, meaning, examples, iter, max_dt
  FROM 
  (
    SELECT a.*
    , e.examples
    , m.meaning
    , b.max_dt, b.iter
    , row_number() over (partition by b.iter order by random()) as rn
    FROM word_dict a
    JOIN 
    (
      SELECT word_id, max(action_dt) as max_dt, count(*) as iter
      FROM word_history
      WHERE action in ('adding', 'email')
      GROUP BY word_id

    ) b on a.word_id = b.word_id
    LEFT JOIN word_meaning m on a.word_id = m.word_id
    LEFT JOIN word_examples e on a.word_id = e.word_id
  ) 
  WHERE rn = 1
  ORDER BY iter
  "

# оne random word from each iteration group
export_words <- 
  dbGetQuery(conn, sql_query) %>% 
  as_tibble() %>% 
  mutate_at(vars(max_dt), as.Date) %>% 
  filter(iter <= !!max_repeat_cnt)

# selecting words for email
more_iter <- setdiff(export_words$iter, 1L) %>% 
  sample(x = ., size = 2L, prob = 1L/(.)^2L)

selected_words <- 
  export_words %>% 
  filter(iter %in% c(1L, more_iter)) %>% 
  arrange(iter)

# empty tbl for title emoji
tiltle_emoji_df <- selected_words[, "iter"] %>% 
  add_column(emoji = as.character(NA))


# Cook Up Some Email Delight ----
log_it("Start of cooking")
email_words_list <- list()

# big loop, an iter for every selected word
for (i in 1:nrow(selected_words)) {
  
  ## Head & meta info ----
  # base meta info about word
  word <- selected_words$word[i]
  word_id <- selected_words$word_id[i]
  word_translate <- selected_words$translation[i]
  word_iter <- selected_words$iter[i]
  word_date <- format(selected_words$max_dt, "%b %e, %Y")[i]
  log_it(paste(i, word))

  # selected title emo::ji
  temoji <- title_emoji(word_iter) %>% as.character()
  tiltle_emoji_df[i, "emoji"] <- temoji
  # HTML word head
  html_word_head <- 
    paste(
      "<h1>", temoji, word,
      "<h2>", emo::ji("ru"), word_translate, "<br>", 
      "<small>", emo::ji("thirty"), word_date, 
      "</small>", "</h2>",
      "</h1>"
    )
  
  # Word forms ----
  # load word forms data
  word_forms_list <- exploring_word_forms(word)
  
  # HTML part of word forms
  forms <- word_forms_list$lv1_forms %>% 
    select(type, word) %>% 
    nest(data = c(word)) %>% 
    mutate(vect = map(data,
                      ~ pull(.) %>% 
                        glue_collapse(sep = ", "))) %>% 
    unnest(vect) %>% 
    glue_data("<b>{type}:</b> {vect}") %>% 
    paste(collapse = "<br>")
  
  # HTML part of defenitions
  descr <- word_forms_list$lv2_forms %>% 
    group_by(descr, original) %>% 
    arrange(str_length(word)) %>% 
    filter(row_number(desc(word == original)) == 1L) %>% 
    filter(word == original) %>% 
    filter(!str_starts(descr, "past participle|past simple|present participle")) %>% 
    arrange(desc(success), str_length(word)) %>% 
    ungroup() %>% 
    glue_data('<b><a href="{link}">{word}</a>:</b> {descr}<br>') %>% 
    paste(collapse = "<br>")
  
  # HTML word forms
  html_word_forms <- 
    if_else(word_forms_list$show_it, 
            paste(
              "<p>", forms, "</p>",
              "<p>", descr, "</p>"
            ), "")
  
  
  # Word meaning ----
  
  # main HTML page info
  express_tbl <- cambridge_express(word)
  found_word <- express_tbl$original
  
  # green label if it's true
  fuzzy_match <- str_detect(word, found_word) || str_detect(found_word, word)
  
  # Cambridge part title
  html_cambridge_head <- 
    paste(
      "<h3>", emo::ji(
        case_when(
          found_word == word ~ "check",
          fuzzy_match ~ "negative_squared_cross_mark",
          TRUE ~ "warning")
      ),
      found_word, "<br>",
      "<small>",
      emo::ji("link"),
      glue('<a href="{link}">dictionary.cambridge.org</a>', link = express_tbl$link),
      "</small>",
      "</h3>"
    )
  
  # parsing HTML page
  export_html <- read_html_iter(express_tbl$link)
  
  # html body and span names
  body <- export_html %>% html_elements("body")
  span_names <- body %>% 
    html_elements("span") %>% 
    map_chr(html_attr, name = "class")
  
  # div html elements
  div_data <- body %>% html_elements("div")
  div_names <- div_data %>% map_chr(html_attr, name = "class")
  
  # word details (usage, pronoun, and etc.)
  div_details <- div_data[which(div_names == "pos-header dpos-h")]
  
  # word spelling details 
  pron_dron <- ifelse(
    length(div_details) == 0, "",
    div_details %>% 
      map_df(tags_to_tibble) %>% 
      filter(name %in% c("region dreg", "pron dpron")) %>% 
      mutate(
        region = if_else(tags %in% c("uk", "us"), tags, as.character(NA)),
        .before = 1L
      ) %>% 
      fill(region, .direction = "down") %>% 
      filter(name == "pron dpron") %>% 
      group_by(region) %>% 
      filter(row_number() <= 1L) %>% 
      ungroup() %>% 
      distinct(region, tags) %>%
      mutate(emj = map_chr(region, ~emo::ji(.))) %>% 
      glue_data("{emj} {tags}") %>% 
      paste0(collapse = "<br>"))
  
  # word spelling and part of speach info 
  html_word_details <- 
    ifelse(
      length(div_details) == 0, "",
      div_details %>% 
        map_df(~ tag_tibble_to_clear(tags_to_tibble(.x), 
                                     word = found_word)) %>% 
        filter(word != "") %>% 
        group_by(name) %>% 
        filter(row_number() == 1L) %>% 
        ungroup() %>% 
        glue_data("<li>{data}</li>") %>% 
        paste0(collapse = "") %>% 
        glue('{pron_dron}<ol start="1">{html} </ol>', html = .)
    )
  
  # word meaning and examples (in an ugly way)
  div_meaning <- div_data[which(div_names == "ddef_h")]
  div_examples <- div_data[which(div_names == "def-body ddef_b")]
  
  # word meaning and examples (in a pretty way)
  text_meaning <- map_chr(div_meaning, tags_to_sentences)
  # some clearing up
  wrong_meaning <- str_starts(text_meaning, pattern = "past participle|past simple|present participle")
  text_meaning2 <- text_meaning[!wrong_meaning]
  text_examples <- map_chr(div_examples, tags_to_sentences) %>% 
    map_chr(~gsub("Thesaurus:.*","",.)) %>% 
    map_chr(~gsub("Synonym.*","",.)) %>% 
    map_chr(~gsub("Opposite.*","",.)) %>% 
    map_chr(~gsub("Compare.*","",.)) %>% 
    map_chr(~gsub("See.*","",.))
  
  # all possible word forms (!)
  possible_words <- 
    unique(c(
      word, found_word,
      str_replace_all(word, " ", "-"),
      str_replace_all(found_word, " ", "-"),
      word_forms_list$lv2_forms %>% 
        filter(success) %>% 
        pull(word)
    )) %>% 
    c(str_to_sentence(.), .)
  
  # Saving examples ----
  log_it("Checking word examples for test")
  already_have_cnt <- 
    dbGetQuery(
      conn,
      glue_sql(
        "
        SELECT count(*) as cnt 
        FROM word_cambridge_examples
        WHERE word_id = {word_id}
        "
      )
    ) %>% pull()
  
  if (fuzzy_match & already_have_cnt == 0L) {
    
    # clearing examples
    text_sentences <- text_examples %>%
      glue_collapse(sep = "<br>") %>%
      str_split("<br>") %>% 
      as_vector() %>% 
      as_tibble_col(column_name = "sentence") %>% 
      filter(sentence != "") %>% # empty string
      filter(str_detect(sentence, "^[[:upper:]]")) %>%  # only sentences
      pull()
    
    if (length(text_sentences) > 0) {
      
      # replacing words for ***
      clear_text_sentences <- 
        find_and_replace_words(text_sentences, possible_words, "***") %>% 
        filter(cnt == 1L) %>% 
        select(txt, wrd)
      
      if (nrow(clear_text_sentences) > 0) {
        
        log_it("Adding new examples in DB")
        tibble(word_id, clear_text_sentences) %>% 
          dbAppendTable(conn, "word_cambridge_examples", .)
      }
    }
  }
  
  # Finishing touches ----
  # highlighting extra information in text
  text_examples1 <- text_examples %>% 
    map(str_replace_all,
        pattern = "(\\[(.*?)\\])", # highlight text in brackets
        replacement = '<span style="color: #D0D0D0"><tt>\\0</tt></span>') %>% # <tt>\\0</tt>
    map(str_replace_all, 
        pattern = "(^|<br>)([a-z])(.*?)(?=[A-Z])", # highlight text before sentence
        replacement = '\\1<span style="color:	#D0D0D0"><tt>\\2\\3</tt></span>') # \\1<tt>\\2\\3</tt>
  
  # highlighting all word forms in example
  text_examples2 <- find_and_replace_words(text_examples1, possible_words)$txt
  
  # meaning and example data
  first_n <- min(length(text_meaning2), length(text_examples))
  add_names <- c("meaning", "examples")
  html_meaning_and_examples <- 
    list(text_meaning2[1:first_n], 
         text_examples2[1:first_n]) %>%
    set_names(add_names) %>%
    as_tibble() %>% 
    head(meaning_length) %>%
    glue_data("<h4>{meaning}</h4>{examples}", .na = "") %>% 
    paste(collapse = "") %>%
    glue(if_else(first_n > meaning_length, '<h4><a href="{link}">...{emoji}</a></h4>', ""), 
         emoji = emo::ji("print"),
         link = express_tbl$link)
  
  # word corpus
  email_words_list[[i]] <- 
    paste(
      html_word_head,
      html_word_forms,
      html_cambridge_head, 
      html_word_details,
      html_meaning_and_examples,
      "<br><br><br>"
    )
  
}


# Code-licious Labels ----
log_it("Labels cooking")

# parsing labels from email
labels_print <- email_words_list %>%
  map(str_extract_all, pattern = "\\[.*?\\]") %>%
  flatten() %>% reduce(c) %>% 
  unique()
# extra spaces and minutiae
labels_match <- labels_print %>% 
  map_chr(str_replace_all,
          pattern = "(\\[)[[:space:]]|[[:space:]](\\])",
          replacement = "\\1\\2") %>% 
  map_chr(str_remove_all, pattern = "\\(|\\)")
# labels from Cambridge site
labels_dict <- dbReadTable(conn, "labels_and_codes_dict") %>% as_tibble()

# emoji labels
emoji_labels_df <- tiltle_emoji_df %>% 
  mutate(descr = if_else(iter == 1, "new", paste0("x", iter))) %>% 
  select(label = emoji, descr)

# HTML labels block
html_labels <- labels_dict %>%
  filter(label %in% !!labels_match) %>%
  inner_join(tibble(label = labels_match, labels_print), by = "label") %>% 
  mutate_at(vars(description), str_to_lower) %>%
  mutate_at(vars(description), str_remove_all, pattern = "\\.") %>% 
  mutate_at(vars(description), str_replace_all, pattern = ":", replacement = ",") %>% 
  select(label = labels_print, description) %>% 
  nest(data = description) %>% 
  mutate(descr = map_chr(data, ~ glue_collapse(.$description, sep = ";"))) %>% 
  select(label, descr) %>% 
  add_row(emoji_labels_df) %>% 
  glue_data("<b>{label}</b>: {descr}") %>% 
  paste0(collapse = "<br>") %>% 
  glue('<small><h2>Labels \\& Codes</h2>{input}</small>', input = .)


# Test Me Maybe ----
log_it("Test initiation")
# random examples from BD
sql_query <-
  "
  SELECT word_id, word, txt, wrd
  FROM 
  (
    SELECT a1.*, a2.txt, a2.wrd
    , dense_rank() over (order by sin(a1.word_id + julianday('now'))) as dr
    , row_number() over (partition by a1.word_id order by random()) as rn
    FROM word_dict a1
    JOIN word_cambridge_examples a2 on a1.word_id = a2.word_id
    LEFT JOIN
    (
      SELECT DISTINCT word_id
      FROM word_history
      WHERE action = 'checking' and julianday('now') - julianday(action_dt) <= 7
      and word_id in (SELECT word_id FROM word_history where action = 'email')
    ) a3 on a1.word_id = a3.word_id
    WHERE a3.word_id is null
  )
  WHERE dr <= 3 and rn <= 4
  ORDER BY random()
  "
export_examples <- 
  dbGetQuery(conn, sql_query) %>% 
  as_tibble()

# preparing examples 
random_emoji = emo::ji("fruit")
clear_examples <- 
  export_examples %>% 
  mutate_at(
    vars(txt),
    str_replace_all,
    pattern = "\\*\\*\\*", 
    replacement = random_emoji
  ) %>% 
  mutate_at(
    vars(wrd),
    str_to_lower
  )
# selected words for test
export_words <- clear_examples %>% 
  distinct(word_id, word) %>% 
  arrange(runif(n()))

# cooking test HTML body
if (nrow(export_words) == 0L) {
  html_checking <- "" 
} else {
  
  log_it("Test body preparing")
  
  html_head <- paste(
    "<br><br><hr>",
    "<h2>", "TEST", "</h2>",
    "<h4>", "Replace",
    glue("\U00AB{emoji}\U00BB", emoji = random_emoji),
    "with", 
    export_words$word %>% 
      map_df(cambridge_express) %>% 
      glue_data('<b><a href="{link}">{word}</a></b>') %>% 
      glue_collapse(sep = ", ", last = " and "),
    "using correct forms. Check the correct answers below.",
    "</h4>"
  )
  
  html_examples <- clear_examples %>% 
    mutate_at(vars(txt), ~ str_replace_all(., "\\(([^()]+)\\)", "")) %>% 
    mutate_at(vars(txt), ~ str_replace_all(., "  ", " ")) %>% 
    mutate_at(vars(txt), str_squish) %>% 
    glue_data('<li>{txt}<br><br></li>') %>% 
    paste0(collapse = "") %>% 
    glue('<ol>{html}<b>\U2193</b>{ending} </ol>', 
         html = .,
         ending = paste0(rep("<br>", 20L), collapse = ""))
  
  html_answers <- clear_examples %>% 
    glue_data("<li>{wrd}</li>") %>% 
    paste0(collapse = "") %>% 
    glue('<ol start="1">{html} </ol>', html = .)
  
  html_checking <- paste(html_head, html_examples, html_answers)
}


# Whipping Email into Shape ----
# upper header
html_header <- 
  paste(
    selected_words %>% 
      glue_data("**{word}**") %>% 
      glue_collapse(sep = ", ", last = " and ")
  ) %>% 
  str_to_sentence()

# motivation in the end
html_praise <- paste0(
  "<br><br><br><h2>", sample(str_to_sentence(praise_parts$exclamation), 1), "! ", 
  praise::praise(), " ",
  emo::ji("face"), "</h2>")

# email composing
log_it("Email composing")
composed_email <- 
  compose_email(
    header = md(html_header),
    body = list(md(email_words_list), md(html_labels), md(html_checking), md(html_praise)),
    footer = md("Auto Dict v4.6<br>Created by Artem R.")
  )


# Inbox Injection ----
log_it("Sending result")
max_attempt = 5L # max attempts of email sending
sending_result <- F
attempt <- 0

# sending email even if gmail doesnt work
while ((!sending_result) & (attempt <= max_attempt)) {
  
  sending_result <- 
    tryCatch({
      composed_email %>% 
        smtp_send(
          from = c("Auto Dictionary" = email_secrect$username),
          to = email_secrect$to,
          bcc = email_secrect$bcc,
          subject = "Words of the day",
          credentials = creds_envvar(
            user = email_secrect$username,
            provider = "gmail"
          )
        )
      TRUE
    },
    error = function(cond) FALSE
    )
  
  attempt <- attempt + 1
  Sys.sleep(2)
  
}

# Log in result in DB
log_it("Adding transaction in DB")
if (sending_result) {
  
  selected_words %>% 
    select(word_id) %>% 
    mutate(
      action = "email",
      action_dt = format(Sys.Date(), "%Y-%m-%d")
    ) %>% 
    dbWriteTable(conn, "word_history", value = ., append = TRUE)
  
  if (nrow(export_words) > 0) {
    export_words %>% 
      select(word_id) %>% 
      mutate(
        action = "checking",
        action_dt = format(Sys.Date(), "%Y-%m-%d")
      ) %>% 
      dbWriteTable(conn, "word_history", value = ., append = TRUE)
  }
}

# Disconnecting DB 
dbDisconnect(conn)
log_it("End of task")
