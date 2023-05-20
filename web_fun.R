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
