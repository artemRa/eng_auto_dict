-- !preview conn=DBI::dbConnect(RSQLite::SQLite(), dbname = "my_english_dict.db")

-- main dict table
CREATE TABLE word_dict 
(
  word_id INTEGER,
  word TEXT NOT NULL,
  translation TEXT NOT NULL,
  source TEXT,
  PRIMARY KEY (word_id)
)
;

-- transactions table
CREATE TABLE word_history 
(
  word_id INTEGER,
  action TEXT NOT NULL,
  action_dt DATE NOT NULL
)
;

-- examples of word usage
CREATE TABLE word_cambridge_examples 
(
  word_id INTEGER,
  txt TEXT,
  wrd TEXT
)
;
  
-- excluded words
CREATE TABLE word_black_list 
(
  word TEXT
)
;