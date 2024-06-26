library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

input_path = "data/source_files/fatalities_20240329.txt"
output_path = "data/fatalities_20240329_raw.csv"


# Read raw text -------------------------------------------------------

lines = readLines(input_path)

lines = str_squish(lines)
lines = str_replace_all(lines, "\"", "``")
lines = lines[lines != ""]
lines = lines[9:148190]


# Get starting index for each table row -------------------------------

# first column (id) is always populated, integer, in order
# last column (source) is always populated, matches "تبيلغ ذوي الشهداء|سجلات وزارة الصح"

last_seq_id = 1
line_starts = c(1)
eol_pattern = "\f|تبيلغ ذوي الشهداء|سجلات وزارة الصح"
for (i in (line_starts[1] + 1):length(lines)) {
  if (str_detect(lines[i], "^[0-9]+$")) {
    seq_id = as.numeric(lines[i])
    if (seq_id == last_seq_id + 1 && str_detect(lines[i-1], eol_pattern)) {
      line_starts[seq_id] = i 
      last_seq_id = seq_id
    }
  }
}


# Map row chunks to data.frame ----------------------------------------

# id - always populated, integer
# name - always populated; sometimes split over multiple rows of text
# gov_id - can be missing
# dob - sometimes missing; sometimes set to age
# sex - always populated, matches "ذكر|انثى|أنثى"
# age - sometimes missing
# source - always populated, matches "تبيلغ ذوي الشهداء|سجلات وزارة الصح"

rows = mapply(
  function(start, end) lines[start:end], 
  start = line_starts,
  end = c(line_starts[-1] - 1, length(lines))
)

map_to_ls = function(row) {
  out = list(
    id = NA_character_,
    name = NA_character_,
    gov_id = NA_character_,
    dob = NA_character_,
    sex = NA_character_,
    age = NA_character_,
    source = NA_character_,
    error = NA_character_
  )
  
  out$id = row[1]
  # out$name = row[2]
  idx_sex = head(str_which(row, "^(ذكر|انثى|أنثى)$"), 1)
  out$sex = row[idx_sex]
  idx_source = head(str_which(row, "تبيلغ ذوي الشهداء|سجلات وزارة الصح"), 1)
  out$source = row[idx_source]
  
  
  str_detect(vals, "تبيلغ ذوي الشهداء|سجلات وزارة الصح") ~ "source"
  str_detect(vals, "^(ذكر|انثى|أنثى)$") ~ "sex"
  
  # check if age is populated
  if (idx_source > idx_sex + 1) {
    out$age = row[idx_sex + 1]
  }
  
  if (idx_sex == 3) {
    # single-part name and gov_id missing and dob missing (unambiguous)
    
    out$name = row[2]
    
  } else {
    
    vals = row[2:idx_sex]
    labels = dplyr::case_when(
      str_detect(vals, "^(ذكر|انثى|أنثى)$") ~ "sex",
      # str_detect(vals, "تبيلغ ذوي الشهداء|سجلات وزارة الصح") ~ "source",
      str_detect(vals, "^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$") ~ "dob", 
      str_detect(vals, "^[0-9]{1,3}$") ~ "age",
      str_detect(vals, "^[0-9]{4,}$") ~ "gov_id",
      str_detect(vals, "[\u0600-\u06ff]") ~ "words",
      TRUE ~ "other"
    )
    pattern = paste0(labels, collapse = " ")
    
    if (str_detect(pattern, "^(words )+(gov_id )?(dob )?sex$")) {
      # valid pattern
      
      if ("gov_id" %in% labels) out$gov_id = vals[labels == "gov_id"][1]
      if ("dob" %in% labels) out$dob = vals[labels == "dob"][1]
      # if (any(labels %in% c("words", "other"))) out$address = paste0(vals[labels %in% c("words", "other")], collapse = " ")
      if (any(labels %in% c("words"))) out$name = paste0(rev(vals[labels %in% c("words")]), collapse = " ")
      
    } else if (str_detect(pattern, "^(words )+(gov_id )?([a-z_]+) sex$")) {
      # invalid dob but unambiguous
      
      labels[length(labels) - 1] = "dob"
      if ("gov_id" %in% labels) out$gov_id = vals[labels == "gov_id"][1]
      if ("dob" %in% labels) out$dob = vals[labels == "dob"][1]
      if (any(labels %in% c("words"))) out$name = paste0(rev(vals[labels %in% c("words")]), collapse = " ")

    } else if (str_detect(pattern, "^(words )+([a-z_]+) ([a-z_]+) sex$")) {
      # invalid dob but unambiguous
      
      labels[length(labels) - 1] = "dob"
      labels[length(labels) - 2] = "gov_id"
      if ("gov_id" %in% labels) out$gov_id = vals[labels == "gov_id"][1]
      if ("dob" %in% labels) out$dob = vals[labels == "dob"][1]
      if (any(labels %in% c("words"))) out$name = paste0(rev(vals[labels %in% c("words")]), collapse = " ")
      
    } else {
      
      out$error = pattern

    }
    
  }
  
  out
  
}

df = rows %>% lapply(map_to_ls) %>% lapply(data.frame) %>% bind_rows

which(!is.na(df$error))
# 13716
# 18847

i = 13716
rows[[i]] %>% cbind
df$name[i] = rows[[i]][2]
df$gov_id[i] = rows[[i]][3]
df$error[i] = NA_character_
str(df[i, ])

i = 18847
rows[[i]] %>% cbind
df$name[i] = rows[[i]][2]
df$gov_id[i] = rows[[i]][3]
df$dob[i] = rows[[i]][4]
df$error[i] = NA_character_
str(df[i, ])

sum(!is.na(df$error)) == 0
df$error = NULL


# Save as CSV ---------------------------------------------------------

write.csv(
  df,
  file = output_path,
  quote = FALSE,
  na = "",
  row.names = FALSE
)

# Check write/read
tmp = read.csv(
  file = output_path,
  colClasses = "character",
  na.strings = ""
)
all.equal(df, tmp)
rm("tmp")
