library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/source_files/fatalities_20240430_src.txt"
output_path = "data/fatalities_20240430.csv"


# 1. Read dataset from text -------------------------------------------

lines = readLines(input_path)

lines = str_squish(lines)
lines = str_replace_all(lines, "\"", "``")
lines = lines[lines != ""]
lines = lines[8:132390]


# 1.1. Get starting index for each table row --------------------------

# first column (id) is always populated, integer, in order
# last column (sex) is always populated, matches "^(أنثى|أنثي|انثى|انثي|دكر|ذكر)$"

last_seq_id = 1
line_starts = c(1)
eol_pattern = "^(\f|أنثى|أنثي|انثى|انثي|دكر|ذكر)$"
for (i in (line_starts[1] + 1):length(lines)) {
  if (str_detect(lines[i], "^[0-9]+$")) {
    seq_id = as.numeric(lines[i])
    if (seq_id == last_seq_id + 1 && str_detect(lines[i-1], eol_pattern)) {
      line_starts[seq_id] = i 
      last_seq_id = seq_id
    }
  }
}


# 1.2. Map row chunks to data.frame -----------------------------------

# id - always populated, integer
# name - always populated
# gov_id - can be missing
# age - sometimes missing
# address - sometimes missing; may be split into multiple lines
# sex - always populated, matches "^(أنثى|أنثي|انثى|انثي|دكر|ذكر)$"

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
    age = NA_character_,
    address = NA_character_,
    sex = NA_character_,
    error = NA_character_
  )
  
  out$id = row[1]
  out$name = row[2]
  idx_sex = head(str_which(row, "^(أنثى|أنثي|انثى|انثي|دكر|ذكر)$"), 1)
  out$sex = row[idx_sex]
  
  if (idx_sex > 3) {
    
    vals = row[3:idx_sex]
    labels = dplyr::case_when(
      str_detect(vals, "^[0-9]{1,3}$") ~ "age",
      str_detect(vals, "^[0-9]{4,}$") ~ "gov_id",
      str_detect(vals, "^(أنثى|أنثي|انثى|انثي|دكر|ذكر)$") ~ "sex",
      str_detect(vals, "[\u0600-\u06ff]") ~ "words",
      TRUE ~ "other"
    ) 
    pattern = paste0(labels, collapse = " ")
    
    if (str_detect(pattern, "^(gov_id )?(age )?(words )*(other )?sex$")) {
      # valid pattern
      
      if ("gov_id" %in% labels) out$gov_id = vals[labels == "gov_id"][1]
      if ("age" %in% labels) out$age = vals[labels == "age"][1]
      if (any(labels %in% c("words", "other"))) out$address = paste0(vals[labels %in% c("words", "other")], collapse = " ")
      
    } else if (str_detect(pattern, "^(words|age|other) (age )?(words )*(other )?sex$")) {
      # invalid gov_id but unambiguous
      
      labels[1] = "gov_id"
      if ("gov_id" %in% labels) out$gov_id = vals[labels == "gov_id"][1]
      if ("age" %in% labels) out$age = vals[labels == "age"][1]
      if (any(labels %in% c("words", "other"))) out$address = paste0(vals[labels %in% c("words", "other")], collapse = " ")
      
    } else {
      
      out$error = paste0(pattern, ": ", paste0(vals, collapse = "|"))
      
    }
    
  }
  
  out
  
}

df = rows %>% lapply(map_to_ls) %>% lapply(data.frame) %>% bind_rows

# Check 
sum(!is.na(df$error)) == 0
df$error = NULL


# 2. Cleaning ---------------------------------------------------------

df$.id =  as.integer(df$id)

df$.name = clean_name(df$name)
df$.flg_name = case_when(
  is.na(df$name) ~ "missing",
  str_detect(df$name, "[A-Za-z0-9]") ~ "alphanumeric characters",
  str_detect(df$name, "[^\u0600-\u06ff ]") ~ "special characters",
  TRUE ~ NA_character_
)

df$.gov_id = case_when(
  str_detect(df$gov_id, "^[0-]+$") ~ NA_character_,
  str_detect(df$gov_id, "^_+$") ~ NA_character_,
  str_detect(df$gov_id, "^[\u0600-\u06ff. ]+$") ~ NA_character_,
  TRUE ~ df$gov_id
)
df$.flg_gov_id = case_when(
  is.na(df$gov_id) ~ "missing",
  str_detect(df$gov_id, "^[0-]+$") ~ "missing",
  str_detect(df$gov_id, "^_+$") ~ "missing",
  str_detect(df$gov_id, "^[\u0600-\u06ff. ]+$") ~ "missing (comment)",
  str_detect(df$gov_id, "[^0-9]") ~ "special characters",
  str_length(df$gov_id) > 9 ~ "10+ digits",
  str_length(df$gov_id) < 9 ~ "partial",
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.age = as.integer(df$age)
df$.flg_age = case_when(
  is.na(df$age) ~ "missing",
  TRUE ~ NA_character_
)

df$.address = case_when(
  str_detect(df$address, "^[-=]+$") ~ NA_character_,
  TRUE ~ df$address
)
df$.flg_address = case_when(
  is.na(df$address) ~ "missing",
  str_detect(df$address, "^[-=]+$") ~ "missing",
  TRUE ~ NA_character_
)

df$.sex = case_when(
  df$sex %in% c("ذكر", "دكر") ~ "M",
  df$sex %in% c("انثى", "انثي", "أنثي", "أنثى") ~ "F",
  TRUE ~ NA_character_
)


# Checks
all(!is.na(df$.id))
all(!is.na(df$.name) | !is.na(df$.flg_name))
all(!is.na(df$.gov_id) | !is.na(df$.flg_gov_id))
all(!is.na(df$.age) | !is.na(df$.flg_age))
all(!is.na(df$.address) | !is.na(df$.flg_address))
all(!is.na(df$.sex))


# 3. Output -----------------------------------------------------------

# Subset / rename / reorder columns
df = select(
  df,
  id = .id,
  name = .name,
  gov_id = .gov_id,
  age = .age,
  address = .address,
  sex = .sex,
  flg_name = .flg_name,
  flg_gov_id = .flg_gov_id,
  flg_age = .flg_age,
  flg_address = .flg_address
)

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
  colClasses = { x = rep("character", 10); x[c(1,4)] = "integer"; x },
  na.strings = ""
)
all.equal(df, tmp)
