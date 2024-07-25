library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/source_files/fatalities_20240329_src.txt"
output_path = "data/fatalities_20240329.csv"


# 1. Read dataset from text -------------------------------------------

lines = readLines(input_path)

lines = str_squish(lines)
lines = str_replace_all(lines, "\"", "``")
lines = lines[lines != ""]
lines = lines[9:148190]


# 1.1. Get starting index for each table row --------------------------

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


# 1.2. Map row chunks to data.frame -----------------------------------

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


# 1.3. Check & manual fixes -------------------------------------------

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
  TRUE ~ df$gov_id
)
df$.flg_gov_id = case_when(
  is.na(df$gov_id) ~ "missing",
  str_detect(df$gov_id, "^0+$") ~ "missing",
  str_detect(df$gov_id, "^_+$") ~ "missing",
  str_detect(df$gov_id, "[^0-9]") ~ "special characters",
  str_length(df$gov_id) > 9 ~ "10+ digits",
  str_length(df$gov_id) < 9 ~ "partial",
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.dob = case_when(
  str_detect(df$dob, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$") ~ dmy(df$dob),
  str_detect(df$dob, "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$") ~ mdy(df$dob),
  str_detect(df$dob, "^[0-9]{2}-[0-9]{2}-[0-9]{2}$") ~ case_when(
    str_extract(df$dob, "[0-9]{2}$") >= "25" ~ dmy(str_replace(df$dob, "([0-9]{2})$", "19\\1")),
    TRUE ~ dmy(str_replace(df$dob, "([0-9]{2})$", "20\\1"))
  ),
  str_detect(df$dob, "^(19|20)[0-9]{2}$") & df$dob <= "2024" ~ ymd(paste0(df$dob, "-01-01")),
  TRUE ~ as_date(NA)
) %>% suppressWarnings
df$.flg_dob = case_when(
  is.na(df$dob) ~ "missing",
  str_detect(df$dob, "^(19|20)[0-9]{2}$") & df$dob <= "2024" ~ "year of birth",
  df$dob == df$age ~ "invalid - age",
  is.na(df$.dob) ~ "invalid - other",
  TRUE ~ NA_character_
)

df$.flg_dob[yday(df$.dob) == 1] = "YYYY-01-01"
df$.flg_dob[yday(df$.dob) == 33] = "YYYY-02-02"

i = df$.dob < ymd(19000101)
df$.dob[i] = as_date(NA)
df$.flg_dob[i] = "invalid - year < 1900"

df$.sex = case_when(
  str_detect(df$sex, "^ذكر$") ~ "M",
  str_detect(df$sex, "^(أنثى|انثى)$") ~ "F",
  TRUE ~ NA_character_
)

df$.age = case_when(
  is.na(df$age) ~ NA_integer_,
  is.na(as.integer(df$age)) ~ NA_integer_,
  as.numeric(df$age) %in% 1900:2024 ~ NA_integer_,
  as.numeric(df$age) %% 1 != 0 ~ as.integer(df$age),
  TRUE ~ as.integer(df$age),
) %>% suppressWarnings
df$.flg_age = case_when(
  is.na(df$age) == "" ~ "missing",
  str_detect(df$age, "^#+$") ~ "missing",
  as.numeric(df$age) %in% 1900:2024 ~ "invalid - YOB",
  str_detect(df$age, "^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$") ~ "invalid - DOB",
  is.na(as.integer(df$age)) ~ "invalid - other",
  as.numeric(df$age) %% 1 != 0 ~ "truncated",
  TRUE ~ NA_character_
) %>% suppressWarnings

which(str_detect(df$age, "[^#0-9/.-]"))
# 19772
# 19805
# 20487

i = 19772
df$age[i]  # ٣٢.١٢.١٩٩٠ - 32/12/1990
df$.age[i] = NA_integer_
df$.flg_age[i] = "missing"

i = 19805
df$age[i]  # سنتان - two years
df$.age[i] = 2L
df$.flg_age[i] = "manual fix"

i = 20487
df$age[i]  # سنة 46 - year 46
df$.age[i] = 46L
df$.flg_age[i] = "manual fix"

df$.source = case_when(
  df$source == "سجلات وزارة الصحة" ~ "Ministry of Health",
  df$source == "تبيلغ ذوي الشهداء" ~ "Martyr families",
  TRUE ~ NA_character_
)


# Checks
all(!is.na(df$.id))
all(!is.na(df$.name) | !is.na(df$.flg_name))
all(!is.na(df$.gov_id) | !is.na(df$.flg_gov_id))
all(!is.na(df$.dob) | !is.na(df$.flg_dob))
all(!is.na(df$.sex))
all(!is.na(df$.age) | !is.na(df$.flg_age))
all(!is.na(df$.source))


# 3. Output -----------------------------------------------------------

# Subset / rename / reorder columns
df = select(
  df,
  id = .id,
  name = .name,
  gov_id = .gov_id,
  dob = .dob,
  sex = .sex,
  age = .age,
  source = .source,
  flg_name = .flg_name,
  flg_gov_id = .flg_gov_id,
  flg_dob = .flg_dob,
  flg_age = .flg_age
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
  colClasses = { x = rep("character", 11); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
all.equal(df, tmp)
