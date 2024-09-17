library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/source_files/fatalities_20240831_src.txt"
output_path = "data/fatalities_20240831.csv"


# 1. Read dataset from text -------------------------------------------

lines = readLines(input_path)
lines = str_squish(lines)
lines = str_replace_all(lines, "\"", "``")
lines = lines[lines != ""]

df = data.frame(
  id     = lines[seq(from= 9, by=7, to=length(lines))],
  name   = lines[seq(from=10, by=7, to=length(lines))],
  gov_id = lines[seq(from=11, by=7, to=length(lines))],
  dob    = lines[seq(from=12, by=7, to=length(lines))],
  sex    = lines[seq(from=13, by=7, to=length(lines))],
  age    = lines[seq(from=14, by=7, to=length(lines))],
  source = lines[seq(from=15, by=7, to=length(lines))]
)


# 1.1. Checks ---------------------------------------------------------

df$id %>% as.numeric %>% diff %>% `==`(1) %>% all

df$name %>% str_detect("[\u0600-\u06ff]+") %>% all
df$name %>% str_detect("^[\u0600-\u06ff ]+$") %>% mean
df$name %>% str_detect("^[\u0600-\u06ff ]+$") %>% `!` %>% sum

df$gov_id %>% str_detect("^[0-9]{9}$") %>% all
df$gov_id %>% str_detect("^[0-9]{9}$") %>% mean
df$gov_id %>% str_detect("^[0-9]{9}$") %>% `!` %>% sum

df$dob %>% str_detect("^[0-9]{2}-[0-9]{2}-[0-9]{2}$") %>% all

df$sex %>% arabicStemR::fixAlifs() %>% str_detect("^(انثى|انثي|ذكر)$") %>% all

df$age %>% str_detect("^[0-9]+$") %>% all

df$source %>% str_detect("^(بيت - لجنة|تبليغ ذوي الشهداء|سجلات وزارة الصحة|معتمد بقرار لجنة قضائية)$") %>% all


# 2. Cleaning  --------------------------------------------------------

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
  str_detect(df$gov_id, "^طفل رضيع[ 0-9]*$") ~ NA_character_,
  TRUE ~ df$gov_id
)
df$.flg_gov_id = case_when(
  is.na(df$gov_id) ~ "missing",
  str_detect(df$gov_id, "^[0-]+$") ~ "missing",
  str_detect(df$gov_id, "^_+$") ~ "missing",
  str_detect(df$gov_id, "^طفل رضيع[ 0-9]*$") ~ "missing (comment)",
  str_detect(df$gov_id, "^[\u0600-\u06ff. ]+$") ~ "missing (comment)",
  str_detect(df$gov_id, "[^0-9]") ~ "special characters",
  str_length(df$gov_id) > 9 ~ "10+ digits",
  str_length(df$gov_id) < 9 ~ "partial",
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.sex = case_when(
  df$sex == "ذكر" ~ "M",
  df$sex %in% c("أنثى", "انثى", "انثي") ~ "F",
  TRUE ~ NA_character_
)

df$.age = as.integer(df$age)

df$.dob = dmy(df$dob)
df$.dob = case_when(
  year(df$.dob) > 2020 & df$.age > 20 ~ df$.dob - years(100),
  TRUE ~ df$.dob
)
df$.flg_dob = case_when(
  yday(df$.dob) == 1 ~ "YYYY-01-01",
  yday(df$.dob) == 33 ~ "YYYY-02-02",
  TRUE ~ NA_character_
)

df$.source = case_when(
  df$source == "سجلات وزارة الصحة" ~ "Ministry of Health",
  df$source == "معتمد بقرار لجنة قضائية" ~ "Approved by judicial committee",
  df$source == "تبليغ ذوي الشهداء" ~ "Martyr families",
  df$source == "بيت - لجنة" ~ "House - committee",
  TRUE ~ NA_character_
)


# Checks
all(!is.na(df$.id))
all(!is.na(df$.gov_id) | !is.na(df$.flg_gov_id))
all(!is.na(df$.name) | !is.na(df$.flg_name))
all(!is.na(df$.sex))
all(!is.na(df$.age))
all(!is.na(df$.dob) | !is.na(df$.flg_dob))
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
  flg_gov_id = .flg_gov_id,
  flg_name = .flg_name,
  flg_dob = .flg_dob
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
  colClasses = { x = rep("character", 10); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
all.equal(df, tmp)
