library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(xlsx)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/source_files/fatalities_20240105_src.xlsx"
output_path = "data/fatalities_20240105.csv"


# 1. Read XLSX --------------------------------------------------------

df = xlsx::read.xlsx(
  file = input_path, 
  sheetIndex = 1,
  colClasses = "character",
  keepFormulas = TRUE
)
names(df) = c("name", "gov_id", "dob", "sex", "age")
df$gov_id = as.character(df$gov_id)
df$age = NULL  # only formula; drop


# 2. Cleaning  --------------------------------------------------------

df$.id = seq(nrow(df))

df$.name = clean_name(df$name)
df$.flg_name = case_when(
  str_detect(df$name, "[^\u0600-\u06ff ]") ~ "special characters",
  TRUE ~ NA_character_
)

df$.gov_id = as.character(df$gov_id)
df$.flg_gov_id = case_when(
  str_length(df$.gov_id) < 9 ~ "partial",
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.dob = ymd("1899-12-30") + days(trunc(as.numeric(df$dob)))   # Excel numeric date format
df$.dob[df$.dob == ymd("1899-12-29")] = NA_Date_
df$.flg_dob = case_when(
  is.na(df$dob) ~ "missing",
  is.na(df$.dob) ~ "invalid",
  df$.dob == ymd("2000-01-01") ~ "2000-01-01",
  df$.dob == ymd("1999-01-01") ~ "1999-01-01",
  df$.dob == ymd("2000-02-02") ~ "2000-02-02",
  yday(df$.dob) == 1 ~ "YYYY-01-01",
  TRUE ~ NA_character_
)

df$.sex = case_when(
  df$sex == "ذكر" ~ "M",
  df$sex == "انثى" ~ "F",
  df$sex == "غير معرف" ~ NA_character_,
  TRUE ~ NA_character_
)
df$.flg_sex = case_when(
  is.na(df$.sex) ~ "missing",
  TRUE ~ NA_character_
)

# Checks
all(!is.na(df$.id))
all(!is.na(df$.name) | !is.na(df$.flg_name))
all(!is.na(df$.gov_id) | !is.na(df$.flg_gov_id))
all(!is.na(df$.dob) | !is.na(df$.flg_dob))
all(!is.na(df$.sex) | !is.na(df$.flg_sex))


# 3. Output -----------------------------------------------------------

# Subset / rename / reorder columns
df = select(
  df,
  id = .id,
  name = .name,
  gov_id = .gov_id,
  dob = .dob,
  sex = .sex,
  flg_name = .flg_name,
  flg_gov_id = .flg_gov_id,
  flg_dob = .flg_dob,
  flg_sex = .flg_sex
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
  colClasses = { x = rep("character", 9); x[1] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
all.equal(df, tmp)
