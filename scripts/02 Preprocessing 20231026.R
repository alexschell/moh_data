library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/fatalities_20231026_raw.csv"
output_path = "data/fatalities_20231026_wip.csv"


# Read from CSV -------------------------------------------------------

df = read.csv(
  file = input_path, 
  colClasses = "character",
  na.strings = ""
)


# Cleaning  -----------------------------------------------------------

df$.id = as.integer(df$id)

df$.gov_id = df$gov_id
df$.flg_gov_id = case_when(
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.name = df$name
df$.flg_name = case_when(
  str_detect(df$name, "[^\u0600-\u06ff ]") ~ "special characters",
  TRUE ~ NA_character_
)

df$.sex = case_when(
  df$sex == "ذكر" ~ "M",
  df$sex == "انثى" ~ "F",
  TRUE ~ NA_character_
)

df$.age = case_when(
  "أقل من عام" == df$age ~ 0L,
  df$age == "1823" ~ NA_integer_,
  TRUE ~ as.integer(df$age)
)
df$.flg_age = case_when(
  df$age == "1823" ~ "invalid",
  TRUE ~ NA_character_
)

# Checks
all(!is.na(df$.id))
all(!is.na(df$.gov_id) | !is.na(df$.flg_gov_id))
all(!is.na(df$.name) | !is.na(df$.flg_name))
all(!is.na(df$.sex))
all(!is.na(df$.age) | !is.na(df$.flg_age))


# Output --------------------------------------------------------------

# Subset / rename / reorder columns
df = select(
  df,
  id = .id,
  gov_id = .gov_id,
  name = .name,
  sex = .sex,
  age = .age,
  flg_gov_id = .flg_gov_id,
  flg_name = .flg_name,
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
  colClasses = { x = rep("character", 8); x[c(1,5)] = "integer"; x },
  na.strings = ""
)
all.equal(df, tmp)
rm("tmp")
