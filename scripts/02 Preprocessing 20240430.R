library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

input_path = "data/fatalities_20240430_raw.csv"
output_path = "data/fatalities_20240430_wip.csv"


# Read from CSV -------------------------------------------------------

df = read.csv(
  file = input_path, 
  colClasses = "character",
  na.strings = ""
)


# Basic Data Cleaning -------------------------------------------------

df$.id =  as.integer(df$id)

df$.name = df$name
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


# Output --------------------------------------------------------------

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
rm("tmp")
