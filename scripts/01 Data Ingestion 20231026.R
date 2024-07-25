library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = "data/source_files/fatalities_20231026_src.txt"
output_path = "data/fatalities_20231026.csv"


# 1. Read dataset from text -------------------------------------------

lines = readLines(input_path)
lines = str_squish(lines)
lines = str_replace_all(lines, "\"", "``")
lines = lines[lines != ""]

df = data.frame(
  id     = lines[seq(from=191, by=5, to=33925)],
  gov_id = lines[seq(from=192, by=5, to=33925)],
  name   = lines[seq(from=193, by=5, to=33925)],
  sex    = lines[seq(from=194, by=5, to=33925)],
  age    = lines[seq(from=195, by=5, to=33925)]
)


# 1.1. Checks ---------------------------------------------------------

df$id %>% as.numeric %>% diff %>% `==`(1) %>% all

df$name %>% str_detect("^[\u0600-\u06ff ]+$") %>% all
df$name %>% str_detect("^[\u0600-\u06ff ]+$") %>% mean
df$name %>% str_detect("^[\u0600-\u06ff ]+$") %>% `!` %>% sum

df$gov_id %>% str_detect("^[0-9]{9}$") %>% all

df$dob %>% str_detect("^[0-9]{2}-[0-9]{2}-[0-9]{2}$") %>% all

df$sex %>% str_detect("^(انثى|ذكر)$") %>% all

df$age %>% str_detect("^[0-9]+$") %>% all
df$age %>% as.numeric %>% is.na %>% mean
table(df$age[!str_detect(df$age, "^[0-9]+$")])
hist(as.numeric(df$age)[as.numeric(df$age) < 200])


# 2. Cleaning  --------------------------------------------------------

df$.id = as.integer(df$id)

df$.gov_id = df$gov_id
df$.flg_gov_id = case_when(
  !check_luhn(df$gov_id) ~ "incorrect check digit",
  TRUE ~ NA_character_
)

df$.name = clean_name(df$name)
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


# 3. Output -----------------------------------------------------------

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
