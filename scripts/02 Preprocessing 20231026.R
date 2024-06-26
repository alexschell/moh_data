library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

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
df$.name = df$name
df$.sex = case_when(
  df$sex == "ذكر" ~ "M",
  df$sex == "انثى" ~ "F",
  TRUE ~ NA_character_
)
df$.age = case_when(
  "أقل من عام" == df$age ~ 0L,
  TRUE ~ as.integer(df$age)
)

# Checks
all(!is.na(df$.id))
all(!is.na(df$.gov_id))
all(!is.na(df$.name))
all(!is.na(df$.sex))
all(!is.na(df$.age))


# Output --------------------------------------------------------------

# Subset / rename / reorder columns
df = select(
  df,
  id = .id,
  gov_id = .gov_id,
  name = .name,
  sex = .sex,
  age = .age
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
  colClasses = { x = rep("character", 5); x[c(1,5)] = "integer"; x },
  na.strings = ""
)
all.equal(df, tmp)
rm("tmp")
