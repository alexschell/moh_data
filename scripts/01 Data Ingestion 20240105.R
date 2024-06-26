library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(xlsx)

options(stringsAsFactors = FALSE)

input_path = "data/source_files/fatalities_20240105.xlsx"
output_path = "data/fatalities_20240105_raw.csv"


# Read XLSX -----------------------------------------------------------

df = xlsx::read.xlsx(
  file = input_path, 
  sheetIndex = 1,
  colClasses = "character",
  keepFormulas = TRUE
)
names(df) = c("name", "gov_id", "dob", "sex", "age")
df$gov_id = as.character(df$gov_id)
df$age = NULL  # only formula; drop


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
