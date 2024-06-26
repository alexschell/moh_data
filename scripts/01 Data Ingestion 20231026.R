library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

input_path = "data/source_files/fatalities_20231026.txt"
output_path = "data/fatalities_20231026_raw.csv"


# Read & map to DF ----------------------------------------------------

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

# df$id %>% as.numeric %>% diff %>% `==`(1) %>% all
# df$gov_id %>% str_detect("^[0-9]{9}$") %>% all
# df$name %>% str_detect("[\u0600-\u06ff]+") %>% all
# df$sex %>% str_detect("^(انثى|ذكر)$") %>% all
# 
# df$age %>% as.numeric %>% is.na %>% sum
# table(df$age[!str_detect(df$age, "^[0-9]+$")])
# hist(as.numeric(df$age)[as.numeric(df$age) < 200])


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
