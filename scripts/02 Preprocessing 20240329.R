library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

options(stringsAsFactors = FALSE)

input_path = "data/fatalities_20240329_raw.csv"
output_path = "data/fatalities_20240329_wip.csv"


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
  TRUE ~ df$gov_id
)
df$.flg_gov_id = case_when(
  is.na(df$gov_id) ~ "missing",
  str_detect(df$gov_id, "^0+$") ~ "missing",
  str_detect(df$gov_id, "^_+$") ~ "missing",
  str_detect(df$gov_id, "[^0-9]") ~ "special characters",
  str_length(df$gov_id) > 9 ~ "10+ digits",
  str_length(df$gov_id) < 9 ~ "partial",
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


# Output --------------------------------------------------------------

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
rm("tmp")
