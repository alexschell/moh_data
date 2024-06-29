library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(arabicStemR)

options(stringsAsFactors = FALSE)

input_path = c(
  "data/fatalities_20240329_wip.csv",
  "data/fatalities_20240430_wip.csv"
)

output_path = "data/temp/names_20240329.R"


# Read from CSV -------------------------------------------------------

df3 = read.csv(
  file = input_path[1], 
  colClasses = { x = rep("character", 11); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)

df4 = read.csv(
  file = input_path[2], 
  colClasses = { x = rep("character", 10); x[c(1,4)] = "integer"; x },
  na.strings = ""
)


# Copy paste from DF4 -------------------------------------------------

df4_names = 
  df4 %>%
  filter(!is.na(gov_id) & !duplicated(gov_id)) %>%
  select(gov_id, name)

df_34 = 
  df3 %>%
  select(id, gov_id, name) %>%
  left_join(df4_names, by="gov_id") %>%
  mutate(
    flg = str_detect(name.x, " [\u0600-\u06ff]( |$)"),
    dist = stringdist::stringdist(name.x, name.y) / pmax(nchar(name.x), nchar(name.y))
  ) 

# df_34 %>% group_by(flg) %>% summarise(mean(is.na(name.y)), n())

split_x = df_34$name.x %>% str_replace_all(" (?=[\u0600-\u06ff]( |$))", "_") %>% str_split(" ")
split_y = df_34$name.y %>% str_split(" ")
is_aligned = sapply(split_x, length) == sapply(split_y, length)
words_x = unlist(split_x[is_aligned])
words_y = unlist(split_y[is_aligned])

is_compound = str_detect(words_x, "_")
words_x = words_x[is_compound]
words_y = words_y[is_compound]

df_lookup = 
  data.frame(
    x = unique(words_x),
    y = sapply(unique(words_x), function(k) names(tail(sort(table(words_y[words_x == k])), 1)))
  ) %>%
  mutate(x = str_replace_all(x, "_", " ")) %>%
  filter(x != y) 

df_34_part1 = 
  df_34 %>%
  filter(!is.na(name.y) | !flg, dist <= 0.25)

df_34_part2 = 
  df_34 %>% 
  filter((is.na(name.y) & flg) | dist > 0.25)

df_34_part2$name.z = {
  x = df_34_part2$name.x
  for (i in seq(nrow(df_lookup))) {
    x = str_replace_all(x, df_lookup$x[i], df_lookup$y[i])
  }
  x
}

# df_34_part2 %>% 
#   mutate(name.y==name.x) %>% 
#   mutate(tname.x = transliterate(name.x),
#          tname.z = transliterate(name.z)) %>%
#   View

df_34_part2$name.z = str_replace_all(df_34_part2$name.y, "ياعى ن ي", "ياغي")

out = 
  bind_rows(df_34_part1, df_34_part2) %>%
  mutate(
    tname.x = transliterate(name.x),
    tname.y = transliterate(name.y),
    tname.z = transliterate(name.z)
  ) %>%
  mutate(
    dist_xy = stringdist::stringdist(tname.x, tname.y) / pmax(nchar(tname.x), nchar(tname.y)),
    dist_xz = stringdist::stringdist(tname.x, tname.z) / pmax(nchar(tname.x), nchar(tname.z)),
    flg = as.integer(flg)
  )


# Output --------------------------------------------------------------

out = out %>% select(id, gov_id, name.x, name.y, name.z, flg, dist_xy, dist_xz)

write.csv(
  out,
  file = output_path,
  quote = FALSE,
  na = "",
  row.names = FALSE
)

# Check write/read
tmp = read.csv(
  file = output_path, 
  colClasses = { x = rep("character", 8); x[c(1,6)] = "integer"; x[7:8] = "numeric"; x },
  na.strings = ""
)
all.equal(out, tmp)
rm("tmp")

