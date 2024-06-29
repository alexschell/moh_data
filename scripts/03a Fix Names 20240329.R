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

output_path = ""


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

df_34_part2$name.y = {
  x = df_34_part2$name.x
  for (i in seq(nrow(df_lookup))) {
    x = str_replace_all(x, df_lookup$x[i], df_lookup$y[i])
  }
  x
}

df_34_part2 %>% 
  mutate(name.y==name.x) %>% 
  mutate(tname.x = transliterate(name.x),
         tname.y = transliterate(name.y)) %>%
  View

df_34_part2$name.y = str_replace_all(df_34_part2$name.y, "ياعى ن ي", "ياغي")

bind_rows(df_34_part1, df_34_part2) %>%
  mutate(
    tname.x = transliterate(name.x),
    tname.y = transliterate(name.y)
  ) %>%
  mutate(dist = stringdist::stringdist(tname.x, tname.y) / pmax(nchar(tname.x), nchar(tname.y)))

