library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(stringdist)
library(arabicStemR)

options(stringsAsFactors = FALSE)

input_path = c(
  "data/fatalities_20240329_wip.csv",
  "data/fatalities_20240430_wip.csv"
)

output_path = "data/names_fix_20240329.csv"


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


# Join to get 20240430 names ------------------------------------------

df4_names = 
  df4 %>%
  filter(!is.na(gov_id) & !duplicated(gov_id)) %>%
  select(gov_id, name)

df_34 = 
  df3 %>%
  select(id, gov_id, name) %>%
  left_join(df4_names, by="gov_id") %>%
  mutate(dist = stringdist::stringdist(name.x, name.y) / pmax(nchar(name.x), nchar(name.y)))


# Get substitution patterns from OCR-affected names -------------------

# OCR-affected ~ has lone letters / matches ' [\u0600-\u06ff](?=( |$))'

extract_4gram = function(x) {
  last4 = str_extract(str_remove_all(x, " "), "[\\w]{3,4}$")
  pattern = paste0(str_replace_all(last4, "(?<=\\w)(?=\\w)", " ?"), "$")
  str_extract(x, pattern)
}

extract_4gram_mapped = function(x, y) {
  gram = extract_4gram(x)
  complement = str_remove(x, paste0(gram, "$"))
  str_remove(y, paste0("^", complement))
}

split_x = 
  df_34 %>%
  filter(!is.na(name.y), dist < 0.5) %>%
  `$`(name.x) %>%
  str_replace_all(" (?=[\u0600-\u06ff]( |$))", "_") %>%
  str_split(" ")
split_y = 
  df_34 %>%
  filter(!is.na(name.y), dist < 0.5) %>%
  `$`(name.y) %>%
  str_split(" ")
is_aligned = sapply(split_x, length) == sapply(split_y, length)
words_x = unlist(split_x[is_aligned])
words_y = unlist(split_y[is_aligned])

df_lookup = 
  data.frame(
    word_x = words_x,
    word_y = words_y
  ) %>%
  filter(str_detect(word_x, "_")) %>% 
  mutate(word_x = str_replace_all(word_x, "_", " ")) %>%
  filter(word_x != word_y) %>%
  mutate(
    x = extract_4gram(word_x),
    y = extract_4gram_mapped(word_x, word_y)
  ) %>%
  filter(!is.na(x), !is.na(y)) %>%
  group_by(x, y) %>%
  summarise(n_xy = n(), .groups = "drop") %>%
  group_by(x) %>%
  arrange(desc(n_xy)) %>%
  mutate(
    n_x = sum(n_xy),
    p_top = max(n_xy) / n_x,
    rn = row_number()
  ) %>%
  ungroup() %>%
  mutate(dist = stringdist::stringdist(x, y) / pmax(nchar(x), nchar(y))) %>%
  filter(p_top >= 0.7, n_x >= 2, rn == 1)

# Other whole-word substitutions
df_lookup2 = 
  data.frame(
    x = words_x,
    y = words_y
  ) %>%
  filter(!is.na(x), !is.na(y)) %>%
  filter(!str_detect(x, "_")) %>% 
  group_by(x, y) %>%
  summarise(n_xy = n(), .groups = "drop") %>%
  group_by(x) %>%
  arrange(desc(n_xy)) %>%
  mutate(
    n_x = sum(n_xy),
    p_top = max(n_xy) / n_x,
    rn = row_number()
  ) %>%
  ungroup() %>%
  mutate(dist = stringdist::stringdist(x, y) / pmax(nchar(x), nchar(y))) %>%
  filter(p_top >= 0.9, n_x >= 10, rn == 1) %>%
  filter(
    x != y,
    arabicStemR::fixAlifs(x) != arabicStemR::fixAlifs(y)
  )


# Apply substitutions where applicable --------------------------------

lookup_substitute = function(s, df, prefix = "", suffix = "") {
  for (i in seq(nrow(df))) {
    s = str_replace_all(s, paste0(prefix, df$x[i], suffix), df$y[i])
  }
  s
}

df_34$name.z = lookup_substitute(df_34$name.x, df_lookup, suffix = "(?=($| ))")
df_34$name.z = lookup_substitute(df_34$name.z, df_lookup2, prefix = "(?<=(^| ))", suffix = "(?=($| ))")

# Look at instances not fixed
df_34 %>%
  filter(is.na(name.y), str_detect(name.z, " [\u0600-\u06ff]( |$)")) %>%
  mutate(
    tname.x = arabicStemR::transliterate(name.x),
    tname.z = arabicStemR::transliterate(name.z)
  ) %>%
  select(id, tname.x, tname.z, name.x)

# Apply manual fixes
# idx = str_detect(df_34$name.z, " [\u0600-\u06ff]( |$)")
# df_34$name.z[idx] = {
#   x = df_34$name.x[idx]
#   x = str_replace_all(x, "ياعى ن ي", "ياغي")
#   x = str_replace_all(x, "العي ن", "العين")
#   x = str_replace_all(x, "الكيى ي", "الكتري")
#   x = str_replace_all(x, "أمي ه", "أميره")
#   x = str_replace_all(x, "حسير ن", "حسبن")
#   x = str_replace_all(x, "المعثن ي", "المعني")
#   x = str_replace_all(x, "شعي ة", "شعيرة")
#   x = str_replace_all(x, "العي ن", "العين")
#   x = str_replace_all(x, "مي ا", "ميرا")
#   x = str_replace_all(x, "ملعن ي", "ملغي")
#   x
# }

df_lookup3 = data.frame(
  x = c("ياعى ن ي", "العي ن", "الكيى ي", "أمي ه", "حسير ن", "المعثن ي", "شعي ة", "العي ن", "مي ا", "ملعن ي"),
  y = c("ياغي", "العين", "الكتري", "أميره", "حسبن", "المعني", "شعيرة", "العين", "ميرا", "ملغي")
)

idx = str_detect(df_34$name.z, " [\u0600-\u06ff]( |$)")
df_34$name.z[idx] = lookup_substitute(df_34$name.x[idx], df_lookup3)


# Also fix some lone bigrams: ' [\u0600-\u06ff]{2}(?=( |$))'

df_34 %>%
  filter(is.na(name.y), str_detect(name.x, " [\u0600-\u06ff]{2}( |$)")) %>%
  mutate(
    tname.x = arabicStemR::transliterate(name.x),
    tname.z = arabicStemR::transliterate(name.z)
  ) %>%
  select(id, tname.x, tname.z, name.x)

# idx = str_detect(df_34$name.z, " [\u0600-\u06ff]{2}( |$)")
# df_34$name.z[idx] = {
#   x = df_34$name.z[idx]
#   x = str_replace_all(x, "الير يم", "البريم")
#   x = str_replace_all(x, "النعي ني", "النعيزي")
#   x = str_replace_all(x, "سرى ي", "شري")
#   x
# }

df_lookup4 = data.frame(
  x = c("الير يم", "النعي ني", "سرى ي"),
  y = c("البريم", "النعيزي", "شري")
)

idx = str_detect(df_34$name.z, paste0(df_lookup4$x, collapse="|"))
df_34$name.z[idx] = lookup_substitute(df_34$name.z[idx], df_lookup4)

# Checks

df_34 = 
  df_34 %>% 
  mutate(
    dist_xy = stringdist::stringdist(name.x, name.y) / pmax(nchar(name.x), nchar(name.y)),
    dist_yz = stringdist::stringdist(name.y, name.z) / pmax(nchar(name.y), nchar(name.z)),
    dist_xz = stringdist::stringdist(name.x, name.z) / pmax(nchar(name.x), nchar(name.z))
  )

df_34 %>%
  filter(!is.na(name.y)) %>%
  group_by(name.x == name.y) %>%
  summarise(
    pct_better = mean(dist_yz < dist_xy), 
    pct_same = mean(dist_yz == dist_xy), 
    pct_worse = mean(dist_yz > dist_xy), 
    pct_fixed = mean(dist_yz == 0 & dist_xy > 0), 
    n = n()
  )

# df_34 %>% filter(dist_yz > dist_xy) %>%
#   mutate(
#     tname.x = arabicStemR::transliterate(name.x),
#     tname.y = arabicStemR::transliterate(name.y),
#     tname.z = arabicStemR::transliterate(name.z)
#   ) %>%
#   select(id, tname.x, tname.y, tname.z, name.x) %>% View


# Output --------------------------------------------------------------

out = 
  df_34 %>% 
  filter(name.x != name.z) %>%
  select(id, name.x, name.z)

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
  colClasses = { x = rep("character", 3); x[1] = "integer"; x },
  na.strings = ""
)
all.equal(out, tmp)
rm("tmp")
