library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(arabicStemR)

library(ggplot2)

options(stringsAsFactors = FALSE)

source("scripts/utils.R")

input_path = c(
  "data/fatalities_20231026_wip.csv",
  "data/fatalities_20240105_wip.csv",
  "data/fatalities_20240329_wip.csv",
  "data/fatalities_20240430_wip.csv",
  "data/fatalities_20240630_wip.csv"
)
output_path = c(
  "data/fatalities_20231026_wip_2.csv",
  "data/fatalities_20240105_wip_2.csv",
  "data/fatalities_20240329_wip_2.csv",
  "data/fatalities_20240430_wip_2.csv",
  "data/fatalities_20240630_wip_2.csv"
)

# Read from CSV -------------------------------------------------------

# df = lapply(input_path, read.csv, na.strings = "")

df2 = read.csv(
  file = input_path[2],
  colClasses = { x = rep("character", 9); x[1] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
df3 = read.csv(
  file = input_path[3],
  colClasses = { x = rep("character", 11); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
df4 = read.csv(
  file = input_path[4],
  colClasses = { x = rep("character", 10); x[c(1,4)] = "integer"; x },
  na.strings = ""
)
df5 = read.csv(
  file = input_path[5],
  colClasses = { x = rep("character", 10); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)


# Fix Name Errors -----------------------------------------------------

# Deterministic fixes

fixes = function(x) {
  x %>%
    str_replace(" ?\\) ?([\u0600-\u06ff ]+?[\u0600-\u06ff]+) ?\\($", " \\(\\1\\)") %>%
    str_replace("^\\( ?([\u0600-\u06ff ]+?[\u0600-\u06ff]+) ?\\) ?(.*)$", "\\2 \\(\\1\\)") %>%
    str_replace("^0 ([\u0600-\u06ff ]+) ([\u0600-\u06ff]{3,}(?: [\u0600-\u06ff]{1,2})*) 0$", "\\2 0 0 \\1") %>% 
    str_replace("^00 ([\u0600-\u06ff ]+) ([\u0600-\u06ff]{3,}(?: [\u0600-\u06ff]{1,2})*) 00$", "\\1 00 00 \\2")
}

df3$.name = fixes(df3$name)
df4$.name = fixes(df4$name)
df4$.name = str_replace(df4$.name, "^([\u0600-\u06ff ]+) 00 00 ([\u0600-\u06ff ]+)$", "\\2 00 00 \\1")


# Based on distance to overlapping names to 20240105, 20240430 datasets

fix_order = function(x) {
  x %>% 
    str_replace_all("^([^0]+) ([0 ]+) ([^0]+)$", "\\3 \\2 \\1") %>%
    str_replace_all("^([\u0600-\u06ff ]+) ?([/*\\.]) ?([\u0600-\u06ff ]+)$", "\\3 \\2 \\1")
}

# Try flipping name order when name partially missing ---------

dftmp = 
  df3 %>%
  select(id, gov_id, name3 = .name) %>%
  filter(!is.na(gov_id)) %>%
  left_join(df2 %>% select(id2 = id, gov_id, name2 = name), by="gov_id", na_matches = "never") %>%
  left_join(df4 %>% select(id4 = id, gov_id, name4 = .name), by="gov_id", na_matches = "never") %>%
  filter(str_detect(name3, "0") | str_detect(name3, "^([\u0600-\u06ff ]+) ?([/*\\.]) ?([\u0600-\u06ff ]+)$")) %>%
  filter(!duplicates(gov_id)) %>% 
  mutate(
    dist0 = dstring(coalesce(name2, name4), name3),
    name3_flipped = fix_order(name3),
    dist1 = dstring(coalesce(name2, name4), name3_flipped)
  )

dftmp %>% ggplot(aes(x=dist0, y=dist1)) + geom_point(alpha=0.2)

dftmp = dftmp %>% filter(dist0 > 0.3, dist1 < (0.5 * dist0))

df3 =
  df3 %>% 
  left_join(dftmp %>% select(gov_id, name3_flipped), by = "gov_id", na_matches = "never") %>%
  mutate(.name = coalesce(name3_flipped, .name)) %>%
  select(-name3_flipped)


# Substitute systematic patterns

chunk = function(x) {
  exceptions = c("طه", "بن", "عز", "مي", "بر", "عس", "جد", "مى")
  # pattern = paste0('(?<=\\w{3,20}) (?=(?!', paste0(exceptions, collapse = "|"), ')[\u0600-\u06ff]{1,2}\\b)')
  pattern = paste0('(?<!0) (?=(?!', paste0(exceptions, collapse = "|"), ')[\u0600-\u06ff]{1,2}\\b)')
  str_replace_all(x, pattern, "_")
}

fn0 = function(x, y, f_x = function(x, y) x, f_y = function(x, y) y, ...) {
  if (length(x) != length(y)) stop()
  dist = dstring(x, y)
  x = str_split(chunk(x[dist < 0.4]), " ")
  y = str_split(chunk(y[dist < 0.4]), " ")
  aligned = sapply(x, length) == sapply(y, length)
  x = str_replace_all(unlist(x[aligned]), "_", " ")
  y = str_replace_all(unlist(y[aligned]), "_", " ")
  df = 
    data.frame(x0 = x, y0 = y, x = f_x(x, y, ...), y = f_y(x, y, ...)) %>%
    filter(!is.na(x), !is.na(y), )
  df = df %>%
    group_by(x, y) %>%
    summarise(n_xy = n(), .groups = "drop") %>%
    group_by(x) %>%
    arrange(desc(n_xy)) %>%
    mutate(
      n_x = sum(n_xy),
      p_top = max(n_xy) / n_x,
      rn = row_number(),
      dist = dstring(x, y),
      dist_top = dstring(x[1], y[1])
    ) %>%
    ungroup()
}

# dftmp = 
#   df3 %>% select(id, gov_id, name3 = .name) %>%
#   left_join(df4 %>% select(gov_id, name4 = .name), by = "gov_id", na_matches = "never") %>%
#   left_join(df2 %>% select(gov_id, name2 = name), by = "gov_id", na_matches = "never") %>%
#   mutate(name24 = coalesce(name2, name4)) %>%
#   filter(!is.na(name24)) %>%
#   select(id, gov_id, name3, name24)

extract_ngram = function(x, y, n=3, tail=0) {
  x = str_squish(str_replace_all(x, "[\\\\/]", " "))
  last4 = str_extract(str_remove_all(x, " "), paste0("[\\w]{", min(n), ",", max(n), "}", if (tail > 0) paste0("[\\w]{1,", tail, "}") else "$"))
  pattern = paste0(str_replace_all(last4, "(?<=\\w)(?=\\w)", " ?"), "$")
  str_extract(x, pattern)
}

extract_ngram_mapped = function(x, y, n=3) {
  x = str_squish(str_replace_all(x, "[\\\\/]", " "))
  y = str_squish(str_replace_all(y, "[\\\\/]", " "))
  gram = extract_ngram(x, y, n)
  complement = str_remove(x, paste0(gram, "$"))
  case_when(
    complement == "" ~ y,
    str_detect(y, paste0("^", complement)) ~  str_remove(y, paste0("^", complement)),
    TRUE ~ NA_character_
  )
}

lookup_substitute = function(s, df, prefix = "", suffix = "") {
  if (nrow(df) > 0) {
    for (i in seq(nrow(df))) {
      s = str_replace_all(s, paste0(prefix, df$x[i], suffix), df$y[i])
    }
    s
  } else {
    s
  }
}

myfn = function(x, df) {
  if (nrow(df) > 0) {
    x = chunk(x)
    df$x = str_replace_all(df$x, " ", "_")
    df$y = str_replace_all(df$y, " ", "_")
    x = lookup_substitute(x, df, prefix="", suffix="(?= |$)")
    str_replace_all(x, "_", " ")
  } else {
    x
  }
}

sequence_substitute = function(x, y, z) {
  lookup = fn0(x, y, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 3)
  lookup = lookup %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
  x1 = x = myfn(x, lookup)
  z1 = z = myfn(z, lookup)
  
  lookup = fn0(x, y, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 4)
  lookup = lookup %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
  x2 = x = myfn(x, lookup)
  z2 = z = myfn(z, lookup)
  
  lookup = fn0(x, y, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 5)
  lookup = lookup %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
  x3 = x = myfn(x, lookup)
  z3 = z = myfn(z, lookup)
  
  lookup4 = lookup = fn0(x, y)
  lookup = lookup %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
  x4 = x = lookup_substitute(x, lookup, prefix="(?<=^| )", suffix="(?= |$)")
  z4 = z = lookup_substitute(z, lookup, prefix="(?<=^| )", suffix="(?= |$)")
  
  list(x = x4, z = z4)
  
}

# dff3 = fn0(dftmp$name3, dftmp$name24, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 3)
# dff = dff3 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__0 = myfn(dftmp$name3, dff)
# df3$name__0 = myfn(df3$.name, dff)
# 
# dff4 = fn0(dftmp$name__0, dftmp$name24, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 4)
# dff = dff4 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__1 = myfn(dftmp$name__0, dff)
# df3$name__1 = myfn(df3$name__0, dff)
# 
# dff5 = fn0(dftmp$name__1, dftmp$name24, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 5)
# dff = dff5 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__2 = myfn(dftmp$name__1, dff)
# df3$name__2 = myfn(df3$name__1, dff)
# 
# dff0 = fn0(dftmp$name__2, dftmp$name24)
# dff = dff0 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__3 = lookup_substitute(dftmp$name__2, dff, prefix="(?<=^| )", suffix="(?= |$)")
# df3$name__3 = lookup_substitute(df3$name__2, dff, prefix="(?<=^| )", suffix="(?= |$)")

# dff3 = fn0(dftmp$name4, dftmp$name2, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 3)
# dff = dff3 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__0 = myfn(dftmp$name4, dff)
# df4$name__0 = myfn(df4$.name, dff)
# 
# dff4 = fn0(dftmp$name__0, dftmp$name2, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 4)
# dff = dff4 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__1 = myfn(dftmp$name__0, dff)
# df4$name__1 = myfn(df4$name__0, dff)
# 
# dff5 = fn0(dftmp$name__1, dftmp$name2, f_x = extract_ngram, f_y = extract_ngram_mapped, n = 5)
# dff = dff5 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__2 = myfn(dftmp$name__1, dff)
# df4$name__2 = myfn(df4$name__1, dff)
# 
# dff0 = fn0(dftmp$name__2, dftmp$name2)
# dff = dff0 %>% filter(dist_top > 0, n_x >= 2, p_top >= 0.9, rn == 1)
# dftmp$name__3 = lookup_substitute(dftmp$name__2, dff, prefix="(?<=^| )", suffix="(?= |$)")
# df4$name__3 = lookup_substitute(df4$name__2, dff, prefix="(?<=^| )", suffix="(?= |$)")

# 2 -> 4
dftmp = 
  df4 %>% select(id, gov_id, name4 = .name) %>%
  left_join(df2 %>% select(gov_id, name2 = name), by = "gov_id", na_matches = "never") %>%
  filter(!is.na(name2)) %>%
  select(id, gov_id, name4, name2)

tmp = sequence_substitute(dftmp$name4, dftmp$name2, df4$.name)
df4$..name = tmp$z

# 2,4 -> 3
dftmp = 
  df3 %>% select(id, gov_id, name3 = .name) %>%
  left_join(df4 %>% select(gov_id, name4 = ..name), by = "gov_id", na_matches = "never") %>%
  left_join(df2 %>% select(gov_id, name2 = name), by = "gov_id", na_matches = "never") %>%
  mutate(name24 = coalesce(name2, name4)) %>%
  filter(!is.na(name24)) %>%
  select(id, gov_id, name3, name24)

tmp = sequence_substitute(dftmp$name3, dftmp$name24, df3$.name)
df3$..name = tmp$z

# df_man_fix = 
#   dftmp %>%
#   mutate(
#     dist0 = dstring(name3, name24),
#     dist1 = dstring(name__3, name24)
#   ) %>% 
#   select(id, gov_id, name3, name__3, name24, dist0, dist1) %>%
#   filter(dist1 > 0.12 | (dist1 > dist0) | (str_detect(name__3, " \\w{1,2}( |$)") & dist1 > 0) | (!str_detect(name__3, "^[\u0600-\u06ff ]+$") & dist1 > 0) ) %>%
#   mutate(t0=transliterate(name3), t1=transliterate(name__3), tt = transliterate(name24))
# 
# write.csv(df_man_fix[!duplicated(df_man_fix[, c("id")]), ], "data/names_fix.csv", row.names = FALSE, na = "", quote = FALSE)


df3_fixes = read.csv("data/names_20240329b.csv", colClasses = c("integer", "character", "character"), na.strings = "")

df3 = df3 %>% # select(-name_fixed) %>%
  left_join(df3_fixes %>% select(id, name_fixed = name), by = "id") %>%
  mutate(
    name_old = name,
    name = coalesce(name_fixed, ..name)
  ) %>%
  select(-.name, -..name, -name_fixed)

df4 = df4 %>%
  mutate(
    name_old = name,
    name = ..name
  ) %>%
  select(-.name, -..name)


# Output --------------------------------------------------------------

# df3

out = df3

write.csv(
  out,
  file = output_path[3],
  quote = FALSE,
  na = "",
  row.names = FALSE
)

# Check write/read
tmp = read.csv(
  file = output_path[3],
  colClasses = { x = rep("character", 12); x[c(1,6)] = "integer"; x[4] = "Date"; x },
  na.strings = ""
)
all.equal(out, tmp)
rm("tmp")


# df4

out = df4

write.csv(
  out,
  file = output_path[4],
  quote = FALSE,
  na = "",
  row.names = FALSE
)

# Check write/read
tmp = read.csv(
  file = output_path[4],
  colClasses = { x = rep("character", 11); x[c(1,4)] = "integer"; x },
  na.strings = ""
)
all.equal(out, tmp)
rm("tmp")
