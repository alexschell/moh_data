library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)
library(arabicStemR)

# 1. Function Definitions ------------------------------------------------

flatten<-function(x) {
  dumnames<-unlist(getnames(x,T))
  dumnames<-gsub("(*.)\\.1","\\1",dumnames)
  repeat {
    x <- do.call(.Primitive("c"), x)
    if(!any(vapply(x, is.list, logical(1)))){
      names(x)<-dumnames
      return(x)
    }
  }
}

getnames<-function(x,recursive){
  
  nametree <- function(x, parent_name, depth) {
    if (length(x) == 0) 
      return(character(0))
    x_names <- names(x)
    if (is.null(x_names)){ 
      x_names <- seq_along(x)
      x_names <- paste(parent_name, x_names, sep = "")
    }else{ 
      x_names[x_names==""] <- seq_along(x)[x_names==""]
      x_names <- paste(parent_name, x_names, sep = "")
    }
    if (!is.list(x) || (!recursive && depth >= 1L)) 
      return(x_names)
    x_names <- paste(x_names, ".", sep = "")
    lapply(seq_len(length(x)), function(i) nametree(x[[i]], 
                                                    x_names[i], depth + 1L))
  }
  nametree(x, "", 0L)
}

url_to_list = function(html) {
  x = xml2::read_html(html)
  flatten(as_list(x))
}

get_report_victims = function(html) {
  
  x = url_to_list(html)
  
  idx0 = x %>% sapply(str_detect, "The victims were named as:") %>% which %>% head(1)
  idx1 = x %>% sapply(str_detect, "Media from sources|Sources|Summary|Geolocation notes") %>% which %>% head(1)
  
  if (length(idx0) == 0) {
    return(list())
  } else if (length(idx1) == 0 || idx1 <= idx0) {
    idx1 = x %>% names %>% str_detect("^html\\.body\\.main\\.div\\.article\\.div\\.div\\.?[0-9]+$") %>% which
    idx1 = idx1[idx1 > idx0] %>% head(1)
    if (length(idx1) == 0) stop("can't find stopping point")
  }
  
  x = x[idx0:(idx1 - 1)] %>% unlist
  
  df = 
    data.frame(
      name = names(x), 
      text = x
    ) %>%
    mutate(
      is_empty = !str_detect(x, "[^\n\t ]"),
      is_boundary = str_detect(name, "^html\\.body\\.main\\.div\\.article(\\.div){4}\\.?[0-9]+$"),
      group = cumsum(is_boundary)
    ) %>%
    filter(!is_empty) %>%
    group_by(group) %>%
    summarise(text = list(text))
  
  df$text
  
}


# 2. Read data --------------------------------------------------------

# 2.1. Incident level -------------------------------------------------

urls = readLines("data/airwars/incident_urls.txt")

ls = lapply(urls, get_report_victims)

df_incidents = data.frame(
  incident_id = str_extract(urls, "ispt[0-9]+[a-z]{0,2}"),
  url = urls
)
df_incidents$info = ls


# 2.2. Individual level -----------------------------------------------

df_individuals = df_incidents %>%
  tidyr::unnest(info) %>%
  group_by(incident_id) %>%
  filter(row_number() != 1) %>%
  mutate(group_index = cumsum(as.numeric(sapply(info, function(x) all(str_detect(x, "Family members|The victims were named as")))))) %>%
  ungroup %>%
  group_by(incident_id, group_index) %>%
  mutate(group_is_family = as.numeric(str_detect(paste0(info[1], collapse=" "), "Family members"))) %>%
  filter(group_index == 0 | row_number() > 1) %>%
  mutate(individual_index = case_when(group_is_family == 1 ~ row_number(), TRUE ~ ceiling(row_number() / 2))) %>%
  ungroup %>%
  group_by(incident_id) %>%
  mutate(group_index = group_index - min(group_index) + 1) %>%
  ungroup %>%
  mutate(
    group_id = paste(incident_id, str_pad(group_index, 2, "left", "0"), sep = "."),
    individual_id = paste(group_id, str_pad(individual_index, 3, "left", "0"), sep = ".")
  ) %>%
  select(-group_index, -individual_index) %>%
  group_by(incident_id, group_id, group_is_family, individual_id) %>%
  mutate(info = sapply(info, paste0, collapse=' | ')) %>%
  summarise(info = paste0(info, collapse = ' | '), .groups = "drop")


# 2.3. Item level: categorize items -----------------------------------

df_items = df_individuals
df_items$item = str_split(df_items$info, " \\| ")
df_items = df_items %>%
  tidyr::unnest(item) %>% 
  group_by(individual_id) %>% 
  mutate(item_id = row_number()) %>%
  mutate(
    item_type = case_when(
      item_id == 1 ~ "designator_en",
      str_detect(arabicStemR::removeDiacritics(item), "[\u0600-\u06ff]") & !str_detect(item, "[a-zA-Z]") ~ "designator_ar",
      item %in% c("killed", "injured") ~ "casualty_type",
      item %in% c("male", "female") ~ "sex",
      item %in% c("Adult", "Child", "Age unknown") ~ "age_category",
      str_detect(item, " years old") ~ "age",
      str_detect(item, "Matched to MoH ID |ossible match with MoH ID") ~ "moh_id",
      str_detect(tolower(item), "son|daught?er|child|wife|husband|mother|father|sister|brother|sibling|cousin|nephew|niece|unclear if related") ~ "relation",
      item == "pregnant" ~ "pregnant",
      TRUE ~ "."
    ),
    item_type = case_when(
      item_type == "." & coalesce(lead(item_type) == "casualty_type", TRUE) ~ "comment",
      str_detect(arabicStemR::removeDiacritics(item), "[\u0600-\u06ff]") & lag(item_type) == "designator_en" ~ "designator_ar",
      TRUE ~ item_type
    )
  ) %>% 
  ungroup

# Check
any(df_items$item_type == ".")

# Unexpected item item type patterns
df_items %>% 
  group_by(individual_id) %>% 
  mutate(pattern = paste0(item_type, collapse = " ")) %>%  
  ungroup %>%
  filter(!str_detect(pattern, "^designator_en( designator_ar)?( age| age_category)?( sex)?( pregnant)?( relation)?( comment)?( casualty_type)?( moh_id)?$")) %>% 
  select(individual_id, item_id, item, item_type)

# Fix
idx = df_items$individual_id %in% c("ispt0417.01.021", "ispt0417.01.022") & df_items$item_id == 5
df_items$item_type[idx] = "comment"


# 2.4. Map items back to individual level -----------------------------

df_individuals_ = 
  df_items %>% select(-item_id) %>%
  tidyr::spread(item_type, item) %>%
  select(
    incident_id:individual_id, info,
    designator_en, designator_ar, 
    age, age_category, sex, pregnant, relation, 
    comment, casualty_type, moh_id
  ) %>%
  mutate(
    .flags = case_when(
      str_detect(tolower(designator_en), "son|daught?er|child|wife|husband|mother|father|sister|brother|sibling|cousin|nephew|niece|unclear if related") ~ "designator is relation",
      TRUE ~ NA_character_
    ),
    .age = str_extract(age, "(?<=^ {0,2})[0-9.]+(?= +years old)") %>% as.numeric,
    .age_category = case_when(age_category == "Age unknown" ~ "unknown", TRUE ~ tolower(age_category)),
    .sex = case_when(sex == "male" ~ "m", sex == "female" ~ "f"),
    .pregnant = as.numeric(coalesce(pregnant, "") == "pregnant"),
    .moh_id = str_extract(moh_id, "[0-9]{5,9}")
  )

# Checks

df_individuals_ %>% filter(is.na(.age) != is.na(age)) %>% select(individual_id, age, .age)
df_individuals_ %>% filter(is.na(.age_category) != is.na(age_category)) %>% select(individual_id, age_category, .age_category)
df_individuals_ %>% filter(is.na(.sex) != is.na(sex)) %>% select(individual_id, sex, .sex)
df_individuals_ %>% filter((.pregnant != 1) != is.na(pregnant)) %>% select(individual_id, pregnant, .pregnant)
df_individuals_ %>% filter(is.na(.moh_id) != is.na(moh_id)) %>% select(individual_id, moh_id, .moh_id, relation, comment)

# Fixes

idx = df_individuals_$individual_id %in% c("ispt0057.01.002", "ispt0402.01.027", "ispt1669.01.073")
all(is.na(df_individuals_$relation[idx]))
df_individuals_$relation[idx] = str_remove(df_individuals_$moh_id[idx], "Matched to MoH ID ")

idx = df_individuals_$individual_id == "ispt1669.01.077"
all(is.na(df_individuals_$comment[idx]))
df_individuals_$comment[idx] = str_remove(df_individuals_$moh_id[idx], "Matched to MoH ID ")


# 3. Output -----------------------------------------------------------

# 3.1. Incidents ------------------------------------------------------

df_incidents_ = df_individuals_ %>%
  group_by(incident_id) %>%
  summarise(
    num_identified_casualties = n(),
    num_identified_killed = sum(casualty_type == "killed")
  )

df_incidents__ = df_incidents %>%
  mutate(date = lubridate::mdy(str_extract(urls, "[a-z]+-[0-9]{1,2}-[0-9]{4}"))) %>%
  select(-info) %>%
  left_join(df_incidents_, by = "incident_id") %>%
  mutate(
    num_identified_casualties = coalesce(num_identified_casualties, 0),
    num_identified_killed = coalesce(num_identified_killed, 0)
  ) %>%
  arrange(date)

write.csv(df_incidents__, "data/airwars/airwars_incidents.csv", quote = FALSE, na = "", row.names = FALSE)


# 3.1. Individuals ----------------------------------------------------

df_individuals__ = df_individuals_ %>%
  select(
    incident_id,
    group_id,
    group_is_family,
    individual_id,
    designator_en,
    designator_ar,
    age = .age,
    age_category = .age_category,
    sex = .sex,
    pregnant = .pregnant,
    relation,
    comment,
    casualty_type,
    moh_id = .moh_id,
    flags = .flags
  )

write.csv(df_individuals__, "data/airwars/airwars_individuals.csv", quote = FALSE, na = "", row.names = FALSE)
