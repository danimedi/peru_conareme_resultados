library(stringi)
library(magrittr)

get_gender <- function(first_names) {
  dict <- suppressMessages(readr::read_csv("data_raw/first_name-gender_dictionary.csv"))
  dict_lookup <- dict$gender
  dict_lookup <- setNames(dict_lookup, dict$name)
  first_names <- first_names %>%
    stri_trans_general(id = "Latin-ASCII") %>%
    stri_extract(regex = "\\w+") %>%
    tolower()
  dict_lookup[first_names]
}
