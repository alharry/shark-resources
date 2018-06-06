library(rvest)
library(tidyverse)

A <- read_html("http://shark-references.com/literature/listAll/A")


Test <- A %>% 
  html_node(".list") %>%
  html_text() %>%
  str_split(pattern = "\n\t\t\t\t\t\t\n\t\t") %>%
  tibble() %>%  unnest() %>% .[1] %>%
  rename(resource = ".")  %>% 
  mutate(resource = str_trim(resource)) %>% 
  mutate(year = str_extract(resource, "[:digit:]+")) %>%
  separate(resource, into = c("resource", "DOI"), sep = "DOI: ")
  
