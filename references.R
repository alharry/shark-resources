library(rvest)
library(tidyverse)

scrape_refs <- function(letter = "A"){

data <- read_html(paste0("http://shark-references.com/literature/listAll/", letter))

Test <- data %>% 
  html_node(".list") %>%
  html_text() %>%
  str_split(pattern = "\n\t\t\t\t\t\t\n\t\t") %>%
  tibble() %>%  unnest() %>% .[1] %>%
  rename(resource = ".")  %>% 
  mutate(resource = str_trim(resource)) %>% 
  mutate(year = str_extract(resource, "[:digit:]+")) %>%
  separate(resource, into = c("resource", "DOI"), sep = "DOI: ") %>% 
  mutate(resource = str_replace_all(resource, "\n\t\t\t", " ")) %>%
  mutate(resource = str_replace_all(resource, "\t\t\t\t", "")) %>% 
  filter(!is.na(resource))

return(Test)
}

