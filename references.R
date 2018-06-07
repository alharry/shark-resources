library(rvest)
library(tidyverse)

## Function for scraping references from SharkReferences.com
scrape_refs <- function(letter = "A"){

# Read in html
my_link <- read_html(paste0("http://shark-references.com/literature/listAll/", letter))

# 
data <- my_link %>% 
  html_node(".list") %>%
  html_text() %>%
  str_split(pattern = "\n\t\t\t\t\t\t\n\t\t") %>%
  tibble() %>%  unnest() %>% .[1] %>%
  rename(resource = ".")  %>% 
  mutate(resource = str_trim(resource)) %>% 
  mutate(year = str_extract(resource, "[:digit:]{4}")) %>%
  separate(resource, into = c("resource", "DOI"), sep = "DOI: ") %>% 
  mutate(resource = str_replace_all(resource, "\n\t\t\t", " ")) %>%
  mutate(resource = str_replace_all(resource, "\t\t\t\t", "")) %>% 
  mutate(source = "Shark-references") %>% 
  arrange(year) %>% 
  filter(!is.na(resource))

return(data)
}

reference_list <- map_df(list("A", "B"), scrape_refs)


write_rds(reference_list)


# Refresh web-scraping