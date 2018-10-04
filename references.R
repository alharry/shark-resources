library(rvest)
library(tidyverse)
library(knitr)

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
  mutate(year = as.numeric(year)) %>% 
  separate(resource, into = c("resource", "DOI"), sep = "DOI: ") %>% 
  mutate(resource = str_replace_all(resource, "\n\t\t\t", " ")) %>%
  mutate(resource = str_replace_all(resource, "\t\t\t\t", "")) %>% 
  mutate(source = "Shark-references") %>% 
  arrange(year) %>% 
  filter(!resource == "") %>% 
  mutate(resource_id = "") %>% 
  mutate(suffix = "") %>% 
  # this removes accented characters, as those in "acuña2018", "araújo2018", "böhm1891" 
  mutate(author_year = iconv(resource, from = "UTF-8",to = "ASCII//TRANSLIT")) %>%
  # small number of weird references that don't carry through, get rid of them
  filter(!is.na(author_year)) %>% 
  mutate(author_year = tolower(str_extract(author_year, "^[\\w\\'\\’\\ \\~\\-\\\"]+"))) %>%
  mutate(author_year = paste0(author_year, year)) %>%
  mutate(author_year = str_replace_all(author_year, "[\\'\\’\\ \\~\\-\\\"]", "")) %>% 
  select(resource_id, resource, year, everything())

return(data)
}


# Assign suffix to resources 
letter_assign <- function(x){
  my_letters <- c("",letters, paste0("a", letters), paste0("b", letters), paste0("c", letters))
  
  if(nrow(x) >1){
      mutate(x, suffix = ifelse(suffix == "", my_letters[row_number()], suffix)) %>% 
      mutate(resource_id = paste0(author_year, suffix))
      } else {
      mutate(x, resource_id = paste0(author_year, suffix))
  }
  
}


## Step 1. Load reference list and log file
reference_list <- read_rds("shark-resources.rds") 
log_file <- read_rds("log.rds")
  
## Step 2. Get new references
  # First get all references from Shark References 
new_references <- map_df(LETTERS, scrape_refs) %>%
  # Use anti_join to separate out the new ones
  anti_join(reference_list, by = "resource")
  
  # Now load up the other references
other_references <- readxl::read_xlsx("other-references.xlsx",
                                      col_types = c("numeric", "text", "text")) %>%
  mutate(source = "Other") %>% 
  mutate(resource_id = "") %>% 
  mutate(suffix = "") %>% 
  mutate(author_year = iconv(resource, from = "UTF-8",to = "ASCII//TRANSLIT")) %>%
  filter(!is.na(author_year)) %>% 
  mutate(author_year = tolower(str_extract(author_year, "^[\\w\\'\\’\\ \\~\\-\\\"]+"))) %>%
  mutate(author_year = paste0(author_year, year)) %>%
  # Use anti_join to separate out the new ones
  anti_join(reference_list, by = "resource")

## Step 3. Join new and old references
reference_list <- full_join(new_references, other_references) %>% 
  full_join(reference_list) %>%
  arrange(resource_id) %>%
  split(.$author_year) %>% 
  map_df(letter_assign)
  
## Step 4. Create a new log file entry
log_file <- add_row(log_file, 
                    `date-accessed` = format(Sys.time(), "%b %d %Y %X"),
                    `shark-references` = table(reference_list$source)[2],
                    `other-references` = table(reference_list$source)[1])

## Step 5. Save data
write_rds(reference_list, "shark-resources.rds")
write_rds(log_file, "log.rds")

# Step 6. Render log
knit("shark-resources.Rmd")
