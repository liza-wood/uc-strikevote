library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Functions ----
get_page_urls <- function(base_url, sort = 'name'){
  page_n <- read_html(paste0(base_url,'&s=', sort, '&page=', '1')) %>% 
    xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "page_subtitle", " " ))]') %>% 
    html_text() %>% 
    trimws() %>% 
    str_extract('\\d{1,3}$') %>% 
    as.numeric()
  
  pages <- c()
  for(i in 1:page_n){
    page <- paste0(base_url,'&s=', sort, '&page=', i)
    pages <- rbind(pages, page)
  }
  return(pages)
}
scrape_salary_table <- function(URL){
  url <- URL
  df <- read_html(url) %>% 
    xml_find_all('//*[(@id = "main-listing")]') %>% 
    html_table() %>% 
    data.frame() 
  return(df)
}

# Grabbing data from Transparent California ----
## Provost ----
prov_url <- "https://transparentcalifornia.com/salaries/search/?q=Provost&y=2021&a=university-of-california"
pages <- get_page_urls(prov_url)
df <- lapply(pages,  scrape_salary_table)
df <- do.call('rbind', df)

df <- df %>% 
  filter(!(str_detect(Name, "Provost"))) %>% 
  filter(!(str_detect(Job.title, "Interim \\b|College Provost|Faculty Asst To"))) %>% 
  filter(Regular.pay != "$0.00") %>% 
  mutate(Job.title = trimws(str_remove(Job.title, "University of California"))) %>% 
  mutate(Regular.pay = as.numeric(str_remove_all(Regular.pay,  "\\$|,")),
         Other.pay = as.numeric(str_remove_all(Other.pay,  "\\$|,")),
         Benefits = as.numeric(str_remove_all(Benefits,  "\\$|,")),
         Total.pay = as.numeric(str_remove_all(Total.pay, "\\$|,"))) %>% 
  select(-Overtime.pay)

write.csv(df, "data/uc-provost-salaries.csv", row.names = F)