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
## Full year appointments ----
fy_url <- "https://transparentcalifornia.com/salaries/search/?q=Prof-Fy&y=2021&a=university-of-california"
pages <- get_page_urls(fy_url)
fydf <- lapply(pages,  scrape_salary_table)
fydf <- do.call('rbind', fydf)

## Academic year appointments, associate ----
acay_url <- "https://transparentcalifornia.com/salaries/search/?q=Assoc-Prof-Ay&y=2021&a=university-of-california"
pages <- get_page_urls(acay_url)
acaydf <- lapply(pages,  scrape_salary_table)
acaydf <- do.call('rbind', acaydf)

## Academic year appointments, assistant ----
asay_url <- "https://transparentcalifornia.com/salaries/search/?q=Asst-Prof-Ay&y=2021&a=university-of-california"
pages <- get_page_urls(asay_url)
asaydf <- lapply(pages,  scrape_salary_table)
asaydf <- do.call('rbind', asaydf)

## Academic year appointments, full (multiple pulls given rate limit) ----
fay_url <- "https://transparentcalifornia.com/salaries/search/?q=Prof-Ay&y=2021&a=university-of-california"
pages <- get_page_urls(fay_url)
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf1 <- do.call('rbind', faydf)
pages <- get_page_urls(fay_url, sort = '-name')
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf2 <- do.call('rbind', faydf)
pages <- get_page_urls(fay_url, sort = 'base')
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf3 <- do.call('rbind', faydf)
pages <- get_page_urls(fay_url, sort = '-base')
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf4 <- do.call('rbind', faydf)
pages <- get_page_urls(fay_url, sort = '-title')
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf5 <- do.call('rbind', faydf)
pages <- get_page_urls(fay_url, sort = 'title')
pages <- pages[1:50]
faydf <- lapply(pages,  scrape_salary_table)
faydf6 <- do.call('rbind', faydf)

faydf <- unique(rbind(faydf1, faydf2, faydf3, faydf4, faydf5, faydf6))
aydf <- unique(rbind(faydf, asaydf, acaydf))
aydf$appointment = "AY"
fydf$appointment = "FY"

# Filtering out special cases ----
## Clinical, Adjunct, Law, In residence
df <- rbind(fydf, aydf) %>% 
  mutate(level = ifelse(str_detect(Job.title, 'Asst Prof'), "Assistant",
                 ifelse(str_detect(Job.title, 'Assoc Prof'), "Associate",
                    'Full'))) %>% 
  filter(!(str_detect(Job.title, "Clin\\b|Adj\\s|Law\\b|In Res"))) %>% 
  filter(Regular.pay != "$0.00") %>% 
  mutate(Job.title = trimws(str_remove(Job.title, "University of California"))) %>% 
  mutate(Regular.pay = as.numeric(str_remove_all(Regular.pay,  "\\$|,")),
         Other.pay = as.numeric(str_remove_all(Other.pay,  "\\$|,")),
         Benefits = as.numeric(str_remove_all(Benefits,  "\\$|,")),
         Total.pay = as.numeric(str_remove_all(Total.pay, "\\$|,"))) %>% 
  filter(!(Regular.pay < 68100)) %>% 
  select(-Overtime.pay)

# UCOP pay scale ---- 
## General: https://ap.ucsb.edu/compensation.and.benefits/ucsb.salary.scales/1.pdf
# #Business, econ, engineering: https://ap.ucsb.edu/compensation.and.benefits/ucsb.salary.scales/3.pdf
level <- c(rep("Assistant", 10), rep("Associate", 9), rep("Full", 18))
dollars <- c(68100,72200, 76100, 80500, 84800, 89200, 95200, 99900, 105200, 109900,
             84900,89300, 93800, 99400, 107100, 110000, 114100, 118500, 122900,
             99500, 107200, 115500, 124000, 133200, 143400, 154600, 167100, 181100, 
             123000, 128000, 134200, 141400, 149500, 160200, 171600, 183800, 198400)
scalepay <- data.frame(
  "level" = level,
  "scalepay" = dollars
)

# Numeric comparison of pay scales ----
proposed <- scalepay %>% 
  group_by(level) %>% 
  summarize(proposed_median = median(scalepay))

regular <- df %>% 
  group_by(level) %>% 
  summarize(regular_median = median(Regular.pay, na.rm = T))

total <- df %>% 
  group_by(level) %>% 
  summarize(total_median = median(Total.pay, na.rm = T))

left_join(proposed, regular) %>% left_join(total)

# Visual comparison of pay scales ----
df_long <- df %>% 
  filter(appointment == "AY") %>% 
  left_join(scalepay) %>% 
  pivot_longer(cols = c(Regular.pay, Total.pay, scalepay), names_to = 'PayType',
               values_to = 'Dollars') %>% 
  mutate(PayType = factor(PayType, levels = c('scalepay', 'Regular.pay', 'Total.pay')))
  
levels(df_long$PayType) = c("UCOP Scale", "Regular pay", "Regular + Other pay") 

df_long %>% 
  ggplot(aes(x = level, y = Dollars, color = PayType)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = comma, limits = c(0,400000)) +
  scale_color_manual(values = c("darkgreen", "darkblue", "darkred")) +
  theme_bw() +
  labs(x = "", y = "Annual salaries ($)", color = "",
       title = "How much do UC faculty earn?",
       subtitle = "All UC faculty (except Medical and Law faculty)",
       caption = "Source: Transparent California")

# ESP only ----
esp <- read.csv('data/esp21_pay.csv') %>% 
  left_join(scalepay, by = c("title" = "level")) %>% 
  pivot_longer(cols = c(Regular.pay, Total.pay, scalepay), names_to = 'PayType',
               values_to = 'Dollars') %>% 
  mutate(PayType = factor(PayType, levels = c('scalepay', 'Regular.pay', 'Total.pay')))

levels(esp$PayType) = c("UCOP Scale", "Regular pay", "Regular + Other pay") 

esp %>% 
  ggplot(aes(x = title, y = Dollars, color = PayType)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("darkgreen", "darkblue", "darkred")) +
  theme_bw() +
  labs(x = "", y = "Annual salaries ($)", color = "",
       title = "How much do UC faculty earn?",
       subtitle = "UC Davis Environmental Science and Policy Department",
       caption = "Source: Transparent California")

