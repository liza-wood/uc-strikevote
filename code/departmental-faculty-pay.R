library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(tidyr)

# UCD----
## Faculty listing ----
url <- "https://desp.ucdavis.edu/faculty"
page <- read_html(url)
faculty <- xml_find_all(page, '//*[(@id = "block-system-main")]//a') %>% 
  html_text() %>% 
  data.frame("name" = .)  %>% 
  filter(name != "") %>% 
  mutate(name_first = word(name, 1), 
         name_last = word(name, -1)) %>% 
  mutate(name_first = case_when(
    name_first == "Gwen" ~ "Gwendolyn",
    name_first == "Andy" ~ "Andrew",
    T ~ name_first)) %>% 
  mutate(url_name = paste(name_first, name_last, sep = "+"))

## Public salary listings ----
years <- 2010:2021
salarydf <- data.frame()
for(i in faculty$url_name){
  for(j in years){
  url <- paste0("https://transparentcalifornia.com/salaries/search/?a=university-of-california&q=",i,"&y=", j)
  page <- read_html(url)
  salary <- xml_find_all(page, '//*[(@id = "container-wrapper")]') %>% 
    xml_find_all(., ".//div//table") %>% 
    html_table() %>% 
    data.frame()
  if(nrow(salary) == 0){
    salary <- data.frame(
     "Name" = i, 
     "Job.title" = NA, 
     "Regular.pay" = NA, 
     "Overtime.pay" = NA, 
     "Other.pay" = NA, 
     "Total.pay" = NA,
     "Year" = NA)
  } else{
    salary <- salary %>% 
    select(Name, Job.title, Regular.pay, Overtime.pay, 
           Other.pay, Total.pay) %>% 
      mutate(Year = j) %>% 
      filter(str_detect(Job.title, "Prof|PROF|COOP EXT|DIRECTOR") & # Want professorss
               !str_detect(Job.title, "Adj|ADJ")) # Don't want adjuncts
  }
  salary <- salary %>% 
    mutate(Regular.pay = as.numeric(str_remove_all(Regular.pay, 
                                                   "\\$|\\,")),
           Other.pay = as.numeric(str_remove_all(Other.pay, 
                                                 "\\$|\\,")),
           Total.pay = as.numeric(str_remove_all(Total.pay, 
                                                 "\\$|\\,"))) 
  salarydf <- rbind(salarydf, salary)
}
}

faculty_pay <- salarydf %>% 
  filter(!is.na(Regular.pay)) %>% 
  mutate(Name = case_when(
    Year %in% 2011:2012 ~ trimws(paste(str_extract(Name, "(?<=,).*"), 
                                str_extract(Name, ".*(?=,)"))),
    T ~ Name)) %>% 
  mutate(name_first = toupper(word(Name, 1)), 
         name_last = toupper(word(Name, -1))) %>% 
  mutate(name_last = ifelse(name_last == "URRUTIA", "VALDOVINOS", name_last)) %>% 
  # a few things for double appointments so I will remove those again
  group_by(Name, name_first, name_last, Year, Job.title, Overtime.pay) %>% 
  summarize(Regular.pay = sum(Regular.pay),
            Other.pay = sum(Other.pay),
            Total.pay = sum(Total.pay)) %>% 
  pivot_wider(id_cols = c(name_first, name_last),
              names_from = "Year",
              values_from = "Regular.pay") %>% 
  select(name_first, name_last, `2011`, `2012`, `2013`, `2014`, `2015`:`2019`, `2020`, `2021`) %>% 
  # Identifying first year to remove since that pay is pro-rated
  mutate(first_year = case_when(
   is.na(`2021`) & !is.na(`2011`) ~ "retired",
   is.na(`2020`) ~ "2021",
   is.na(`2019`) ~ "2020",
   is.na(`2018`) ~ "2019",
   is.na(`2017`) ~ "2018",
   is.na(`2016`) ~ "2017",
   is.na(`2015`) ~ "2016",
   is.na(`2014`) ~ "2015",
   is.na(`2013`) ~ "2014",
   is.na(`2012`) ~ "2013",
   is.na(`2011`) ~ "2012",
   T ~ "long-term")) %>%
  mutate(`2021` = ifelse(first_year == "2021", NA, `2021`),
         `2020` = ifelse(first_year == "2020", NA, `2020`),
         `2019` = ifelse(first_year == "2019", NA, `2019`),
         `2018` = ifelse(first_year == "2018", NA, `2018`),
         `2017` = ifelse(first_year == "2017", NA, `2017`),
         `2016` = ifelse(first_year == "2016", NA, `2016`),
         `2015` = ifelse(first_year == "2015", NA, `2015`),
         `2014` = ifelse(first_year == "2014", NA, `2014`),
         `2013` = ifelse(first_year == "2013", NA, `2013`),
         `2012` = ifelse(first_year == "2012", NA, `2012`),
         `2011` = ifelse(first_year == "2011", NA, `2011`)) 
retirees <- filter(faculty_pay, first_year == "retired")
senior <- filter(faculty_pay, first_year == "long-term")
faculty_pay <- faculty_pay %>% 
  filter(first_year != "retired") %>% 
  # ID firstpay year
  mutate(first_pay_year = case_when(
    first_year == "long_term" ~ NA_integer_,
    T ~ as.integer(first_year)),
    first_pay_year = first_pay_year +1) %>% 
  rename("pay11" = `2011`, "pay12" = `2012`, "pay13" = `2013`, "pay14" = `2014`, 
         "pay15" = `2015`, "pay16" = `2016`, "pay17" = `2017`,  
         "pay18" = `2018`, "pay19" = `2019`, "pay20" = `2020`, "pay21" = `2021`) %>% 
  mutate(increase12 = pay12/pay11,
         increase13 = pay13/pay12,
         increase14 = pay14/pay13,
         increase15 = pay15/pay14,
         increase16 = pay16/pay15,
         increase17 = pay17/pay16,
         increase18 = pay18/pay17,
         increase19 = pay19/pay18, 
         increase20 = pay20/pay19,
         increase21 = pay21/pay20)

tempdf <- salarydf %>% 
  filter(Year == 2021) %>% 
  mutate(name_first = toupper(word(Name, 1)), 
         name_last = toupper(word(Name, -1))) %>% 
  mutate(name_last = ifelse(name_last == "URRUTIA", "VALDOVINOS", name_last)) %>% 
  mutate(title = ifelse(str_detect(Job.title, 'Asst Prof'), "Assistant",
                        ifelse(str_detect(Job.title, 'Assoc Prof'), "Associate",
                               'Full'))) %>% 
  select(name_last, title, Regular.pay, Total.pay)
write.csv(tempdf, 'data/esp21_pay.csv', row.names = F)

cols <- which(colnames(faculty_pay) %in% c("increase12","increase21"))
faculty_pay$average_raise_11to21 = rowMeans(faculty_pay[,cols[1]:cols[2]], na.rm = T)
cols <- which(colnames(faculty_pay) %in% c("increase18","increase21"))
faculty_pay$average_raise_18to21 = rowMeans(faculty_pay[,cols[1]:cols[2]], na.rm = T)

# FACULTY AWARDS ----
awards <- read.csv("~/Box/lgu/data_raw/university_awards/California_awards.csv",
                   skip = 2) %>% 
  filter(str_detect(tolower(PI.Name), 
                    tolower(paste(senior$name_last, collapse = "|")))) %>% 
  filter(!(str_detect(Total, "\\("))) %>% 
  mutate(Total = as.numeric(str_remove_all(Total, "\\$|\\,"))) %>% 
  group_by(PI.Name) %>% 
  summarize(total_award_value = sum(Total))


# MICHIGAN ----
## Faculty listing ----
## SEAS
url <- "https://seas.umich.edu/research/faculty"
page <- read_html(url)
faculty_saes <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "field--name-title", " " ))]') %>% 
  html_text() %>% 
  data.frame("name" = .) 
faculty_saes <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "field--type-string", " " )) and contains(concat( " ", @class, " " ), concat( " ", "field__item", " " ))]') %>% 
  html_text() %>% 
  data.frame("role" = .) %>% 
  cbind(faculty_saes)

## FORD
## Searched for core faculty
url <- "https://fordschool.umich.edu/directory?combine=&sort_bef_combine=field_last_name_value_ASC&field_profile_category_target_id=1&field_profile_type_target_id=82&field_department_reference_target_id=All&field_course_year_target_id=All"
page <- read_html(url)
faculty_ford <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "teaser__title", " " ))]//a') %>% 
  html_text() %>% 
  data.frame("name" = .) 
faculty_ford <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "teaser__profile-title", " " ))]') %>% 
  html_text() %>% 
  data.frame("role" = .) %>% 
  cbind(faculty_ford)
urlp2 <- paste0(url, "sort_by=field_last_name_value&sort_order=ASC&page=1")
page <- read_html(urlp2)
faculty_fordp2 <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "teaser__title", " " ))]//a') %>% 
  html_text() %>% 
  data.frame("name" = .) 
faculty_fordp2 <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "teaser__profile-title", " " ))]') %>% 
  html_text() %>% 
  data.frame("role" = .) %>% 
  cbind(faculty_fordp2)

faculty <- rbind(faculty_saes, faculty_ford, faculty_fordp2) %>% 
  mutate(name = trimws(str_squish(name)),
         role = trimws(str_squish(role))) %>% 
  mutate(name_first = word(name, 1), 
         name_last = word(name, -1)) %>% 
  #mutate(name_first = case_when(
  #  name_first == "Gwen" ~ "Gwendolyn",
  #  name_first == "Andy" ~ "Andrew",
  #  T ~ name_first)) %>% 
  mutate(url_name = paste0("FName=",name_first, "&LName=",name_last, "&Year="))

## SAES pay ----
pay_saes_p1 <- data.frame()
url <- "https://umsalary.info/deptsearch.php?Dept=Sch+for+Environ+and+Sustain&Year="
for(i in 0:4){
    page <- read_html(paste0(url, i))
    df <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "index", " " )) and (((count    (preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
      html_table() %>%
      data.frame() 
    colnames(df) <- df[1,]
    df <- slice(df, -1)
    df$year = 2021-i
    pay_saes_p1 <- rbind(pay_saes_p1, df)
}

url <- "https://umsalary.info/deptsearch.php?Dept=Sch%20for%20Environ%20and%20Sustain&Year="
pay_saes_pages <- data.frame()
for(i in 2:4){
  for(j in 0:4){
  page <- read_html(paste0(url, j, "&page=", i))
  df <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "index", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
    html_table() %>%
    data.frame() 
  colnames(df) <- df[1,]
  df <- slice(df, -1)
  df$year = 2021-j
  pay_saes_pages <- rbind(pay_saes_pages, df)
  }
}

## FORD pay ----
pay_ford_p1 <- data.frame()
url <- "https://umsalary.info/deptsearch.php?Dept=G.+Ford+Sc+Pub+Pol&Year="
for(i in 0:4){
  page <- read_html(paste0(url, i))
  df <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "index", " " )) and (((count    (preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
    html_table() %>%
    data.frame() 
  colnames(df) <- df[1,]
  df <- slice(df, -1)
  df$year = 2021-i
  pay_ford_p1 <- rbind(pay_ford_p1, df)
}

url <- "https://umsalary.info/deptsearch.php?Dept=G.%20Ford%20Sc%20Pub%20Pol&Year="
pay_ford_pages <- data.frame()
for(i in 2:4){
  for(j in 0:4){
    page <- read_html(paste0(url, j, "&page=", i))
    df <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "index", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
      html_table() %>%
      data.frame() 
    colnames(df) <- df[1,]
    df <- slice(df, -1)
    df$year = 2021-j
    pay_ford_pages <- rbind(pay_ford_pages, df)
  }
}

# only pages 5 for more recent years
url <- "https://umsalary.info/deptsearch.php?Dept=G.%20Ford%20Sc%20Pub%20Pol&Year="
pay_ford_p5 <- data.frame()
  for(j in 0:2){
    page <- read_html(paste0(url, j, "&page=", 5))
    df <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "index", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
      html_table() %>%
      data.frame() 
    colnames(df) <- df[1,]
    df <- slice(df, -1)
    df$year = 2021-j
    pay_ford_p5 <- rbind(pay_ford_p5, df)
  }

um_pay <- rbind(pay_saes_p1, pay_saes_pages, 
                pay_ford_p1, pay_ford_pages, pay_ford_p5) %>% 
  mutate(Name = str_squish(Name)) %>% 
  filter(!str_detect(Name, "Page: \\d of 4?5? 1 2 3 4\\s?5?"))

temp_df <- um_pay %>% 
  filter(year == 2021) %>% 
  mutate(Title = toupper(Title)) %>% 
  filter(str_detect(Title, 'PROFESSOR')) %>% 
  filter(!(str_detect(Title, ("CLINICAL|ADJUNCT|RESEARCH|EMERITUS")))) %>% 
  mutate(Title = tools::toTitleCase(tolower(Title))) %>% 
  mutate(rank = ifelse(str_detect(Title, 'Asst Prof'), "Assistant",
                       ifelse(str_detect(Title, 'Assoc Prof'), "Associate",
                              'Full')))

write.csv(temp_df, "data/um-salaries.csv", row.names = F)         
         