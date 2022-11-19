library(tidyr)
library(rvest)
library(xml2)
# MEDIAN INCOME, US CENSUS DATA ----
## Source ----
# https://www.census.gov/quickfacts/fact/table/US/PST045221
# Downloaded and is available in data folder -- did not scrape because difficult

## Data ---- 
census <- read.csv("data/uscb.csv") %>% 
  filter(Fact == "Per capita income in past 12 months (in 2020 dollars), 2016-2020") %>% 
  select(Ann.Arbor.city..Michigan, Tempe.city..Arizona,
         Bloomington.city..Indiana,
         Gainesville.city..Florida, Seattle.city..Washington,
         Davis.city..California, Athens.Clarke.County.unified.government..balance...Georgia) %>% 
  rename(#"per_capita_income" = Fact,
         "University of Michigan, Ann Arbor" = Ann.Arbor.city..Michigan,
         "Arizona State University" = Tempe.city..Arizona,
         "Indiana University, Bloomington" = Bloomington.city..Indiana,
         "University of Florida" = Gainesville.city..Florida,
         "University of Washington" = Seattle.city..Washington,
         "University of California, Davis" = Davis.city..California,
         "University of Georgia" = Athens.Clarke.County.unified.government..balance...Georgia
         ) %>% 
  pivot_longer(cols = `University of Michigan, Ann Arbor`:`University of Georgia`, 
               names_to = "university", values_to = "per_capita_income") %>% 
  mutate(per_capita_income = as.numeric(str_remove_all(per_capita_income, 
                                                       "\\$|\\,")))

# LIVING WAGE, MIT CALCULATOR ----
## Source ----
# https://livingwage.mit.edu/

lwc <- data.frame(
  University = c("University of California, Davis",  
                 "University of California, Santa Barbara",
                 "University of California, Los Angeles",
                 "University of Michigan, Ann Arbor",  # Could only ID Yolo county
                 "Arizona State University" , # Phoenix area
                 "University of Florida", "University of Washington",
                 "University of Georgia", "Indiana University, Bloomington"),
  code = c("counties/06113", "metros/42200", "metros/31080",
           "metros/11460", "metros/38060", 
           "metros/23540", "metros/42660", "metros/12020", "metros/14020"))
## Data ----
cost_of_living <- data.frame()
for(i in 1:nrow(lwc)){
  Sys.sleep(3)
  url <- "https://livingwage.mit.edu/"
  url <- paste0(url,lwc$code[i])
  page <- read_html(url)
  ri <- xml_find_all(page, '//*[contains(concat( " ", @class, " " ), concat( " ", "table_wrap", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]') %>% 
    html_table() %>% 
    data.frame() %>% 
    filter(Var.1 == "Required annual income before taxes") %>% 
    select(X1.ADULT) %>% 
    rename("living_wage_income" = X1.ADULT) %>% 
    mutate(living_wage_income = as.numeric(str_remove_all(living_wage_income, 
                                                         "\\$|\\,"))) %>% 
    mutate(university = lwc$University[i])
    
  cost_of_living <- rbind(cost_of_living, ri)
}

# COMBINING DATA ----
costs <- full_join(census, cost_of_living) %>% 
  mutate(city = c("Ann Arbor, MI", "Tempe, AZ", "Bloomington, IN",
                  "Gainseville, FL", "Seattle, WA", "Davis, CA", 
                  "Athens, GA", "Santa Barbara, CA", "Los Angeles, CA"))

write.csv(costs, "data/cost_of_living.csv", row.names = F)
