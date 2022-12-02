library(rvest)
library(xml2)
library(tidyverse)
source('~/Documents/Davis/R-Projects/quantifying_seed_innovation/code/functions_patterns.R')

letter <- read_html('https://docs.google.com/document/u/1/d/e/2PACX-1vQ04GGHsxG5zX0t8zhXd35PxT7zBrxt2MR_KdcFL_yPRsWyb43mib6e3qbHibOvMEnW_48-j8WR8ZOw/pub?urp=gmail_link')
text <- letter %>% 
  xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "c0", " " ))]') %>% 
  html_text()
df <- data.frame("signature" = text[5:length(text)]) %>% 
  filter(signature != "")
unis <- paste(c("UC San Diego", "UC Davis", "UCLA", "UC Santa Barbara",
                "UC Irvine", "UC Berkeley", "UC San Francisco",
                "UC Merced", "UC Riverside", "UC Santa Cruz"), collapse = "|")
# SD: https://grad.ucsd.edu/about/grad-data/enrollment.html
# Davis https://aggiedata.ucdavis.edu/#all
# LA: https://grad.ucla.edu/graduate-program-statistics/enrollment/?t=Annualsnapshot
# SB: https://www.graddiv.ucsb.edu/graduate-statistics
# Irvine: https://uci.edu/university-facts/
# Berkeley https://grad.berkeley.edu/wp-content/uploads/2020-21-Graduate-Student-Profile-Final.pdf
# SF: https://oir.ucsf.edu/ucsf-glance#ENROLLMENT
# merced: https://www.ucmerced.edu/fast-facts
# riverside: https://ir.ucr.edu/stats/enroll/overall
# Santa Cruz: https://mediafiles.ucsc.edu/iraps/3rd-week-enrollments/fall-2022.pdf

grad_ppltn <- c(6573, 4626, 11890, 3149, 
                6090, 3539, 1410, 
                760, 3585, 1976)
unidata <- data.frame("ppltn" = grad_ppltn,
                      "uni" = c("UC San Diego", "UC Davis", "UCLA", "UC Santa Barbara",
                                "UC Irvine", "UC Berkeley", "UC San Francisco",
                                "UC Merced", "UC Riverside", "UC Santa Cruz"))
df$signature[1] <- str_remove(df$signature[1], "Signed,")
df$uni <- str_extract(df$signature, unis)
df$department <- str_remove(str_extract(df$signature, paste0('(?<=',unis,').*')), ', ')

total = nrow(df)
df %>% 
  group_by(uni) %>% 
  count() %>% 
  left_join(unidata) %>% 
  summarize(n = n, ppltn = ppltn, percent = round(100*(n/ppltn))) %>% 
  arrange(-desc(percent)) %>% 
  mutate(n = paste0("n=", n)) %>% 
  mutate(uni = factor(uni, levels = .$uni)) %>% 
  ggplot(aes(x = uni, y = percent, label = n)) +
  geom_col(fill = blues[5]) +
  geom_text(hjust = -.25, size = 3, color = '#1A242F') +
  coord_flip() +
  theme_minimal() +
  theme_osis() +
  ylim(c(0,22.5)) +
  labs(title = paste0("Students supporting a 'no' vote (", total, " total)"),
       subtitle = "Based on signatures from the 'no' vote petition",
       #caption = "Text in plots are the counts of students from each university",
       x = "", y = "Percent of graduate students (Masters + PhD)")

depts <- df %>% 
  mutate(department = str_remove_all(trimws(tolower(department)), 
                                     '^department of | department| phd|\\/\\s?phd')) %>% 
  group_by(department) %>% 
  count()