# GRADUATE STUDENT PAY ----
## Self reported income ---- 
## Reported as the full-time rate at in monthly increments for each role
## Actual money earned was ~60% of values here
ta_pay <- data.frame(
  hiring_period = c("T1910_AMS", # 2018 Fall AMS TA
                    "T1920_ESP", # 2019 Winter ESP 10
                    "T2010_PLS", # 2019 Fall PLS 49
                    "T2030_ESP", # 2020 Spring ESP 298
                    "T2110_ESP", # 2020 Fall ESP 298
                    "T2210_ESP"), # 2021 Fall ESP 298
  year = c(2018.4, # Denoting the year into 4 quarters (1 = Winter, 2 = Spring, 
           2019.1, #                                    3 = Summer, 4 = Fall)
           2019.4, 
           2020.2, 
           2020.4, 
           2021.4), 
  pay = c(4727.33, # 2018 Fall on contract as monthly 100%
          4727.33, # 2019 Winter on contract as monthly 100%
          4869.22, # 2019 Fall on contract as monthly 100%
          4869.24, # 2020 Spring on contract as monthly 100%
          5015.33, # 2020 Fall on contract as monthly 100%
          5165.89), # 2021 Fall on contract as monthly 100%
  position = "TA"
)

gsr_pay <- data.frame(
  hiring_period = c("T1940_S3", # 2019 Summer ILRP Grant
                    "T2040_S4", # 2020 Summer 3RFM Grant
                    "T2140_S8", # 2021 Summer WSARE  Grant
                    #"T2140_S8", # 2021 Summer 3RFM Grant but removing redundancy
                    #"T2210_S8", # 2021 Fall 3RFM Grant
                    "T2220_S8", # 2022 Winter DataLab IST Grant
                    "T2230_S8"), # 2022 Winter DataLab IST Grant
  year = c(2019.3, # Denoting the year into 4 quarters (1 = Winter, 2 = Spring,
           2020.3, #                                    3 = Summer, 4 = Fall)
           2021.3,
           #"T2140_S8",
           #2021.4,
           2022.1,
           2022.2),
  pay = c((48144.00/12), # 2019 Summer offered as the annual 100%
          4463, # 2020 Summer offered as monthly 100%
          (1149.22*4), # 2021 Summer offered as the monthly 25% FTE
          #(71787/12), # 2021 Summer offered as the annual 100%
          #(73941/12), # offered as the annual 100%
          (73941/12), # offered as the annual 100%
          (73941/12)), # offered as the annual 100%
  position = "GSR"
)

student_pay <- rbind(ta_pay, gsr_pay) %>% 
  arrange(year) %>% 
  select(-hiring_period, -position) %>% 
  mutate(year_simple = str_extract(year, "\\d{4}")) %>% 
  group_by(year_simple) %>% 
  summarise(avg_annual_pay = mean(pay)*12) %>% 
  filter(year_simple != 2018) %>% 
  pivot_wider(names_from = "year_simple",
              values_from = "avg_annual_pay") %>% 
  rename("pay19" = `2019`, "pay20" = `2020`, 
         "pay21" = `2021`, "pay22" = `2022`) %>% 
  mutate(increase20 = pay20/pay19,
         increase21 = pay21/pay20,
         increase22 = pay22/pay21)
cols <- which(colnames(student_pay) %in% c("increase20","increase21"))
student_pay$average_raise_20to21 = rowMeans(student_pay[,cols[1]:cols[2]], na.rm = T)

