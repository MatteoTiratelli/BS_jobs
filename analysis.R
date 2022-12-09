library(tidyverse)
library(haven)
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}



### YouGov #########

YouGov <- tibble(Study = c('USA (2015)','USA (2015)','USA (2015)',
                           'UK (2015)','UK (2015)','UK (2015)',
                           'USA (2021)','USA (2021)','USA (2021)'),
                 Variable = c('It is','It is not',"Unsure",
                              'It is','It is not',"Unsure",
                              'It is','It is not',"Unsure"),
                 Value = c(59,24,17,
                           50,37,13,
                           55,22,23))

YouGov$Variable <- factor(YouGov$Variable, levels = c('It is',"Unsure",'It is not'))

ggplot(YouGov, aes(x = Variable, y = Value)) +
  geom_col(colour = 'black', fill = 'grey80') + facet_grid(cols = vars(Study)) + 
  scale_y_continuous(limits = c(0,70)) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Figure 1: Evidence for "bullshit jobs" in the UK and US',
                                 subtitle = "Do you think that your job is or is not making a meaningful contribution to the world?",
                                 caption = "Notes: Data from YouGov. All percentages are weighted.") +
  geom_text(aes(y = (Value + 5),label = paste0(Value,'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_blank())

ggsave("YouGov.pdf", width = 6, height = 3, units = "in")


### ISSP #########

read_dta("/Users/matteo/Downloads/BS jobs/ZA1840.dta") %>% 
  drop_na(v66) %>%
  mutate(variable = ifelse(v66 %in% c(1,2),"Agree",
                           ifelse(v66 %in% c(3),"Unsure",
                                  "Disagree"))) %>%
  mutate(v3 = haven::as_factor(v3)) %>%
  group_by(v3) %>%
  count(variable, wt = v136) %>%
  summarise(variable = variable, n = n, prop = n/sum(n), year = 1989) -> ISSP_1989
read_dta("/Users/matteo/Downloads/BS jobs/ZA3090.dta") %>% 
  filter(!(v41 %in% c(0,9))) %>%
  mutate(variable = ifelse(v41 %in% c(1,2),"Agree",
                           ifelse(v41 %in% c(3,8),"Unsure",
                                  "Disagree"))) %>%
  mutate(v3 = haven::as_factor(v3)) %>%
  group_by(v3) %>% 
  count(variable, wt = weight) %>%
  summarise(variable = variable, n = n, prop = n/sum(n), year = 1997) -> ISSP_1997
read_dta("/Users/matteo/Downloads/BS jobs/ZA4350_v2-0-0.dta") %>% 
  filter(!(V35 %in% c(0,9,NA))) %>%
  mutate(variable = ifelse(V35 %in% c(1,2),"Agree",
                           ifelse(V35 %in% c(3,8),"Unsure",
                                  "Disagree"))) %>%
  mutate(v3 = haven::as_factor(COUNTRY)) %>%
  group_by(v3) %>%
  count(variable, wt = WEIGHT) %>%
  summarise(variable = variable, n = n, prop = n/sum(n), year = 2005) %>%
  mutate(v3 = gsub("[\\(\\)]", "", regmatches(v3, gregexpr("\\(.*?\\)", v3))[[1]])) -> ISSP_2005
read_dta("/Users/matteo/Downloads/BS jobs/ZA6770_v2-1-0.dta") %>%
  filter(!(v28 %in% c(0, 9,NA))) %>%
  mutate(variable = ifelse(v28 %in% c(1,2),"Agree",
                           ifelse(v28 %in% c(3,8),"Unsure",
                                  "Disagree"))) %>%
  mutate(v3 = haven::as_factor(country)) %>%
  group_by(v3) %>%
  count(variable, wt = WEIGHT) %>%
  summarise(variable = variable, n = n, prop = n/sum(n), year = 2015) %>%
  mutate(v3 = substr(v3, 1,2)) -> ISSP_2015

bind_rows(ISSP_1989, ISSP_1997, ISSP_2005, ISSP_2015) %>%
  mutate(v3 = recode(v3, 'gb' = 'UK', 'GB-GBN' = 'UK', 'GB' = 'UK', 
                     'US' = 'USA', 'usa' = 'USA')) %>%
  filter(v3 %in% c('UK', 'USA')) %>%
  mutate(variable = factor(variable, levels = c('Agree',"Unsure",'Disagree')),
         prop = prop*100) %>%
  ggplot(aes(x = variable, y = prop)) +
  geom_col(colour = 'black', fill = 'grey80') + facet_grid(rows = vars(v3), cols = vars(year)) + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,50,100)) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Evidence for the prevalence of "bullshit jobs" (ISSP)',
                                 subtitle = "How much do you agree or disagree with... My job is useful to society",
                                 caption = "Notes: All percentages are weighted") +
  geom_text(aes(y = (prop + 10), label = paste0(round(prop),'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_blank())

ggsave("/Users/matteo/Downloads/BS jobs/ISSP.pdf", width = 6, height = 4, units = "in")


bind_rows(ISSP_1989, ISSP_1997, ISSP_2005, ISSP_2015) %>%
  mutate(v3 = recode(v3, 'gb' = 'UK', 'GB-GBN' = 'UK', 'GB' = 'UK', 
                     'US' = 'USA', 'usa' = 'USA',
                     "a" = "Austria", "AT" = "Austria",
                     "cz" = "Czech Republic", "CZ" ="Czech Republic",
                     'dk' = "Denmark", 'DK' = "Denmark",
                     "f" = "France", "FR" = "France",
                     "d" = "Germany (West)", "DE" = "Germany (West)", "D-W" = "Germany (West)", "DE-W" = "Germany (West)",
                     "h" = "Hungary", "HU" = "Hungary",
                     "j" = "Japan", "JP" = "Japan",
                     "NL" = "Netherlands", "nl" = "Netherlands", 
                     "NZ" = "New Zealand", "nz" = "New Zealand",
                     "n" = "Norway", "NO" = "Norway",
                     "e" = "Spain", "ES" = "Spain",
                     "s" = "Sweden", "SE" = "Sweden",
                     "ch" = "Switzerland", "CH" = "Switzerland")) %>%
  filter(v3 %in% c('UK', 'USA', "Denmark", "France", "Germany (West)",
                   "Japan","New Zealand","Norway","Spain","Sweden","Switzerland")) %>%
  mutate(variable = factor(variable, levels = c('Agree',"Unsure",'Disagree')),
         prop = prop*100) %>%
  filter(variable == "Agree") %>% #group_by(year) %>% summarise(prop = mean(prop))
  bind_rows(tibble(v3 = "Average", variable = "Agree", n = 10, prop = c(66.6,65.9,68.6,72.3), year = c(1989,1997,2005,2015))) %>%
  mutate(v3 = fct_relevel(v3, "Average"),
         prop = 100-prop) %>%
  ggplot(aes(x = year, y = prop, colour = v3)) +
  geom_line() + 
  facet_wrap(facets = vars(v3), nrow = 4) + 
  gghighlight::gghighlight(v3 %in% c("Average"), calculate_per_facet = TRUE, 
                           use_direct_label = FALSE, unhighlighted_params = list(colour = "black")) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Figure 2: Bullshit jobs in developed economies',
                                 subtitle = "Proportion who disagree or are unsure that their job is useful to society",
                                 caption = "Notes: Data from the International Social Survey Programme. All percentages are weighted.") +
  scale_x_continuous(limits = c(1988,2016), breaks = c(1989, 1997,2005,2015)) +
  scale_y_continuous(breaks = c(20,30,40), labels = c("20%","30%","40%"))+
  scale_colour_manual(values = c("blue")) +
  MyThemes::theme_base() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'))
ggsave("/Users/matteo/Downloads/BS jobs/ISSP2.pdf", width = 6, height = 4, units = "in")


### EWCS ##########

EWCS <- read_dta("/Users/matteo/Downloads/BS jobs/UKDA-7363-stata/stata/stata13/ewcs_1991-2015_ukda_18mar2020.dta")

EWCS <- EWCS[EWCS$year > 2004,]

EWCS %>% 
  select(EU15, year, y15_Q61j, w4, w5_EU15) %>%
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  filter(EU15 == 1) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  group_by(year) %>%
  count(variable, wt = w5_EU15) %>%
  summarise(variable = variable, n = n, prop = n/sum(n), v3 = 'EU 15') %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels = c('Often',"Sometimes",'Rarely'))) %>%
  mutate(prop = prop*100) %>%
  ggplot(aes(x = variable, y = prop)) +
  geom_col(colour = 'black', fill = 'grey80') + 
  facet_grid(cols = vars(year), rows = vars(v3)) + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,50,100)) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Evidence for the prevalence of "bullshit jobs" (EWCS)',
                                 subtitle = "How often do you have the feeling of doing useful work?",
                                 caption = "Notes: All percentages are weighted.") +
  geom_text(aes(y = (prop + 7.5), label = paste0(round(prop),'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_blank())

# NB: Interviewer variance was also significant in the analysis targeting an 
# ordinal variable, Q61j You have the feeling of doing useful work? Roughly 20% 
# of the variation in the reported feeling of doing useful work was explained by 
# the interviewer differences

ggsave("EWCS.pdf", width = 6, height = 3, units = "in")


EWCS %>% 
  select(EU15, countid, year, y15_Q61j, w4) %>%
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  filter(EU15 == 1) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  group_by(year, countid) %>%
  count(variable, wt = w4) %>%
  summarise(variable = variable, n = n, prop = n/sum(n)) %>%
  ungroup() %>%
  filter(variable == "Often") %>%
  mutate(prop = prop*100,
         country = as_factor(countid)) %>%
  select(year, country, prop) %>%
  bind_rows(tibble(year = c(2005,2010,2015), country = "Average", prop = c(80.2, 83.5, 85.7))) %>%
  mutate(country = fct_relevel(country, "Average"),
         prop = 100-prop) %>%
  ggplot(aes(x = year, y = prop, colour = country)) +
  geom_line() + 
  gghighlight::gghighlight(country %in% c("Average"), calculate_per_facet = TRUE, 
                           use_direct_label = FALSE, unhighlighted_params = list(colour = "black")) +
  facet_wrap(facets = vars(country), nrow = 4) + 
  xlab(NULL) + ylab(NULL) + labs(title = 'Figure 3: Bullshit jobs in western Europe',
                                 subtitle = "Proportion who rarely or sometimes have the feeling of doing useful work",
                                 caption = "Notes: Data from the European Working Conditions Survey. All percentages are weighted.") +
  scale_colour_manual(values = c("blue")) +
  scale_x_continuous(limits = c(2004,2016), breaks = c(2005, 2010, 2015), labels = c('2005','2010','2015')) +
  scale_y_continuous(breaks = c(10,20), labels = c('10%','20%')) +
  MyThemes::theme_base() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'))
ggsave("/Users/matteo/Downloads/BS jobs/EWCS2.pdf", width = 6, height = 4, units = "in")


### By employment sector ##########

EWCS %>%  # if you do it across all countries you get basically flat lines, i.e. no pattern by employment sector
  select(countid, year, y15_Q61j, w4, w5_EU15, y15_isco_08_2, EU15) %>% # 2005 uses 88 ISCO scheme and can't really do anything useful with only 2 digits of that...
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  filter(EU15 == 1) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  drop_na(y15_isco_08_2) %>%
  mutate(BS = ifelse(y15_isco_08_2 %in% c(12,24,33),'Information work', # Administrative and Commercial Managers, Business and Administration Professionals, Business and Administration Associate Professionals
                     ifelse(str_detect(y15_isco_08_2, "^4"),'Clerical support',
                            'All other sectors'))) %>%
  group_by(BS) %>%
  count(variable, wt = w5_EU15) %>%
  summarise(variable = variable, n = n, prop = n/sum(n)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels = c('Often',"Sometimes",'Rarely')),
         BS = factor(BS, levels = c('Information work','Clerical support','All other sectors'))) %>%
  mutate(prop = prop*100,
         country = "EU15") %>%
  filter(variable == "Often") %>%
  bind_rows(tibble(BS = c("All other sectors","Clerical support","Information work"),
                   variable = "Often",
                   n = 1,
                   prop = c(78.0, 71.1,84.5),
                   country = "United Kingdom")) %>%
  ggplot(aes(x = BS, y = prop, group = country)) +
  geom_line() + 
  gghighlight::gghighlight(country %in% c("United Kingdom","EU15")) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Evidence for the prevalence of "bullshit jobs" (EWCS)',
                                 subtitle = "Proportion who often have the feeling of doing useful work",
                                 caption = "Notes: All percentages are weighted. Data is average for EU15 from 2010 and 2015 surveys.") +
  scale_y_continuous(limits = c(70,90), breaks = c(70,80,90), labels = c('70%','80%','90%')) +
  MyThemes::theme_base() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'))

ggsave("/Users/matteo/Downloads/BS jobs/EmploymentSector1.pdf", width = 6, height = 4, units = "in")



  
read_dta("/Users/matteo/Downloads/BS jobs/ZA6770_v2-1-0.dta") %>% # across all countries it is 7 per cent lower in "Information work" than all other sectors
  filter(!(v28 %in% c(0, 9,NA))) %>%
  mutate(v3 = substr(haven::as_factor(country),1,2)) %>%
  mutate(BS = ifelse(str_detect(ISCO08, "^12|^24|^33"),'Information work',
                     ifelse(str_detect(ISCO08, "^4"),'Clerical support',
                            'All other sectors'))) %>%
  mutate(variable = ifelse(v28 %in% c(1,2),"Agree",
                           ifelse(v28 %in% c(3,8),"Unsure",
                                  "Disagree"))) %>%
  group_by(BS, v3) %>%
  count(variable, wt = WEIGHT) %>%
  summarise(variable = variable, n = n, prop = (n/sum(n))*100) %>%
  mutate(variable = factor(variable, levels = c('Agree',"Unsure",'Disagree')),
         BS = factor(BS, levels = c('Information work','Clerical support','All other sectors')),
         v3 = v3) %>%
  filter(variable == "Agree") %>% 
  mutate(v3 = recode(v3, 'gb' = 'UK', 'GB-GBN' = 'UK', 'GB' = 'UK', 
                     'US' = 'USA', 'usa' = 'USA',
                     "a" = "Austria", "AT" = "Austria",
                     "cz" = "Czech Republic", "CZ" ="Czech Republic",
                     'dk' = "Denmark", 'DK' = "Denmark",
                     "f" = "France", "FR" = "France",
                     "d" = "Germany (West)", "DE" = "Germany (West)", "D-W" = "Germany (West)", "DE-W" = "Germany (West)",
                     "h" = "Hungary", "HU" = "Hungary",
                     "j" = "Japan", "JP" = "Japan",
                     "NL" = "Netherlands", "nl" = "Netherlands", 
                     "NZ" = "New Zealand", "nz" = "New Zealand",
                     "n" = "Norway", "NO" = "Norway",
                     "e" = "Spain", "ES" = "Spain",
                     "s" = "Sweden", "SE" = "Sweden",
                     "ch" = "Switzerland", "CH" = "Switzerland")) %>%
  filter(v3 %in% c('UK', 'USA', "Denmark", "France", "Germany (West)",
                   "Japan","New Zealand","Norway","Spain","Sweden","Switzerland")) %>% #group_by(BS) %>% summarise(prop = mean(prop))
  bind_rows(tibble(BS = c("All other sectors","Clerical support", "Information work"),
                   prop = c(74.9,61.1,63.1),
                   v3 = "Average",
                   variable = "Agree",
                   n = 10)) %>%
  ggplot(aes(x = BS, y = prop, colour = v3, group = v3)) +
  geom_line() + 
  gghighlight::gghighlight(v3 %in% c("Average"), calculate_per_facet = TRUE, 
                           use_direct_label = FALSE, unhighlighted_params = list(colour = "black")) +
  facet_wrap(facets = vars(v3), nrow = 4) + 
  xlab(NULL) + ylab(NULL) + labs(title = 'Evidence for the prevalence of "bullshit jobs" (ISSP)',
                                 subtitle = "Proportion agreeing their job is useful to society",
                                 caption = "Notes: All percentages are weighted.") +
  scale_y_continuous(breaks = c(50, 60,70,80), labels = c('50%',"60%","70%","80%")) +
  scale_colour_manual(values = c("blue")) +
  MyThemes::theme_base() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("/Users/matteo/Downloads/BS jobs/EmploymentSector2.pdf", width = 6, height = 4, units = "in")


EWCS %>% 
  select(countid, year, y15_Q61j, w4, w5, y15_isco_08_2) %>% # 2005 uses 88 ISCO scheme and can't really do anything useful with only 2 digits of that...
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  drop_na(y15_isco_08_2) %>%
  mutate(y15_isco_08_2 = haven::as_factor(y15_isco_08_2)) %>%
  group_by(y15_isco_08_2) %>%
  count(variable, wt = w5) %>%
  summarise(variable = variable, n = n, prop = n/sum(n)) %>%
  filter(variable == 'Often') %>%
  arrange(desc(prop)) %>%
  print(n = Inf)


read_dta("/Users/matteo/Downloads/BS jobs/ZA6770_v2-1-0.dta") %>%
  filter(!(v28 %in% c(0, 9,NA))) %>%
  drop_na(ISCO08) %>%
  mutate(ISCO08 = substr(ISCO08,1,2)) %>%
  mutate(variable = ifelse(v28 %in% c(1,2),"Agree",
                           ifelse(v28 %in% c(3,8),"Unsure",
                                  "Disagree"))) %>%
  group_by(ISCO08) %>%
  count(variable, wt = WEIGHT) %>%
  summarise(variable = variable, n = n, prop = (n/sum(n))*100) %>%
  filter(variable == 'Agree', n > 50) %>%
  arrange(desc(prop)) %>%
  print(n = Inf)

gridExtra::grid.arrange(p1,p2, nrow=2)

### Jobs growth by sector #########

read_csv("/Users/matteo/Downloads/BS jobs/empbysector.csv") %>%
  drop_na() %>%
  mutate(across(!c(Time, `All in employment2`), ~ .x * 1000)) %>%
  select(!c(`All in employment2`, `Public sector3`, `Private sector`)) %>%
  mutate(across(!c(Time), ~ (.x[Time == "Jan-Mar 2022"] - .x[Time == "Jan-Mar 1997"]))) %>% 
  filter(Time == "Jan-Mar 2022") %>%
  pivot_longer(-Time) %>%
  select(!c('Time')) %>%
  ggplot(aes(x = value, y = fct_reorder(name, (value)))) +
  geom_col() +
  geom_col(colour = 'black', fill = 'grey80') +
  scale_x_continuous(label = scales::comma) +
  xlab(NULL) + ylab(NULL) + MyThemes::theme_empty() + 
  labs(title = "Figure 4: New jobs by industry (UK, 1997 to 2022)",
       #subtitle = "New jobs created in the UK, 1997 to 2022",
       caption = "Source: ONS EMP13") +
  theme(legend.position = "none",
        plot.subtitle = element_text(face = 'italic'),
        panel.grid.major = element_line())
ggsave("/Users/matteo/Downloads/BS jobs/ONS.pdf", width = 6, height = 3, units = "in")


read_csv("/Users/matteo/Downloads/BS jobs/empbysectorUSA.csv") %>%
  select(Sector, Growth) %>%
  drop_na() %>%
  mutate(Growth = Growth*1000) %>%
  ggplot(aes(x = Growth, y = fct_reorder(Sector, (Growth)))) +
  geom_col() +
  geom_col(colour = 'black', fill = 'grey80') +
  scale_x_continuous(limits = c(-5000000,11000000), label = scales::comma) +
  xlab(NULL) + ylab(NULL) + MyThemes::theme_empty() + 
  labs(title = "Figure 5: New jobs by industry (USA, 1997 to 2022)",
       #subtitle = "New jobs created in the USA, 1997 to 2022",
       caption = "Source: U.S. Bureau of Labor Statistics") +
  theme(legend.position = "none",
        plot.subtitle = element_text(face = 'italic'),
        panel.grid.major = element_line())
ggsave("/Users/matteo/Downloads/BS jobs/BLS.pdf", width = 6, height = 2.5, units = "in")


read_csv("/Users/matteo/Downloads/BS jobs/working hours.csv") %>% #World Bank data
  filter(EMPSTAT == "TE") %>%
  select(Country, Time, Value) %>%
  ggplot(aes(x = Time, y = Value, colour = Country, group = Country)) + 
  geom_line() + ylab("Annual working hours") +
  gghighlight::gghighlight(Country %in% c('OECD countries', 'United Kingdom', "United States", 'Germany', 'France'))

read_csv("/Users/matteo/Downloads/BS jobs/Labour force participation rates.csv") %>% #World Bank data
  pivot_longer(-Year) %>%
  drop_na() %>%
  ggplot(aes(x = Year, y = value, colour = name, group = name)) + 
  geom_line() + ylab("Labour Force Participation Rate")

read_csv("/Users/matteo/Downloads/BS jobs/Labour market polarisation.csv") %>% #ILOSTAT: EMP_TEMP_SEX_OCU_NB_A
  filter(`Skill level` != "Total") %>%
  group_by(Country, Year) %>%
  mutate(prop = Value/sum(Value)) %>%
  ggplot(aes(x = Year, y = prop, colour = `Skill level`, group = `Skill level`)) + 
  geom_line() + ylab("Proportion of jobs by skill level") +
  facet_wrap(facets = vars(Country))


### Misc ##########

bind_rows(ISSP_1989, ISSP_1997, ISSP_2005, ISSP_2015) %>%
  group_by(variable) %>%
  summarise(prop = mean(prop)) %>%
  mutate(variable = factor(variable, levels = c('Agree',"Unsure",'Disagree')),
         prop = prop*100) %>%
  ggplot(aes(x = variable, y = prop, colour = variable, fill = variable)) +
  geom_col() + #facet_wrap(~ year) + 
  scale_y_continuous(breaks = c(0,50,100), labels = c('0','.5','1')) +
  xlab(NULL) + ylab(NULL) + labs(title = 'ISSP evidence for the prevalence of "bullshit jobs"',
                                 subtitle = "How much do you agree or disagree with the following: My job is useful to society",
                                 caption = "Notes: All percentages are weighted.") +
  geom_text(aes(y = (prop + 7),label = paste0(round(prop),'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


EWCS %>% # by country
  select(countid, year, y15_Q61j, w4, w5) %>%
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  group_by(countid) %>%
  count(variable, wt = w4) %>%
  summarise(variable = variable, n = n, prop = n/sum(n)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels = c('Often',"Sometimes",'Rarely'))) %>%
  mutate(facet = haven::as_factor(countid)) %>%
  ggplot(aes(x = variable, y = prop, colour = variable, fill = variable)) +
  geom_col() + facet_wrap(~ facet) + 
  scale_y_continuous(breaks = c(0,0.5,1), labels = c('0','.5','1')) +
  xlab(NULL) + ylab(NULL) + labs(title = 'EWCS evidence for the prevalence of "bullshit jobs" (2005 - 2015)',
                                 subtitle = "How often do you have the feeling of doing useful work?",
                                 caption = "Notes: All percentages are weighted.") +
  # geom_text(aes(y = (prop + 7),label = paste0(round(prop),'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

EWCS %>% # by year
  select(countid, year, y15_Q61j, w4, w5) %>%
  filter(y15_Q61j %in% c(1,2,3,4,5)) %>%
  mutate(variable = ifelse(y15_Q61j %in% c(1,2),"Often",
                           ifelse(y15_Q61j %in% c(3),"Sometimes",
                                  "Rarely"))) %>%
  group_by(year) %>%
  count(variable, wt = w5) %>%
  summarise(variable = variable, n = n, prop = n/sum(n)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels = c('Often',"Sometimes",'Rarely'))) %>%
  ggplot(aes(x = variable, y = prop, colour = variable, fill = variable)) +
  geom_col() + facet_wrap(~ year) + 
  scale_y_continuous(breaks = c(0,0.5,1), labels = c('0','.5','1')) +
  xlab(NULL) + ylab(NULL) + labs(title = 'EWCS evidence for the prevalence of "bullshit jobs"',
                                 subtitle = "How often do you have the feeling of doing useful work?",
                                 caption = "Notes: All percentages are weighted.") +
  # geom_text(aes(y = (prop + 7),label = paste0(round(prop),'%'))) +
  MyThemes::theme_empty() + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(face = 'italic'),
        strip.background = element_rect(color="black", linetype="solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


