library(flextable)
library(janitor)

LMNG_tab <- ACC1  %>% 
  # filter(Lev=="1") %>% 
  # select(Batch, temp, month, `Inspection Lot`, `Water content`,Content, Spectrum,  `Biggest Unknown`,"1-Met, hylLMNGazole",
  # "2-MethylLMNGazole", "4-MethylLMNGazole", "1-MethoxymethylLMNGazole"  ) %>% 
  # filter(!str_detect(Batch,"038")) %>% 
  
  # filter(!(month == 0 & str_detect(`Inspection Lot`, "^89"))) %>% 
  # filter(!((Batch=="LMNG0016XX" |Batch=="LMNG0024XX") & temp=="40°C")) %>% 
  # rename("Individual unknown impurities" = "Individual ORC", "Sum of all impurities" = "Sum ORC + RC") %>% 
  # rename("5-MeTHFA-Ca water free" = "5-MeTHFA-Ca water free calc" ) %>% 
  # relocate("5-MeTHFA-Ca water free",  .after = "5-MeTHFA-Ca") %>% 
  mutate(across(8:43, ~ as.numeric(.))) %>%
  mutate(across(9:11, ~ round_half_up(., 2))) %>% 
  mutate(across(12:13, ~ round_half_up(., 0))) %>% 
  mutate(across(c(8,14:32), ~ round_half_up(., 4))) %>% 
  mutate(across(34:43, ~ round_half_up(., 2))) %>% 
  group_by(Batch) %>% 
  arrange(Batch, temp , month)

spalten <- colnames(LMNG_tab)
spalten <- gsub("\\(≥ 0.05%) ","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
names(LMNG_tab) <- spalten #neue, korrigierte Namen

tab <- as_grouped_data(LMNG_tab, groups = c("Batch"))
tab <- flextable(tab)
tab <- set_header_labels(tab, temp = "Storage Condition", month = "Duration")
tab

save_as_docx(tab, path = "LMNG_tab.docx")

#►----- nur MiBi

LMNG_tabMiBi <- LMNG  %>% 
  # filter(Lev=="1") %>% 
  select(c("Batch", "temp", "month", "Inspection Lot","Microbial Count (TAMC)", "Microbial Count (TYMC)")) %>%  #`
  # mutate(across(7:10, ~ round_half_up(., 2))) %>% 
  # mutate(across(10, ~ round_half_up(., 1))) %>%
  group_by(Batch) %>% 
  arrange(Batch, temp , month)

LMNG_tabMiBi$temp[LMNG_tabMiBi$month ==0] <- c("n/a") 
LMNG_tabMiBi <- unique(LMNG_tabMiBi)

tab2 <- as_grouped_data(LMNG_tabMiBi, groups = c("Batch"))
tab2 <- flextable(tab2)
tab2 <- set_header_labels(tab2, temp = "Storage Condition", month = "Duration")
tab2

save_as_docx(tab2, path = "LMNG_tabMiBi.docx")
