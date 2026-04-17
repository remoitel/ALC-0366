ACC1_L2_raw <- read_excel("ALC-0366_36mteY2Y6L2.xlsx")


ACC1_L2 <- ACC1_L2_raw %>%
  filter(Lev==2) %>% 
  # filter(!is.na(`UD code`) )%>% 
  select("Batch", "Inspection Lot", "IL short text",contains("Level Unkn"),,contains("Level Impu"),contains("RRT Unkn"),-contains("0.05%")) |> 

  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,])"),"°C")) |>   #str_c wie paste, aber besser für NA
  
  relocate(any_of(c("temp","month")), .after = Batch) %>% 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month), as.numeric) %>%  
  relocate(any_of(c("temp","month")), .after = Batch) 
# wo sind HPLC Resultate für T0 und T3?? In FlexRep/QA33 nichts vorhanden, nicht einmal die Chromatogramme

spalten <- colnames(ACC1_L2)
# spalten <- gsub("\\.{3}\\d{1,3}","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
# spalten <- gsub(" \\(≥ 0.05%\\)","",spalten) #Assay entfernen
spalten <- gsub("Impurity","Unknown",spalten) #Imp. entfernen
spalten <- gsub("Unknown ","Unknown",spalten) #Imp. entfernen
names(ACC1_L2) <- spalten #neue, korrigierte Namen
# ACC1$inert[ACC1$`Inspection Lot`=="160000026252"] <- "not inert"

ACC1_L2$temp <- factor(ACC1_L2$temp, levels = c("-20°C","5°C"))
ACC1_L2$month <- as.integer(ACC1_L2$month)
ACC1_L2$month <-replace_na(ACC1_L2$month, 0) #Fehlende Monate gleich Initialwert
lev1 <-levels(droplevels(ACC1_L2$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....

ACC10_L2<-ACC1_L2 %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-c(temp)) %>%
  expand_grid(temp=lev1)


ACC1_L2 <- ACC1_L2[!ACC1_L2$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
ACC1_L2_fertig <- rbind(ACC1_L2,ACC10_L2)  # Kombinieren
ACC1_L2_fertig$month <-as.integer(ACC1_L2_fertig$month)

ACC1_L2 <-ACC1_L2_fertig

ACC1_L2 <- ACC1_L2 %>% 
  split.default(str_remove(names(.), "[\\.]{3}\\d+$")) %>%   #3 Punkten und einer Zahl zum Schluss
  map_dfc(~ exec(coalesce, !!!.x)) 


# ------- continued at: unknowns

#Korrektur (wird im SAP noch korrigiert)
#enb$`Assay Imp. enb-0159 minus C2H4`[enb$`Inspection Lot` == "890000148040"] <- 0.0
#enb$`Assay Imp. enb-0159 minus CH2`[enb$`Inspection Lot` == "890000148040"] <- 0.031

#------ Experiments

names(enb) %>% str_replace_all("[...]", " ")

alc_test <- enb %>% select(matches('nknown1[/\D]'))

dput(grep("RRT", names(enb_raw), value = TRUE))

subset(enb_raw, select = grep("^Unkn", names(enb_raw), value = TRUE))

test <- enb_raw %>% select(c("Lev"), grep("RRT U", names(enb_raw), value = TRUE)) 


test <- enb %>%  select(c(Batch,`Inspection Lot`, month, temp, grep("[Uu]nknown ?1", names(enb), value=T))) %>% 
  filter(str_detect(`Inspection Lot`, "890000148053")) 

