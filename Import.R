ACC1_raw <- read_excel("ACC0001-XX_Y1.xlsx")

#Überflüssige Spalten
wegdamit <- c("Lev", "Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time") 


ACC1 <- ACC1_raw %>%
  filter(Lev==1) %>% 
  filter(!is.na(`UD code`) )%>% 
  select(-any_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC '),-contains("No QC")) %>% 
  select(-contains("ICP-")) %>%  #ohne ICP-Resultate   
  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,])"),"°C")) |>   #str_c wie paste, aber besser für NA

  # filter(!is.na("UD code"))%>%  # Leere UD -> entfernen

  relocate(any_of(c("temp","month")), .after = Batch) %>% 
  filter(!(str_detect(`IL short text`, "^IP[KM]") & !is.na(`IL short text`))) %>% #also allow na
  # filter(!(str_detect(`Inspection Lot`, "^89") & is.na(temp)) ) %>%
# select(c("Lev","Batch","month","temp","inert","Inspection Lot", "IL short text","pH" )) %>% 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month), as.numeric) %>%  
  relocate(any_of(c("temp","month")), .after = Batch) |> 
  mutate(Purity = coalesce(`Purity ALC-0366...144`,`Purity ALC-0366...288`) ,.keep = "unused") |> 
  mutate("Sum of all impurities" = coalesce(`Sum of all impurities...284`,`Sum of all impurities...148`) ,.keep = "unused") |> 
  select("Batch", "temp", "month", "Inspection Lot", "IL short text", 
          "Appearance color", "Appearance texture",  "Water content",  "Purity", "Sum of all impurities", 
         "Assay ALC-0366", "Microbial Count (TAMC)", "Microbial Count (TYMC)", 
         "Level Unknown 1", "Level Unknown 2", "Level Unknown 3", "Level Unknown 4", "Level Unknown 5", "Level Unknown 6", 
         "Level Unknown 7", "Level Unknown 8", "Level Unknown 9", "Level Unknown 10", 
         "Level Impurity11", "Level Impurity12", "Level Impurity13", "Level Impurity14", 
         "Level Impurity15", "Level Impurity16", "Level Impurity17", "Level Impurity18", 
         "Level Impurity19", "Level Impurity20"
         )
  

# ACC1$inert[ACC1$`Inspection Lot`=="160000026252"] <- "not inert"

ACC1$temp <- factor(ACC1$temp, levels = c("-20°C","5°C"))

ACC1$month <- as.integer(ACC1$month)

ACC1$month <-replace_na(ACC1$month, 0) #Fehlende Monate gleich Initialwert

lev1 <-levels(droplevels(ACC1$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....

ACC10<-ACC1 %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-c(temp)) %>%
  expand_grid(temp=lev1)


ACC1 <- ACC1[!ACC1$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
ACC1_fertig <- rbind(ACC1,ACC10)  # Kombinieren
ACC1_fertig$month <-as.integer(ACC1_fertig$month)

ACC1 <-ACC1_fertig


spalten <- colnames(ACC1)
spalten <- gsub("\\.{3}\\d{1,3}","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
# spalten <- gsub("\\Assay ?","",spalten) #Assay entfernen
# spalten <- gsub("\\Imp. ","",spalten) #Imp. entfernen
names(ACC1_selection) <- spalten #neue, korrigierte Namen

ACC1_long<- ACC1 %>% 
  # select(1:26) %>% # ohne MiBi
   select(-c("Inspection Lot","IL short text")) %>% 
  pivot_longer(!c(Batch, month,temp), names_to = "analy_param", values_to = "value") |>     # alle zu long ausser months und temp
  mutate_at(vars(value), as.numeric)

