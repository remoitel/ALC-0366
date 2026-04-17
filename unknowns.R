library(plotly)
library(htmlwidgets)
detach("package:plotly", unload=TRUE)
library(ggrepel)

#------ unknowns ---

ACC1_uk <- ACC1_L2 %>% select(matches('Batch|method|temp|month|inert|Inspection Lot|Level Unk|RRT Unk')) %>% 
  select(-matches('\\.')) 


ACC1_uk_cols <- as.list(colnames(ACC1_uk))

# typeof(ACC1_uk_cols)

ACC1_uk_cols.num <- as.numeric(str_extract(ACC1_uk_cols, "[0-9]+")) #tidyverse
# ACC1_uk_cols.num <- as.numeric(gsub('\\D','', ACC1_uk_cols)) #base R

ACC1_uk_cols.cha <- str_extract(ACC1_uk_cols, "[aA-zZ% ]+") #tidyverse
# ACC1_uk_cols.cha <- gsub('[0-9]+','', ACC1_uk_cols) #base R
# gsub('\\d','', ACC1_uk_cols) #base R, similar

df_ACC1_uk_cols <- data.frame(x=ACC1_uk_cols.cha, y=ACC1_uk_cols.num)
df_ACC1_uk_cols[order(df_ACC1_uk_cols$y),]#base R
# df_ACC1_uk_cols %>% arrange(y) #tidyverse

col_vec <- df_ACC1_uk_cols %>% 
  arrange(y) %>% 
  unite(col_vec, c("x","y"), sep = "") %>% 
  pull(col_vec)

# (col_vec <- c(colnames(enb)[1:5], col_vec[6:20]))
col_vec <- str_replace(col_vec, "NA", "") #remove NA

ACC1_uk <- ACC1_L2[col_vec]

names(ACC1_uk) <- str_replace(names(ACC1_uk), "RRT Unknown", "RRT")
(names(ACC1_uk) <- str_replace(names(ACC1_uk), "Level Unknown", "Assay"))

ACC1_uk_long <- ACC1_uk %>%
  # filter(Batch == "ENB0003-XX") |> 
  # filter(temp != "-20°C") |> 
  # filter(inert==T) %>%
  pivot_longer(cols = matches('[0-9]$') ,
               names_to = c("param","Name", "No"), 
               names_pattern = "(\\D*)(.*?)(\\d{1,2})", #two capturing groups (), ? for non-greedy 
               values_to = "value") |> 
  pivot_wider(names_from = param, values_from = value) |> 
  unnest(cols = c("Assay", "RRT")) |> 
  mutate_at(vars(c("Assay","RRT")), as.numeric) %>% 
  
# mutate_at(vars(c("Area%","RRT")), as.numeric) %>% 

#------- add data from clipbboard

copdat <- read.delim("clipboard", sep="\t", header =T)

ACC1_uk_long2 <- copdat |>
  rename("Inspection Lot" = "Inspection.Lot") |> 
  mutate_at(vars("Inspection Lot"), as.character) |> 
  # mutate_at(vars(month), as.numeric) |> 
  mutate_at(vars(No), as.character) |> 
  filter(month <4) |> 
  mutate_at(vars(month), factor) |> 
  union(ACC1_uk_long )


#----------- Area% vs RRT -----------
 # p <- ggplotly(
  ACC1_uk_long2 %>% 
  drop_na(RRT) %>% 
  filter(RRT!=0) %>% 
  filter(Assay>0.05) %>% 
  # filter(!(Batch=="ENB0007-XX" & temp =="5°C")) %>% #not 5°C data available -> remove T0
  # filter(!(Batch=="ENB0009-ZX" & temp =="5°C"))  |>  #not 5°C data available -> remove T0

         # filter(inert == T) %>% 

  ggplot(aes(RRT, Assay, color=month, label = month))+
  # geom_label_repel( label.size = 0.1, box.padding = 0.15,
  #                   label.padding = 0.15, max.overlaps = Inf )+
  geom_point(size=1, alpha=0.8)+
  ylab("%")+
  # ylim(c(0,2.6))+

  facet_grid(Batch ~ temp)+
  scale_x_continuous(breaks = seq(0.2,1.8, 0.2),limits=c(0,1.85)) + 
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#ffc832"),
        axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 9, angle=0),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=8),
        legend.title =element_text(size=8),
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing = unit(0.4, "lines") )+
  scale_color_viridis_d( direction = -1,name ="Months") +
  guides(colour = guide_legend(nrow = 1))

ggsave("ACC1_uk.png", dpi=200, last_plot(),units = "cm", width = 18, height = 10)

saveWidget(p, "p1.html", selfcontained = F, libdir = "lib")
#-----------------
library(flextable)
library(janitor)

ACC1_uk_tab <- ACC1_L2  %>% 
  # filter(Lev=="1") %>% 
  drop_na("Level Unknown1") |> 
  # "2-MethylLMNGazole", "4-MethylLMNGazole", "1-MethoxymethylLMNGazole"  ) %>% 
  # filter(!str_detect(Batch,"038")) %>% 
  
  # filter(!(month == 0 & str_detect(`Inspection Lot`, "^89"))) %>% 
  # filter(!((Batch=="LMNG0016XX" |Batch=="LMNG0024XX") & temp=="40°C")) %>% 
  # rename("Individual unknown impurities" = "Individual ORC", "Sum of all impurities" = "Sum ORC + RC") %>% 
  # rename("5-MeTHFA-Ca water free" = "5-MeTHFA-Ca water free calc" ) %>% 
  # relocate("5-MeTHFA-Ca water free",  .after = "5-MeTHFA-Ca") %>% 
  arrange(Batch, temp , month) 
 

tab <- as_grouped_data(ACC1_uk_tab, groups = c("temp"))
tab <- flextable(tab)
tab <- set_header_labels(tab, temp = "Storage Condition", month = "Duration")
tab

save_as_docx(tab, path = "ACC1_uk_tab_tab.docx")

#--------
 p <-
  ACC1_uk_long2 |> 
  filter(month==0) |> 
  filter(temp=="-20°C") |>
  filter(Assay > 0.045) |> 
  arrange(desc(Assay)) |> 
  mutate_at(vars(No), as.numeric) |>
  mutate(id = row_number() ) |> 
  select(id, Assay, RRT) |> 
  pivot_wider(names_from = c(id), values_from = c(Assay, RRT), names_sort = TRUE)

p <-round_half_up(p, 4)
write.table(p, "clipboard", sep="\t", row.names=FALSE)
