
#Water

ACC1_long %>% 
  # filter(str_detect(analy_param, "CMP-SA-PEG")) %>% 
  filter(analy_param =="Water content") %>% 
  filter(!is.na(value)) |> 

  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1, alpha=0.5)+
  # geom_hline(data=hline_dat, aes(yintercept=new_limit),linetype= 1, col = "red3", size=0.2)+
  # geom_hline(data=hline_dat2, aes(yintercept=new_limit), size=0)+
  geom_point(size = 1.5, alpha=0.9) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=2.049,linetype= 1, col = "red3", linewidth=0.4)+
  facet_grid( analy_param ~ .)+
  xlab("Months")+
  ylab("[% w/w]") +
  # ylim(c(75,90))+
  theme(strip.text.x = element_text(family="Merck",  size = 13, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#ffc832"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#2dbecd", "#a5cd50","#ff4d00"), name ="Storage conditions")+
  scale_fill_manual(values =c("#a2d2ff", "#fb8500"))+
  guides(fill = "none") #supress legend of aes fill (color areas)

ggsave("acc_water.png",dpi = 250,  last_plot(),units = "cm", width = 20, height = 8)



#Assay ALC-0366

ACC1_long %>% 
  # filter(str_detect(analy_param, "CMP-SA-PEG")) %>% 
  filter(analy_param =="Assay ALC-0366") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1, alpha=0.5)+
  # geom_hline(data=hline_dat, aes(yintercept=new_limit),linetype= 1, col = "red3", size=0.2)+
  # geom_hline(data=hline_dat2, aes(yintercept=new_limit), size=0)+
  geom_point(size = 1.5, alpha=0.9) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=89.5,linetype= 1, col = "red3", linewidth=0.4)+
  geom_hline(yintercept=110.49,linetype= 1, col = "red3", linewidth=0.4)+
  facet_grid( analy_param ~ .)+
  xlab("Months")+
  ylab("[% w/w]") +
  # ylim(c(75,90))+
  theme(strip.text.x = element_text(family="Merck",  size = 13, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#ffc832"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#2dbecd", "#a5cd50","#ff4d00"), name ="Storage conditions")+
  scale_fill_manual(values =c("#a2d2ff", "#fb8500"))+
  guides(fill = "none") #supress legend of aes fill (color areas)

ggsave("acc_assay.png",dpi = 250,  last_plot(),units = "cm", width = 20, height = 8)



#Sum of all impurities

ACC1_long %>% 
  # filter(str_detect(analy_param, "CMP-SA-PEG")) %>% 
  filter(analy_param =="Sum of all impurities") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1, alpha=0.5)+
  # geom_hline(data=hline_dat, aes(yintercept=new_limit),linetype= 1, col = "red3", size=0.2)+
  # geom_hline(data=hline_dat2, aes(yintercept=new_limit), size=0)+
  geom_point(size = 1.5, alpha=0.9) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=5.049,linetype= 1, col = "red3", linewidth=0.4)+
  facet_grid( .~ analy_param )+
  xlab("Months")+
  ylab("[%]") +
  ylim(c(0,5.1))+
  theme(strip.text.x = element_text(family="Merck",  size = 13, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#ffc832"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#2dbecd", "#a5cd50","#ff4d00"), name ="Storage conditions")+
  scale_fill_manual(values =c("#a2d2ff", "#fb8500"))+
  guides(fill = "none") #supress legend of aes fill (color areas)

ggsave("acc_sumImp.png",dpi = 250,  last_plot(),units = "cm", width = 20, height = 8)




#Purity ALC-0366

ACC1_long %>% 
  # filter(str_detect(analy_param, "CMP-SA-PEG")) %>% 
  filter(analy_param =="Purity") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1, alpha=0.5)+
  # geom_hline(data=hline_dat, aes(yintercept=new_limit),linetype= 1, col = "red3", size=0.2)+
  # geom_hline(data=hline_dat2, aes(yintercept=new_limit), size=0)+
  geom_point(size = 1.5, alpha=0.9) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=94.5,linetype= 1, col = "red3", linewidth=0.4)+
  facet_grid( analy_param ~ .)+
  xlab("Months")+
  ylab("[%]") +
  yl
im(c(94.5,100))+
  theme(strip.text.x = element_text(family="Merck",  size = 13, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#ffc832"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#2dbecd", "#a5cd50","#ff4d00"), name ="Storage conditions")+
  scale_fill_manual(values =c("#a2d2ff", "#fb8500"))+
  guides(fill = "none") #supress legend of aes fill (color areas)

ggsave("acc_purity.png",dpi = 250,  last_plot(),units = "cm", width = 20, height = 8)

