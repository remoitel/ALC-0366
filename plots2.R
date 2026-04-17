# Impurities: Mefox -----

LMCG_long %>% 
  filter(analy_param == "Mefox") %>%  
  # filter(temp == "5°C" | temp == "25°C"  ) %>% 
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Mefox %(w/w)")+
  # facet_grid( ~ Batch )+
  facet_grid(inert~Batch)+
  #facet_grid( analy_param ~ Batch)+
  # ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 17, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), linewidth =2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 11, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("LMCG_Mefox.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
# ggsave("LMCG_Mefox.png",device = "png", last_plot(),units = "cm", width = 20, height = 15)

# Impurities: HOMeTHFA -----
LMCG_long %>% 
  filter(analy_param == "HOMeTHFA") %>%  
  # filter(temp == "5°C" | temp == "25°C"  ) %>% 
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("HOMeTHFA %(w/w)")+
  facet_grid(inert~Batch)+
  #facet_grid( analy_param ~ Batch)+
  # ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 6, linetype= "dashed", col = "red3", size=1)+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  geom_hline(yintercept = 0.5, linetype= 3, col = "orange3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), linewidth =2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 11, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("LMCG_HOMe.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
# ggsave("LMCG_HOMe.png",device = "png", last_plot(),units = "cm", width = 20, height = 15)

# Impurities: 5-MeTHFA-Ca -----

LMCG_long %>% 
  filter(analy_param == "5-MeTHFA-Ca") %>%  
  # filter(temp == "5°C" | temp == "25°C"  ) %>% 
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("5-MeTHFA-Ca %(w/w)")+
  facet_grid(inert~Batch)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(85,100))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 95, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 102, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), linewidth =2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 11, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("LMCG_assay.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
# ggsave("LMCG_assay.png",device = "png", last_plot(),units = "cm", width = 20, height = 15)


# Impurities: several -----

LMCG_long %>% 
  filter(Batch =="LMCG0936XX") |> 
  filter(!( analy_param == "rRt 1.16")) %>%  
  filter(temp == "5°C" ) %>% 
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Concentration [%]")+
  facet_wrap(analy_param~., scales="free")+
  #facet_grid( analy_param ~ Batch)+
  # ylim(c(85,100))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 95, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 102, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=inert), size=2)+
  geom_line(aes(col=inert), linewidth =2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 11, colour = "#503291"),
        legend.text = element_text(size=10), legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#a5cd50", "#eb3c96"), name ="@5°C Storage Condition: ")

ggsave("LMCG_imp.svg",device = "svg", last_plot(),units = "cm", width = 30, height = 15)
# ggsave("LMCG_assay.png",device = "png", last_plot(),units = "cm", width = 20, height = 15)
