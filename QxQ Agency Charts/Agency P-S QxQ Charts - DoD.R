#### Product vs Service - Quarter by Quarter Chart - Defense####

setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts/Product-Service")
data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Defense Serv-Prod Data by quarter.csv")

data$Year = as.character(data$Year)

data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(Year %in% 2017:2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(Year %in% 2017:2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))



plot <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civdef, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 5, fontface = "bold")+   ####Adds total to top
  #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
  # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
  #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
  #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
  # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
  #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "Blues")[c(1,2,4,6)])+
  facet_grid(~civ_def, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
       title = "Defense Contract Obligations Comparison FY16-FY19",
       subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))




ggsave("Defense Contract Obligations by Quarter - FY17-FY19 - P-S.jpg", plot,                ######
       width = 13, height = 6.5, units = "in")
