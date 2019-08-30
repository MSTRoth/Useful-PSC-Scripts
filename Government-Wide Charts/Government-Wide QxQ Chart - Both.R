### Quarter comparisons for Contract Obligations - Civilian/Defense Breakout####

library(colorspace)
library(RColorBrewer)
library(tidyverse)
options(scipen=999)


setwd("x:/1 Marielle Folder/Visualizations/Government-Wide")
data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Civilian and Defense Data by quarter.csv")

data$Year = as.character(data$Year)


data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year %in% 2016:2018 | (Year == 2019 #& `civ_def`== "Civilian" ###if first quarter
  )) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

#display.brewer.all()

data.civdef$civ_def <- c("Defense - Q2", "Civilian - Q3")  ### Change based on quarter

plotyr <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civdef
                          , Year != 2019
                          ), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 5, fontface = "bold")+   
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
  facet_grid(~civ_def, labeller = label_wrap_gen(20), scales = "free")+
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)", 
       title = "Contract Obligations Comparison FY16-FY19",    
       subtitle = NULL
       , caption = "Data Source: Bloomberg Government"
  ) +                                                                                                    
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 9, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))

plotyr

ggsave("Contract Obligations by Quarter- FY16-FY19Q3.jpg", plotyr,                  ###Change name 
       width = 15, height = 7, units = "in")