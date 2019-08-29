#####DoD####


library(tidyverse)
library(RColorBrewer)
options(scipen=999)


setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts") 

data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Civilian and Defense Data by quarter.csv")

Agency <- "DoD"
Year <- "FY17-19Q2"
data$Year = as.character(data$Year)


data.civdef_def <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(civ_def == "Defense") %>%             ## if only civ or def
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_def$Year = as.character(data.civdef_def$Year)

data.def <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  filter(civ_def == "Defense") %>%              ## if only civ or def
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year %in% c(2017:2019)) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))


plotdef <- ggplot(data.def, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.def
                          , Year != 2019
                          )
            , aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
       title = paste(Agency, " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
       caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption                                                                                                    ##Change Depending on Year/Q
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))


plotdef


ggsave(filename = paste(Agency," Contract Obligations ", Year, "by quarter.jpg", sep = ""), plotdef,          ##Change Based on Agency and year/quarter
       width = 13, height = 6.5, units = "in")