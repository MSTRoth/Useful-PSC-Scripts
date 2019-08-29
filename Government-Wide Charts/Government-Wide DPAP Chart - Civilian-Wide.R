####Total contract spending - services categories + products - DPAP####

## Government-Wide Total Contract Spending - Civilian-Wide data####

library(colorspace)
library(RColorBrewer)
library(tidyverse)
options(scipen=999)


setwd("X:/1 Marielle Folder/Visualizations/Government-Wide")
data <- read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP (services and total) Data - Government Wide.csv")

data$`DPAP Category` <- factor(data$`DPAP Category`,
                               levels = c("Products", "Construction Services",
                                          "Electronic & Communication Services",
                                          "Equipment Related Services",
                                          "Facility Related Services",
                                          "Knowledge Based Services",
                                          "Logistics Management Services",
                                          "Medical Services",
                                          "Research and Development",
                                          "Transportation Services"),
                               ordered = is.ordered(data$`DPAP Category`))

##Process

DPAP_civ <- data %>%
  filter(`DPAP Category` != "Total") %>% 
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$_billions_Civilian`))


label_height_civ <- DPAP_civ %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$_billions_Civilian`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_civ, by = c("Fiscal Year", "pors", "pors$") )

##Plot

plot_civ <- ggplot(label_height_civ, aes(x = `Fiscal Year`, y = `$_billions_Civilian`,
                                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_civ, `DPAP Category` %in% c("Medical Services", "Products")), aes(x=`Fiscal Year`, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = paste("Civilian", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 

##Save - Change to relevent year span when saving

ggsave(paste("Civilian", " Total Contract Spending Service Product ", "FY09-FY18.jpg", sep = ""), plot_civ,
       width = 13, height = 6.5, units = "in")