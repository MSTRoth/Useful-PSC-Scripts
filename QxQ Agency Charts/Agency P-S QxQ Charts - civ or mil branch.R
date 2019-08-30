#### Product vs Service - Quarter by Quarter Chart - Civilian or Military Branch####
library(tidyverse)
library(RColorBrewer)
options(scipen=999)

setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts/Product-Service")

Agency <- "USAID"
Year <- "FY17-19Q3"
dis_year <- "FY17-19Q3"

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", Agency," ", Year,".csv", sep = ""))
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")


data.organized <- data %>% 
  rename("PSC" = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         "transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year",
         "fiscal_quarter" = `Fiscal Quarter`) %>% 
  # filter(`Funding Bureau` == SubAgency)  %>%                                         #### if subset of Department/Agency 
  # filter(`Funding Office Level 6` == SubAgency) %>%                                         #### if subset of Department/Agency
  select(PSC, transaction_value, fiscal_quarter, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code","P.S")), by = c(PSC = "PSC Code")) %>% 
  filter(PSC != "UNKN") %>% 
  filter(fiscal_year %in% c(2017:2019)) %>% ###date range for chart
  select(transaction_value, fiscal_quarter, `P.S`) %>% 
  group_by(`P.S`, fiscal_quarter) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>% 
  mutate(total_obligations = round((sum)/1000000000, digits=2)) %>%           ###division of $$
  group_by(FY, `P.S`) %>% 
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations))%>% 
  mutate(FYYear = paste("FY", FY, sep = "")) %>% 
  mutate(Q_quarter = paste("Q", quarter, sep =""))



plot <- 
  ggplot(data.organized, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(data = subset(data.organized, total_obligations>(max(data.organized$label_y)/30)), aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.organized, 
                          FY != 2019 & 
                            total_obligations >(max(data.organized$label_y)/15)), aes(label = sprintf('%.0f%%', prop), 
                                                                                                                 y = label_y), size = 4, vjust = 3, fontface = "bold", check_overlap = T)+
  stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "Blues")[c(1,2,4,6)])+
  facet_wrap(~`P.S`, labeller = label_wrap_gen(20)#, scales = "free"
  )+
  labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
       title = paste(Agency, " Contract Obligations Comparison - ", dis_year, sep = ""),                   ##Change based on Agency and year/quarter
       caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))

plot

ggsave(filename = paste(Agency," Contract Obligations ", dis_year, "by quarter - P-S.jpg", sep = ""), plot,          ##Change Based on Agency and year/quarter
       width = 13, height = 6.5, units = "in")
