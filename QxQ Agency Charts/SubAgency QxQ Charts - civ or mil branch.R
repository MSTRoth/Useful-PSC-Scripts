###Subagency Charts (all as list)####

library(tidyverse)
library(RColorBrewer)
options(scipen=999)

setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts") 

Agency <- "DOJ"
Year <- "FY17-19Q3"
dis_year <- "FY17-19Q3" ###displayed year (in case different from file name)

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", Agency," ", Year,".csv", sep = ""))

Subagency <- as.list(unique(data$`Funding Bureau`))

data.organized<- lapply(Subagency, function(x){
  data.organized <- data %>%
    filter(`Funding Bureau` == x) %>% 
    rename("transaction_value" = `Transaction Value`,
           "fiscal_quarter" = `Fiscal Quarter`) %>%
    filter(`Fiscal Year` %in% c(2017:2019)) %>%                               ###date range for chart
    select(transaction_value, fiscal_quarter, `Funding Bureau`) %>%
    group_by(fiscal_quarter, `Funding Bureau`) %>%
    summarise(sum = sum(transaction_value)) %>% 
    separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>%
    mutate(total_obligations = round((sum)/1000000, digits=2)) %>%           ###division of $$
    group_by(FY, `Funding Bureau`) %>%
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations))%>%
    mutate(FYYear = paste("FY", FY, sep = "")) %>%
    mutate(Q_quarter = paste("Q", quarter, sep =""))
  
})

plot<- lapply(data.organized, function(a){
  plot <-
    ggplot(a, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(a
                            , FY != 2019
                            ), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    #facet_grid(~agency_comp, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Millions",                                            ##Change based on $$ division
         title = paste(Agency, " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
         subtitle = a$`Funding Bureau`[1],
         caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          plot.caption = element_text(size = 8, face = "italic"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20),
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
})

plot

lapply(1:length(Subagency), function(x) {
  ggsave(filename = paste(Agency, " - ", Subagency[[x]]," Contract Obligations", dis_year, " by quarter.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 6.5, units = "in")
})