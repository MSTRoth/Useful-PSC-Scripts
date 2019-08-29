library(tidyverse)
library(RColorBrewer)


###Company Profile Charts -- Vendor Contract Transactions by Agency

#Location for saving charts
setwd("X:/1 Marielle Folder/Visualizations/Vendor Specific")

Vendor<- "Raytheon"  ###name of vendor exactly how it was saved
year_range <- "FY15-18"  ###year range to display

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/Vendor Specific/", Vendor," Company Profile.csv", sep = ""))



###Get top n agencyies by obligation
top_n_agencies <- data %>%
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
  dplyr::rename(fiscal_year = "Fiscal Year",
                funding_agency = "Funding Agency",
                transaction_value = "Transaction Value") %>%
  #filter(fiscal_year != 2019) %>%                                        ###whichever years to not include
  group_by(funding_agency) %>%
  dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
  arrange(desc(grand_total_transaction_value)) %>%
  top_n(5)

top_n_agencies <- top_n_agencies$funding_agency

###Process Data to get total transaction value by year

data.agency.year <- data %>%
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
  dplyr::rename(fiscal_year = "Fiscal Year",
                funding_agency = "Funding Agency",
                transaction_value = "Transaction Value") %>%
  #filter(fiscal_year != 2019) %>%                                      ###whichever years to not include
  filter(funding_agency %in% top_n_agencies) %>%
  dplyr::group_by(funding_agency, fiscal_year) %>%
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))  ###$$$ division

data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)
data.agency.year$facet = factor(data.agency.year$funding_agency, levels = c(top_n_agencies))

###Create Barplot and Save as JPG
plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
    scale_fill_manual("Fiscal Year", values = c("2015" = "steelblue1"
                                                ,"2016" = "orangered"
                                                ,"2017" = "grey70"
                                                , "2018" = "orange"
                                                #, "2019" = "olivedrab3"
                                                )) +
    facet_grid(~facet, labeller = label_wrap_gen(20))+
   # facet_wrap(~facet, labeller = label_wrap_gen(20), scales = "free")+                        ##use if want to change scales across agencies
    labs(x="Fiscal Year", y = "Contract Obligations (in) Millions",                             ##depends on $$ division
         title = paste(Vendor, " Contract Obligations by Agency ", year_range, sep = ""))+                            ##Vendor name and years
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank())




plot



ggsave(paste(Vendor, " Contract Obligations by Agency - ", year_range, ".jpg", sep = ""), plot,               ##Vendor Name
       width = 13, height = 6, units = "in")

