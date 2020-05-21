library(tidyverse);library(quantmod);library(ggplot2);library(foreach);library(ggrepel)

vanguard_sectors = c("VOX","VCR","VDC","VDE","VFH","VHT","VIS","VGT","VAW","VNQ","VPU")

get_stocks = function(stocks,from,to){
  
  .get_stocks = function(stock){
    
    dxtf <- getSymbols(stock, src = "yahoo", from = from, to = to, auto.assign = FALSE) %>% 
      data.frame() %>% rownames_to_column("date") %>% 
      `colnames<-`(tolower(gsub(paste0(stock,"\\."),"",colnames(.)))) %>%
      mutate(stock = stock) %>%
      mutate(period_open = ifelse(date==from,open,NA)) %>%
      fill(period_open) %>%
      mutate(change = round(adjusted - period_open,2),
             percent_change = round(100*change/open,2))
    
    return(dxtf)
  }
  
  out <- foreach(i=stocks) %do%
    .get_stocks(i) %>% 
    bind_rows %>%
    mutate(date = as.Date(date))
  
  return(out)
}

df <- get_stocks(vanguard_sectors,"2020-03-09","2020-05-20") %>%
  mutate(label = ifelse(date == max(date),stock,NA))

ggplot(df,aes(x=date,y=percent_change,color=stock)) +
  geom_line(size=1) +
  ggtitle("Vanguard Sectors") + xlab("Date") + ylab("Percent Change") +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_label_repel(aes(label = label),nudge_x = 1,na.rm = TRUE)
