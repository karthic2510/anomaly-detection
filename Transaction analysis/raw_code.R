#Load required libaries

library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
#library(lubridate)

#Load data 

ccd <- read.csv("purchase_credit_card.csv")

#Examine data
summary(ccd)

colnames(ccd) <- c("year_month", "agency_number", "agency_name", "ch_last_name",
                   "ch_first_initial", "description", 'amount', 'vendor', 
                   'transaction_date', 'posted_date', 'MCC')

colnames(ccd) #fix colnames into more standard R formats for easier syntax

#fix data types 

ccd$year_month <- as.factor(ccd$year_month)
#Converting year month into a factor to enable better graphs
ccd$agency_number <- as.factor(ccd$agency_number) 
ccd$vendor <- as.factor(ccd$vendor)
ccd$MCC <- as.factor(ccd$MCC)
ccd$transaction_date <- as.Date(ccd$transaction_date, format = "%m/%d/%Y %I:%M:%S %p")
ccd$posted_date <- as.Date(ccd$posted_date, format = "%m/%d/%Y %I:%M:%S %p")

unique(ccd$year_month)
#The data includes transactions from a 12-month period between July 2013 to June 2014.
length(unique(ccd$agency_number))
#116 unique agencies
length(unique(ccd$vendor)); length(unique(ccd$MCC));
#86279 unique vendors who fall into 435 unique categories


quantile(ccd$amount, c(0.001, 0.01, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999))
mean(ccd$amount)
sd(ccd$amount)
#While the range for transaction amount varies between -42,863 to 1,903,858.4
#98% of the transactions lie between the much smaller range of -166 to 4,298.568
#99.8% of the transactions lie between -1,261.482 to 21,925.972

#For all plots exploring distribution of transaction amount against another variables,
#we are filtering out extreme outliers to enable easier visual examination.
plot1 <- ccd %>% filter(amount > 0 & amount < 6000) %>% 
  ggplot(aes(x = year_month, y = amount)) + 
  geom_jitter(color = "skyblue", fill = 'skyblue') + theme_minimal() + 
  xlab("Year & Month") + ylab("Transaction Amount")
ggplotly(plot1)

#ggplotly(ggplot(ccd, aes( x = year_month, y = amount)) + geom_jitter() + theme_minimal())
#plot_ly(x = ccd$year_month, y = ccd$amount, type = 'scatter', mode = 'markers')
#Amount graphs do not provide much data, we can instead use means of these groups. 

#Histograms to see count across various variables 

plot5 <- ccd %>% group_by(agency_number) %>% mutate(counts = n()) %>%
  arrange(desc(counts)) %>%  ungroup() %>% 
  top_frac(0.2) %>% ggplot(aes(x = reorder(agency_name, -counts))) +
  geom_histogram(stat = 'count', fill = 'skyblue', color = 'skyblue') +
  theme_minimal() + theme(axis.text.x = element_blank()) + xlab("Agency") +
  ylab("Number of Transactions")
ggplotly(plot5)

plot6 <- ccd %>% group_by(vendor) %>% mutate(counts = n()) %>% arrange(desc(counts)) %>% 
  ungroup() %>% top_frac(0.2) %>%  ggplot(aes(x = reorder(vendor, -counts))) +
  geom_histogram(stat = 'count', fill = 'skyblue', color = 'skyblue') +
  theme_minimal() + theme(axis.text.x = element_blank()) + xlab("Vendor") +
  ylab("Number of Transactions")
ggplotly(plot6)

plot7 <- ccd %>% group_by(MCC) %>% mutate(counts = n()) %>% 
  arrange(desc(counts)) %>%  ungroup() %>% 
  top_frac(0.2) %>% ggplot(aes(x = reorder(MCC, -counts))) +
  geom_histogram(stat = 'count', fill = 'skyblue', color = 'skyblue') +
  theme_minimal() + theme(axis.text.x = element_blank()) + xlab("Merchant Category") +
  ylab("Number of Transactions")
ggplotly(plot7)

#histogram time to see progress
plot8 <- ccd %>% filter(amount > 0 & amount < 6000) %>% ggplot(aes(x = year_month)) +
  geom_histogram(stat = 'count', color = 'skyblue', fill = "skyblue") +
  theme_minimal() + xlab("Year & Month") + ylab("Number of Transactions")
ggplotly(plot8)

#Collating data by Agency

stat_agency <- ccd %>% group_by(agency_name) %>% 
  dplyr::summarise(count_agency_all = n(), total_amount_agency_all = sum(amount),
            avg_amount_agency_all = mean(amount),
            min_amount_agency_all = min(amount), 
            max_amount_agency_all = max(amount)) %>% 
  arrange(desc(total_amount_agency_all)) %>% ungroup()
datatable(stat_agency)

plot9 <- stat_agency %>% filter(avg_amount_agency_all < 150000) %>% 
  ggplot(aes(x = avg_amount_agency_all, y = count_agency_all,
             size = total_amount_agency_all,
             fill = agency_name, color = agency_name)) +
  geom_jitter() + 
  theme_minimal() + xlab("Average Transaction Amount") + ylab("Number of Transactions") +
  theme(legend.position = "none")
ggplotly(plot9)

stat_agency2 <- ccd %>% group_by(agency_name) %>% 
  arrange(agency_name, transaction_date) %>% 
  mutate(last_transaction = max(transaction_date)) %>%
  mutate(gap_transaction = last_transaction - transaction_date) %>% 
  mutate(lag_transaction = transaction_date - lag(transaction_date)) %>% 
  arrange(gap_transaction) %>% ungroup()
head(stat_agency2)

stat_agency_day <- stat_agency2 %>% filter(gap_transaction == 0) %>% group_by(agency_name) %>% 
  dplyr::summarise(count_agency_day = n(),
            total_amount_agency_day = sum(amount), 
            average_amount_agency_day = mean(amount),
            min_amount_agency_day = min(amount),
            max_amount_agency_day = max(amount),
            avg_lag_tnx_day = mean(lag_transaction),
            min_lag_tnx_day = min(lag_transaction)) %>% ungroup()

stat_agency_m <- stat_agency2 %>% filter(gap_transaction < 31) %>% group_by(agency_name) %>% 
  dplyr::summarise(count_agency_m = n(),
                   total_amount_agency_m = sum(amount), 
                   average_amount_agency_m = mean(amount),
                   min_amount_agency_m = min(amount),
                   max_amount_agency_m = max(amount),
                   avg_lag_tnx_m = mean(lag_transaction),
                   min_lag_tnx_m = min(lag_transaction)) %>% ungroup()

stat_agency_3m <- stat_agency2 %>% filter(gap_transaction < 91) %>% group_by(agency_name) %>% 
  dplyr::summarise(count_agency_3m = n(),
                   total_amount_agency_3m = sum(amount), 
                   average_amount_agency_3m = mean(amount),
                   min_amount_agency_3m = min(amount),
                   max_amount_agency_3m = max(amount),
                   avg_lag_tnx_3m = mean(lag_transaction),
                   min_lag_tnx_3m = min(lag_transaction)) %>% ungroup()

stat_agency <- left_join(stat_agency, stat_agency_day, by = 'agency_name')
stat_agency <- left_join(stat_agency, stat_agency_m, by = 'agency_name')
stat_agency <- left_join(stat_agency, stat_agency_3m, by = 'agency_name')
datatable(head(stat_agency))

#Collating data by Agency and merchant(vendor)

stat_agency_merchant <- ccd %>% group_by(agency_name, vendor) %>% 
  dplyr::summarise(count_merchant_all = n(), total_amount_merchant_all = sum(amount),
                   avg_amount_merchant_all = mean(amount),
                   min_amount_merchant_all = min(amount), 
                   max_amount_merchant_all = max(amount)) %>% 
  arrange(desc(total_amount_merchant_all)) %>% ungroup()

plot11 <- stat_agency_merchant %>% filter(avg_amount_merchant_all < 150000
                                          && agency_name == "OKLAHOMA STATE UNIVERSITY") %>% 
  ggplot(aes(x = avg_amount_merchant_all, y = count_merchant_all, size = total_amount_merchant_all,
             fill = vendor, color = vendor)) + geom_jitter() + 
  theme_minimal() + xlab("Average Transaction Amount") + ylab("Number of Transactions") +
  ggtitle("Transactions by vendor for Oklahoma State University") + theme(legend.position = "none")
ggplotly(plot11)

stat_agency_merchant2 <- ccd %>% group_by(agency_name, vendor) %>% 
  arrange(agency_name, vendor, transaction_date) %>%
  mutate(last_transaction = max(transaction_date)) %>% 
  mutate(gap_transaction = (last_transaction - transaction_date)) %>%
  mutate(lag_transaction = transaction_date - lag(transaction_date)) %>% 
  arrange(gap_transaction) %>% ungroup()
str(stat_agency_merchant2)

stat_agency_merchant_day <- stat_agency_merchant2 %>% filter(gap_transaction == 0) %>%
  group_by(agency_name, vendor) %>% 
  dplyr::summarise(count_agency_merchant_day = n(),
                   total_amount_agency_merchant_day = sum(amount), 
                   average_amount_agency_merchant_day = mean(amount),
                   min_amount_agency_merchant_day = min(amount),
                   max_amount_agency_merchant_day = max(amount),
                   avg_lag_tnx_merchant_day = mean(lag_transaction),
                   min_lag_tnx_merchant_day = min(lag_transaction)) %>% ungroup()


stat_agency_merchant_m <- stat_agency_merchant2 %>% filter(gap_transaction < 31) %>%
  group_by(agency_name, vendor) %>% 
  dplyr::summarise(count_agency_merchant_m = n(),
                   total_amount_agency_merchant_m = sum(amount), 
                   average_amount_agency_merchant_m = mean(amount),
                   min_amount_agency_merchant_m = min(amount),
                   max_amount_agency_merchant_m = max(amount),
                   avg_lag_tnx_merchant_m = mean(lag_transaction),
                   min_lag_tnx_merchant_m = min(lag_transaction)) %>% ungroup()

stat_agency_merchant_3m <- stat_agency_merchant2 %>% filter(gap_transaction < 91) %>%
  group_by(agency_name, vendor) %>% 
  dplyr::summarise(count_agency_merchant_3m = n(),
                   total_amount_agency_merchant_3m = sum(amount), 
                   average_amount_agency_merchant_3m = mean(amount),
                   min_amount_agency_merchant_3m = min(amount),
                   max_amount_agency_merchant_3m = max(amount),
                   avg_lag_tnx_merchant_3m = mean(lag_transaction),
                   min_lag_tnx_merchant_3m = min(lag_transaction)) %>% ungroup()

stat_agency_merchant <- left_join(stat_agency_merchant, stat_agency_merchant_day,
                                  by = c('agency_name' = 'agency_name', 'vendor' = 'vendor'))
stat_agency_merchant <- left_join(stat_agency_merchant, stat_agency_merchant_m,
                                  by = c('agency_name', 'vendor'))
stat_agency_merchant <- left_join(stat_agency_merchant, stat_agency_merchant_3m,
                                  by = c('agency_name', 'vendor'))
datatable(head(stat_agency_merchant))

#Collating data by Agency and merchant category

stat_agency_mcc <- ccd %>% group_by(agency_name, MCC) %>% 
  dplyr::summarise(count_mcc_all = n(), total_amount_mcc_all = sum(amount),
                   avg_amount_mcc_all = mean(amount),
                   min_amount_mcc_all = min(amount), 
                   max_amount_mcc_all = max(amount)) %>% 
  arrange(desc(total_amount_mcc_all)) %>% ungroup()

plot12 <- stat_agency_mcc %>% filter(avg_amount_mcc_all < 6000
                                          & agency_name == "OKLAHOMA STATE UNIVERSITY") %>% 
  ggplot(aes(x = avg_amount_mcc_all, y = count_mcc_all, size = total_amount_mcc_all,
             fill = MCC, color = MCC)) + geom_jitter() + 
  theme_minimal() + xlab("Average Transaction Amount") + ylab("Number of Transactions") +
  ggtitle("Transactions by MCC for Oklahoma State University") + theme(legend.position = "none")
ggplotly(plot12)

stat_agency_mcc2 <- ccd %>% group_by(agency_name, MCC) %>% 
  arrange(agency_name, MCC, transaction_date) %>%
  mutate(last_transaction = max(transaction_date)) %>% 
  mutate(gap_transaction = (last_transaction - transaction_date)) %>%
  mutate(lag_transaction = transaction_date - lag(transaction_date)) %>% 
  arrange(gap_transaction) %>% ungroup()
str(stat_agency_mcc2)

stat_agency_mcc_day <- stat_agency_mcc2 %>% filter(gap_transaction == 0) %>%
  group_by(agency_name, MCC) %>% 
  dplyr::summarise(count_agency_mcc_day = n(),
                   total_amount_agency_mcc_day = sum(amount), 
                   average_amount_agency_mcc_day = mean(amount),
                   min_amount_agency_mcc_day = min(amount),
                   max_amount_agency_mcc_day = max(amount),
                   avg_lag_tnx_mcc_day = mean(lag_transaction),
                   min_lag_tnx_mcc_day = min(lag_transaction)) %>% ungroup()


stat_agency_mcc_m <- stat_agency_mcc2 %>% filter(gap_transaction < 31) %>%
  group_by(agency_name, MCC) %>% 
  dplyr::summarise(count_agency_mcc_m = n(),
                   total_amount_agency_mcc_m = sum(amount), 
                   average_amount_agency_mcc_m = mean(amount),
                   min_amount_agency_mcc_m = min(amount),
                   max_amount_agency_mcc_m = max(amount),
                   avg_lag_tnx_mcc_m = mean(lag_transaction),
                   min_lag_tnx_mcc_m = min(lag_transaction)) %>% ungroup()

stat_agency_mcc_3m <- stat_agency_mcc2 %>% filter(gap_transaction < 91) %>%
  group_by(agency_name, MCC) %>% 
  dplyr::summarise(count_agency_mcc_3m = n(),
                   total_amount_agency_mcc_3m = sum(amount), 
                   average_amount_agency_mcc_3m = mean(amount),
                   min_amount_agency_mcc_3m = min(amount),
                   max_amount_agency_mcc_3m = max(amount),
                   avg_lag_tnx_mcc_3m = mean(lag_transaction),
                   min_lag_tnx_mcc_3m = min(lag_transaction)) %>% ungroup()

stat_agency_mcc <- left_join(stat_agency_mcc, stat_agency_mcc_day,
                                  by = c('agency_name' = 'agency_name', 'MCC' = 'MCC'))
stat_agency_mcc <- left_join(stat_agency_mcc, stat_agency_mcc_m,
                                  by = c('agency_name', 'MCC'))
stat_agency_mcc <- left_join(stat_agency_mcc, stat_agency_mcc_3m,
                                  by = c('agency_name', 'MCC'))
datatable(head(stat_agency_mcc, n = 25))

ccd %>% filter(amount > -100 & amount < 1000) %>% 
  ggplot(aes(x = year_month, y = amount)) + geom_boxplot() + theme_minimal() +
  xlab("Year and Month") + ylab("Amount") + ggtitle("Distribution of transaction amount across time")
#There are outliers in transaction amount which extend into very high numbers, however, 
#those transaction account for less than 2% of the data. In order to enable better visual
#examination, transaction amount is limited between - 100 and 1000. The majority of transactions
#lie between 0 and 500. But there is a long tail
#


#COMBINING ALL FACTORS INTO A SINGLE DATAFRAME

ccfactors <- left_join(ccd, stat_agency, by = c('agency_name'))
ccfactors <- left_join(ccfactors, stat_agency_merchant, by = c("agency_name", "vendor"))
ccfactors <- left_join(ccfactors, stat_agency_mcc, by = c("agency_name", "MCC"))


#Ratios to be used in models
#Adding lag parameters for ratio calculation
ccfactors <- ccfactors %>% group_by(agency_name, vendor) %>% 
  arrange(agency_name, vendor, transaction_date) %>% 
  mutate(lag_vendor = transaction_date - lag(transaction_date)) %>% ungroup()

ccfactors <- ccfactors %>% group_by(agency_name, MCC) %>% 
  arrange(agency_name, MCC, transaction_date) %>% 
  mutate(lag_mcc = transaction_date - lag(transaction_date)) %>% ungroup()

#Ratio 1 - current transaction lag over avg lag between tnxs for same vendor / MCC by agency

ccfactors$ratio_minlag_vendor <- ccfactors$lag_vendor/as.numeric(ccfactors$min_lag_tnx_merchant_3m)
ccfactors$ratio_minlag_MCC <- ccfactors$lag_mcc/ccfactors$min_lag_tnx_mcc_3m
ccfactors$ratio_avglag_vendor <- ccfactors$lag_vendor/ccfactors$avg_lag_tnx_merchant_3m
ccfactors$ratio_avglag_MCC <- ccfactors$lag_mcc/ccfactors$avg_lag_tnx_mcc_3m

#Ratio 2 - current transaction amount over avg transaction amount for same vendor/MCC by agency

ccfactors$ratio_amount_vendor <- ccfactors$amount/ccfactors$avg_amount_merchant_all
ccfactors$ratio_amount_vendorday <- ccfactors$amount/ccfactors$avg_amount_merchant_day
ccfactors$ratio_amount_vendorm <- ccfactors$amount/ccfactors$avg_amount_merchant_m
ccfactors$ratio_amount_vendor3m <- ccfactors$amount/ccfactors$avg_amount_merchant_3m
ccfactors$ratio_amount_mcc <- ccfactors$amount/ccfactors$avg_amount_mcc_all
ccfactors$ratio_amount_mccday <- ccfactors$amount/ccfactors$avg_amount_mcc_day
ccfactors$ratio_amount_mccm <- ccfactors$amount/ccfactors$avg_amount_mcc_m
ccfactors$ratio_amount_mcc3m <- ccfactors$amount/ccfactors$avg_amount_mcc_3m