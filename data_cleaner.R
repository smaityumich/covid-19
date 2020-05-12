library(dplyr)
path = 'C:/Users/maity/OneDrive/Documents/covid/'
raw_data1 = read.csv(file = paste(path, 'raw_data1.csv', sep = ''))
raw_data2 = read.csv(file = paste(path, 'raw_data2.csv', sep = ''))
raw_data3 = read.csv(file = paste(path, 'raw_data3.csv', sep = ''))

raw_data1 = raw_data1 %>% select(Date.Announced, Detected.District, Detected.State, Current.Status)
raw_data2 = raw_data2 %>% select(Date.Announced, Detected.District, Detected.State, Current.Status)
raw_data3 = raw_data3 %>% select(Date.Announced, Detected.District, Detected.State, Current.Status, Num.Cases)

indore_raw_data1 = raw_data1[raw_data1$Detected.District == 'Indore',]
#indore_raw_data1 = indore_raw_data1[indore_raw_data1$Current.Status == 'Hospitalized',]
indore_raw_data2 = raw_data2[raw_data2$Detected.District == 'Indore',]
#indore_raw_data2 = indore_raw_data2[indore_raw_data2$Current.Status == 'Hospitalized',]
indore_raw_data3 = raw_data3[raw_data3$Detected.District == 'Indore',]
#indore_raw_data3 = indore_raw_data3[indore_raw_data3$Current.Status == 'Hospitalized',]
indore_raw_data1$Num.Cases = 1
indore_raw_data2$Num.Cases = 1

date_wise_num_cases1 = aggregate(indore_raw_data1$Num.Cases, by=list(Category=indore_raw_data1$Date.Announced), FUN=sum)
date_wise_num_cases2 = aggregate(indore_raw_data2$Num.Cases, by=list(Category=indore_raw_data2$Date.Announced), FUN=sum)
date_wise_num_cases3 = aggregate(indore_raw_data3$Num.Cases, by=list(Category=indore_raw_data3$Date.Announced), FUN=sum)
rm('indore_raw_data1', 'indore_raw_data2', 'indore_raw_data3')


indore_counts = rbind(date_wise_num_cases1, date_wise_num_cases2, date_wise_num_cases3)
rm('date_wise_num_cases1', 'date_wise_num_cases2', 'date_wise_num_cases3')
colnames(indore_counts) = c('Date', 'Counts')
indore_counts$Date = as.character.Date(indore_counts$Date, tryFormats = '%d/%m/%Y')
indore_counts$Date = as.Date(indore_counts$Date, format = '%d/%m/%Y')
indore_counts = indore_counts[order(indore_counts$Date),]
all_dates = seq.POSIXt(from = as.POSIXct(indore_counts$Date[1]), to = as.POSIXct(tail(indore_counts$Date, n = 1)), by = "1 days")
all_dates = as.Date(all_dates, format = '%Y/%m/%d')
all_dates = data.frame(all_dates)
colnames(all_dates) = c('Date')
indore_counts = merge(indore_counts, all_dates, by = 'Date', all.y = TRUE)
indore_counts[is.na(indore_counts)] = 0

rm('raw_data1', 'raw_data2', 'raw_data3', 'all_dates')
indore_counts$Cumulative.Counts = cumsum(indore_counts$Counts)

library(EpiEstim)
R_estimate <- estimate_R(indore_counts$Counts,  method='parametric_si', config=list(t_start=2:42, 
                                          t_end=5:45, n1 = 500, mean_si = 3.96, std_si = 4.75,  
                                          n2 = 100, seed = 1, mcmc_control=list(init.pars=NULL, 
                                           burnin=10000, thin=500, seed=1)))
pdf(paste(path, 'R_indore_smaity.pdf'))
plot(R_estimate)
dev.off()

plot(R_estimate)

save.image(paste(path, 'R_indore_smaity.rda'))
