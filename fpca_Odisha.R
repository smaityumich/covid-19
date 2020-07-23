library(fdapace)
setwd('C:/Users/maity/OneDrive/Documents/covid/')
load('Odisha All Results.rda')

no_of_districts = length(Districts)
number_of_days = rep(0, no_of_districts)
for (i in 1:no_of_districts)
{
  number_of_days[i] = length(R.Curve[[i]]$date.R)
}
min_number_of_days = min(number_of_days)


mean_R = list()
time_R = list()
for (i in 1:no_of_districts)
{
  n = number_of_days[i]
  mean_R[[i]] <- R.Curve[[i]]$mean.R[(n-min_number_of_days+1):n]
  time_R[[i]] <- R.Curve[[i]]$date.R[(n-min_number_of_days+1):n]
}

# Look at https://cran.r-project.org/web/packages/fdapace/fdapace.pdf for further refs
opts = list(maxK = 4)
functional_pca = FPCA(mean_R, time_R, optns = opts)
png('fpca_diagnosis.png')
plot(functional_pca)
dev.off()
pca = data.frame('pca1' = functional_pca$xiEst[, 1], 'pca2' = functional_pca$xiEst[,2])
pca$Name.of.the.District = names(R.Curve)

## Regression of k-th score on 

# Load covariates 
Odisha_covar <- read.csv('Covariates.csv')
Odisha_covar$Name.of.the.District[1] <- 'Angul'
Odisha_covar$Name.of.the.District[29] <- 'Subarnapur'
Odisha_covar$Name.of.the.District[13] <- 'Jajpur'
Data = merge(pca, Odisha_covar, by = 'Name.of.the.District', all = TRUE)

## Regressions
library(glmnet)
library(plotmo) # for plot_glmnet

# Lasso
normalize = function(x){
  return (x - mean(x))/sd(x)
}
std_vars = apply(Data[, 5:33], 2, normalize)
std_vars = data.frame(std_vars)


png('trace-plots.png')
par(mfrow=c(2,1))
r1 = glmnet(as.matrix(std_vars), Data$pca1, alpha = 1, lambda = exp(seq(-2.2, 0.5, by = 0.01)))
plot_glmnet(r1, title = 'Trace plot for 1st PC')



r2 = glmnet(as.matrix(std_vars), Data$pca2, alpha = 1, lambda = exp(seq(-2.2, 0.5, by = 0.01)))
plot_glmnet(r2, title = 'Trace plot for 2nd PC')
dev.off()



# Selecting certain variables
library(dplyr)

selected_vars = cbind.data.frame(Data$SUB.DIVISIONAL.hospital.per.10k, Data$Total.Beds.Capacity.per.10k, 
                                 Data$Total.ICU.Beds.per.10K, Data$PHC.s.CHC.s.per.10k)
colnames(selected_vars) = c('SubDiv_hospital_per_10K', 'Total_bed_per_10K', 'Total_ICU_per_10K', 'PHC_CHC_per_10K')



normalize = function(x){
  return (x - mean(x))/sd(x)
}
std_vars = apply(selected_vars, 2, normalize)
std_vars = data.frame(std_vars)



r1 = lm(Data$pca1 ~ ., data = std_vars)
r2 = lm(Data$pca2 ~ ., data = std_vars)
coeffs1 = r1$coefficients
coeffs2 = r2$coefficients

subdiv_hospital = coeffs1[2] * functional_pca$phi[, 1] + coeffs2[2] * functional_pca$phi[, 2]
total_beds = coeffs1[3] * functional_pca$phi[, 1] + coeffs2[3] * functional_pca$phi[, 2]
total_icu = coeffs1[4] * functional_pca$phi[, 1] + coeffs2[4] * functional_pca$phi[, 2]
phc_chc = coeffs1[5] * functional_pca$phi[, 1] + coeffs2[5] * functional_pca$phi[, 2]
common_time = time_R[[1]]
png('Coeff-func-all.png')
plot(common_time, subdiv_hospital, type = 'l', ylim = c(-350, 1500),  xlab = 'Time', ylab = 'Effect over time')
lines(common_time, total_beds, lty = 1, col = 'red')
lines(common_time, total_icu, lty = 1, col = 'green')
lines(common_time, phc_chc, lty = 1, col = 'blue')
legend('topleft', legend=c("Sub-div hospital per 10K", "Total beds per 10K", 'Total ICU beds per 10K', 'PHC-CHC per 10K'),
       col=c("black", "red", 'green', 'blue'), lty = 1,  cex=0.8)
dev.off()

# Forest plot
library(jtools)
plot_summs(r1, r2)

