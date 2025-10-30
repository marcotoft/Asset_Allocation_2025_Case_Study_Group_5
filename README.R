# Asset_Allocation_2025_Case_Study_Group_5

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# o ------------------------------------------------------------------------ o ####
# o ------------------ ASSET ALLOCATION - 2025 CASE STUDY ------------------ o ####
# o ------------------------------------------------------------------------ o ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### ----------------------------------- ###
### --- CODE FOR PLOTS NOT INCLUDED --- ###
### ----------------------------------- ###

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
# To correlation:
library(lattice)
library(hexbin)
# Mean-variance optimize:
library(quadprog)
library(gridExtra)

## IMPORT DATA ####
partition_data <- function(data) {
  # Step 1: Convert all columns to numeric
  data[] <- lapply(data, function(x) as.numeric(as.character(x)))
  
  # Step 2: Identify rows with missing values
  na_rows <- which(!complete.cases(data))
  na_rows <- sort(na_rows)
  
  # Step 3: Define boundaries for clean segments
  boundaries <- c(0, na_rows, nrow(data) + 1)
  
  # Step 4: Create list of clean subsets
  subsets <- list()
  for (i in seq(1, length(boundaries) - 1, by = 2)) {
    start <- boundaries[i] + 1
    end <- boundaries[i + 1] - 1
    if (start <= end) {
      subsets[[paste0("data", (i + 1) / 2)]] <- data[start:end, ]
    }
  }
  return(subsets)
}

### Load Fama and French ####
F_F_Research_Data_Factors_2 <- read_csv("Desktop/F-F_Research_Data_Factors 2.csv")
FFfactorsAnnual <- F_F_Research_Data_Factors_2

### Load 6 Portfolios ####
port6_AVWRmon <- read_csv("Desktop/6_Port_A.csv")
port6_AEWRmon <- read_csv("Desktop/6_Port_B.csv")
port6_AVWRann <- read_csv("Desktop/6_Port_C.csv")
port6_AEWRann <- read_csv("Desktop/6_Port_D.csv")
port6_NFirms <- read_csv("Desktop/6_Port_E.csv")
port6_AMarketCap <- read_csv("Desktop/6_Port_F.csv")
port6_EWAPriorRmon <- read_csv("Desktop/6_Port_G.csv")
port6_VWAPriorRmon <- read_csv("Desktop/6_Port_H.csv")

port6_AVWRmon <- port6_AVWRmon %>% 
  rename(
    month = ...1,
    SMALL_LoPRIOR = `SMALL LoPRIOR`,
    ME1_PRIOR2 = `ME1 PRIOR2`,
    SMALL_HiPRIOR = `SMALL HiPRIOR`,
    BIG_LoPRIOR = `BIG LoPRIOR`,
    ME2_PRIOR2 = `ME2 PRIOR2`,
    BIG_HiPRIOR = `BIG HiPRIOR`
  )

port6_NFirms <- port6_NFirms %>% 
  rename(
    month = ...1,
    SMALL_LoPRIOR_Nfirms = `SMALL LoPRIOR`,
    ME1_PRIOR2_Nfirms = `ME1 PRIOR2`,
    SMALL_HiPRIOR_Nfirms = `SMALL HiPRIOR`,
    BIG_LoPRIOR_Nfirms = `BIG LoPRIOR`,
    ME2_PRIOR2_Nfirms = `ME2 PRIOR2`,
    BIG_HiPRIOR_Nfirms = `BIG HiPRIOR`
  )

port6_AMarketCap <- port6_AMarketCap %>% 
  rename(
    month = ...1,
    SMALL_LoPRIOR_AMarketCap = `SMALL LoPRIOR`,
    ME1_PRIOR2_AMarketCap = `ME1 PRIOR2`,
    SMALL_HiPRIOR_AMarketCap = `SMALL HiPRIOR`,
    BIG_LoPRIOR_AMarketCap = `BIG LoPRIOR`,
    ME2_PRIOR2_AMarketCap = `ME2 PRIOR2`,
    BIG_HiPRIOR_AMarketCap = `BIG HiPRIOR`
  )


### Load 25 portfolios ####
port25 <- read.csv("Desktop/25_Portfolios_ME_Prior_12_2.csv", skip = 11, header = TRUE)
port25 <- partition_data(port25)

# port25_SaM_A <- port25$data1
port25_AVWRmon <- port25$data1
# port25_SaM_B <- port25$data2
# port25_SaM_C <- port25$data3
# port25_SaM_D <- port25$data4
# port25_SaM_E <- port25$data5
port25_NFirms <- port25$data5
# port25_SaM_F <- port25$data6
port25_AMarketCap <- port25$data6
# port25_SaM_G <- port25$data7
# port25_SaM_H <- port25$data8

port25_AVWRmon <- port25_AVWRmon %>% rename(month = X)
port25_NFirms <- port25_NFirms %>% rename(month = X)
port25_AMarketCap <- port25_AMarketCap %>% rename(month = X)

### Load 6 european portfolios ####
port6Euro <- read.csv("Desktop/Europe_6_Portfolios_ME_Prior_12_2.csv", skip = 12, header = TRUE)
port6Euro <- partition_data(port6Euro)

# port6Euro_SaM_A <- port6Euro$data1
port6Euro_AVWRmon <- port6Euro$data1
# port6Euro_SaM_B <- port6Euro$data2
# port6Euro_SaM_C <- port6Euro$data3
# port6Euro_SaM_D <- port6Euro$data4
# port6Euro_SaM_E <- port6Euro$data5
port6Euro_NFirms <- port6Euro$data5
# port6Euro_SaM_F <- port6Euro$data6
port6Euro_AMarketCap <- port6Euro$data6

port6Euro_AVWRmon <- port6Euro_AVWRmon %>% 
  rename(
    month = X#,
    # SMALL_LoPRIOR = SMALL.LoPRIOR,
    # ME1_PRIOR2 = `ME1 PRIOR2`,
    # SMALL_HiPRIOR = `SMALL HiPRIOR`,
    # BIG_LoPRIOR = `BIG LoPRIOR`,
    # ME2_PRIOR2 = `ME2 PRIOR2`,
    # BIG_HiPRIOR = `BIG HiPRIOR`
  )

port6Euro_NFirms <- port6Euro_NFirms %>% 
  rename(month = X)

port6Euro_AMarketCap <- port6Euro_AMarketCap %>% 
  rename(month = X)

### Load 25 european portfolios ####
port25Euro <- read.csv("Desktop/Europe_25_Portfolios_ME_Prior_12_2.csv", skip = 12, header = TRUE)
port25Euro <- partition_data(port25Euro)

# port25Euro_SaM_A <- port25Euro$data1
port25Euro_AVWRmon <- port25Euro$data1
# port25Euro_SaM_B <- port25Euro$data2
# port25Euro_SaM_C <- port25Euro$data3
# port25Euro_SaM_D <- port25Euro$data4
# port25Euro_SaM_E <- port25Euro$data5
port25Euro_NFirms <- port25Euro$data5
# port25Euro_SaM_F <- port25Euro$data6
port25Euro_AMarketCap <- port25Euro$data6

port25Euro_AVWRmon <- port25Euro_AVWRmon %>% rename(month = X)
port25Euro_NFirms <- port25Euro_NFirms %>% rename(month = X)
port25Euro_AMarketCap <- port25Euro_AMarketCap %>% rename(month = X)

#### Load Time-Series data ####
ExchangeRateData <- read_csv("Desktop/eurofxref-hist.csv") %>% select(c("Date", "USD"))
# Turn Daily Exchange Rates into Monthly Exchange Rates
ExchangeRateDataMonthly <- ExchangeRateData %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(
    USD_first = USD[which.min(Date)],
    USD_last  = USD[which.max(Date)]
  ) %>%
  mutate(
    MonthlyReturnHold1USD = USD_first / USD_last - 1,
    month = format(month, "%Y%m")   # "YYYYMM" as string
  )
ExchangeRateDataMonthly

#### Load Yield Curves [Current_year - euro area] ####
data <- read_csv("Desktop/data.csv")
data <- data.frame(data)
head(data[8])
data[[8]] <- as.factor(data[[8]])

current_year <- data %>% select(c("KEY", "DATA_TYPE_FM", "TIME_PERIOD", "OBS_VALUE")) %>% 
  filter(DATA_TYPE_FM %in% c("BETA0","BETA1","BETA2","BETA3","TAU1","TAU2"))

current_yearMonthly <- current_year %>% 
  group_by(month = floor_date(TIME_PERIOD, "month")) %>% 
  filter(TIME_PERIOD == min(TIME_PERIOD)) %>%
  ungroup() %>%
  select(-month) %>%
  rename(first_in_month = TIME_PERIOD) %>% 
  mutate(month = as.character(format(first_in_month, "%Y%m")))

#### Load Yield Curves [All_years - euro area] ####
data1 <- read_csv("Desktop/data (1).csv")
sapply(data1, "class")

all_years <- data1 %>% select(c("KEY", "DATA_TYPE_FM", "TIME_PERIOD", "OBS_VALUE")) %>% 
  filter(DATA_TYPE_FM %in% c("BETA0","BETA1","BETA2","BETA3","TAU1","TAU2"))

all_yearsMonthly <- all_years %>% 
  group_by(month = floor_date(TIME_PERIOD, "month")) %>% 
  filter(TIME_PERIOD == min(TIME_PERIOD)) %>%
  ungroup() %>%
  select(-month) %>%
  rename(first_in_month = TIME_PERIOD) %>% 
  mutate(month = as.character(format(first_in_month, "%Y%m")))

all_yearsMonthly[[2]] <- as.factor(all_yearsMonthly[[2]])
sapply(all_yearsMonthly, "class")

params_wide1 <- all_yearsMonthly %>%
  pivot_wider(
    names_from = DATA_TYPE_FM,
    values_from = OBS_VALUE
  ) %>% select(-c(KEY,first_in_month)) %>% group_by(month) #change here for first/last
collapsed1 <- params_wide1 %>%
  pivot_longer(
    cols = -month,
    names_to = "param",
    values_to = "value"
  ) %>%
  group_by(month, param) %>%
  summarise(value = first(na.omit(value)), .groups = "drop") %>%  # first non-NA per month/param
  pivot_wider(names_from = param, values_from = value)

# Define a function to calculate the spot-rate
fctSpot_rate <- function(TTM, beta0, beta1, beta2, beta3, tau1, tau2){
  temp1 <- (1-exp(-TTM/tau1))/(TTM/tau1)
  temp2 <- -exp(-TTM/tau1)
  temp3 <- (1-exp(-TTM/tau2))/(TTM/tau2)
  temp4 <- -exp(-TTM/tau2)
  return(beta0+beta1*temp1+beta2*temp1+beta2*temp2+beta3*temp3+beta3*temp4)
}

# Calculate the spot-rate/ZC-rate and convert it into a EURO RF-rate. 
# We have used the parameters first date in th month. The estimated parameters are in % so must convert to decimals
collapsed1 <- mutate(collapsed1, RF_rate = (exp(fctSpot_rate(1/12, collapsed1$BETA0,collapsed1$BETA1,collapsed1$BETA2,collapsed1$BETA3,collapsed1$TAU1,collapsed1$TAU2)/(12*100))-1)*100)

## THERE ARE NO NA-VALUES IN THE DATASETS ##

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# o ------------------------------------------------------------------------ o ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

## 1) FACTOR DESIGN #####
### a) #####
#### i) #####
port6_AVWRmon20 <- port6_AVWRmon
port6_AVWRmon20 <- port6_AVWRmon %>%
  filter(month >= 200409 & month <= 202412)
FFfactorsMonthly20 <- FFfactorsMonthly %>%
  filter(month >= 200409 & month <= 202412)

# Calculate Factors ourselves in US
# Calculate SMB by hand instead of FF
port6_AVWRmon20 <- port6_AVWRmon20 %>% mutate(SMB = (SMALL_LoPRIOR + ME1_PRIOR2 + SMALL_HiPRIOR)/3 - (BIG_LoPRIOR + ME2_PRIOR2 + BIG_HiPRIOR)/3)
port6_AVWRmon20 <- port6_AVWRmon20 %>% mutate(MOM = (SMALL_HiPRIOR + BIG_HiPRIOR)/2 - (SMALL_LoPRIOR + BIG_LoPRIOR)/2)
port6_AVWRmon20 <- port6_AVWRmon20 %>% mutate(RF = FFfactorsMonthly20[[5]])

port6_AVWRmon20 <- merge(port6_AVWRmon20, port6_NFirms, by = "month", all.x = T, suffixes = c("", "_NFirms"))
port6_AVWRmon20 <- merge(port6_AVWRmon20, port6_AMarketCap, by = "month", all.x = T, suffixes = c("", "_AMarketCap"))

port6_AVWRmon20 <- mutate(port6_AVWRmon20, 
                          "numerator" = SMALL_LoPRIOR*SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap+
                            ME1_PRIOR2*ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap+
                            SMALL_HiPRIOR*SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap+
                            BIG_LoPRIOR*BIG_LoPRIOR_Nfirms*BIG_LoPRIOR_AMarketCap+
                            ME2_PRIOR2*ME2_PRIOR2_Nfirms*ME2_PRIOR2_AMarketCap+
                            BIG_HiPRIOR*BIG_HiPRIOR_Nfirms*BIG_HiPRIOR_AMarketCap,
                          "denominator" = SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap+
                            ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap+
                            SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap+
                            BIG_LoPRIOR_Nfirms*BIG_LoPRIOR_AMarketCap+
                            ME2_PRIOR2_Nfirms*ME2_PRIOR2_AMarketCap+
                            BIG_HiPRIOR_Nfirms*BIG_HiPRIOR_AMarketCap,
                          MKT = numerator/denominator - RF) %>% select(-c(numerator,denominator)) # MKT is RF

#### ii) #####
# Calculate factors and from USD->EURO, 6portfolio
port6USEURO_AVWRmon20 <- port6_AVWRmon20[,1:7]
port6USEURO_AVWRmon20[,2:7] <- ((1 + port6USEURO_AVWRmon20[,2:7]/100) * (1 + ExchangeRateDataMonthly20[[4]])-1)*100
port6USEURO_AVWRmon20 <- port6USEURO_AVWRmon20 %>% mutate(SMB = (SMALL_LoPRIOR + ME1_PRIOR2 + SMALL_HiPRIOR)/3 - (BIG_LoPRIOR + ME2_PRIOR2 + BIG_HiPRIOR)/3)
port6USEURO_AVWRmon20 <- port6USEURO_AVWRmon20 %>% mutate(MOM = (SMALL_HiPRIOR + BIG_HiPRIOR)/2 - (SMALL_LoPRIOR + BIG_LoPRIOR)/2)
# Use EURO RF-rate
port6USEURO_AVWRmon20 <- port6USEURO_AVWRmon20 %>% mutate(RF = collapsed1[[8]])

#Change the AMarketCap to EURO, and use new dataframe
port6_AMarketCapNEW <- port6_AMarketCap %>% filter(month >= 200409 & month <= 202412)
port6_AMarketCapNEW[,2:7] <- port6_AMarketCapNEW[,2:7] * (1 + ExchangeRateDataMonthly20[[4]])

port6USEURO_AVWRmon20 <- merge(port6USEURO_AVWRmon20, port6_NFirms, by = "month", all.x = T, suffixes = c("", "_NFirms"))
port6USEURO_AVWRmon20 <- merge(port6USEURO_AVWRmon20, port6_AMarketCapNEW, by = "month", all.x = T, suffixes = c("", "_AMarketCap"))
port6USEURO_AVWRmon20 <- mutate(port6USEURO_AVWRmon20, 
                                numerator = SMALL_LoPRIOR*SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap+
                                  ME1_PRIOR2*ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap+
                                  SMALL_HiPRIOR*SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap+
                                  BIG_LoPRIOR*BIG_LoPRIOR_Nfirms*BIG_LoPRIOR_AMarketCap+
                                  ME2_PRIOR2*ME2_PRIOR2_Nfirms*ME2_PRIOR2_AMarketCap+
                                  BIG_HiPRIOR*BIG_HiPRIOR_Nfirms*BIG_HiPRIOR_AMarketCap,
                                denominator = SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap+
                                  ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap+
                                  SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap+
                                  BIG_LoPRIOR_Nfirms*BIG_LoPRIOR_AMarketCap+
                                  ME2_PRIOR2_Nfirms*ME2_PRIOR2_AMarketCap+
                                  BIG_HiPRIOR_Nfirms*BIG_HiPRIOR_AMarketCap,
                                MKT = numerator/denominator - RF) %>% select(-c(numerator,denominator)) # MKT is RF

#### iii) #####
# Filter the datasets so we only have data from September 2004 to December 2024
port6Euro_AVWRmon20 <- port6Euro_AVWRmon %>%
  filter(month >= 200409 & month <= 202412)
ExchangeRateDataMonthly20 <- ExchangeRateDataMonthly %>%
  filter(month >= 200409 & month <= 202412)

# Convert returns in USD to EURO
port6Euro_AVWRmon20[,2:7] <- ((1 + port6Euro_AVWRmon20[,2:7]/100) * (1 + ExchangeRateDataMonthly20[[4]])-1)*100

# Add the EURO RF-rate to the dataset
port6Euro_AVWRmon20 <- mutate(port6Euro_AVWRmon20, RF = collapsed1[[8]])

### Calculate the proxy-factors manually using the provided formulas
# SMB
port6Euro_AVWRmon20 <- port6Euro_AVWRmon20 %>% mutate(SMB = (port6Euro_AVWRmon20[[2]]+port6Euro_AVWRmon20[[3]]+port6Euro_AVWRmon20[[4]])/3-(port6Euro_AVWRmon20[[5]]+port6Euro_AVWRmon20[[6]]+port6Euro_AVWRmon20[[7]])/3)
ggplot(port6Euro_AVWRmon20, aes(x=SMB)) + geom_density()

# MOM
port6Euro_AVWRmon20 <- port6Euro_AVWRmon20 %>% mutate(MOM = (port6Euro_AVWRmon20[[4]]+port6Euro_AVWRmon20[[7]])/2-(port6Euro_AVWRmon20[[2]]+port6Euro_AVWRmon20[[5]])/2)
ggplot(port6Euro_AVWRmon20, aes(x=MOM)) + geom_density()

# Mkt-RF
#Change the AMarketCap to EURO, and use new dataframe
port6Euro_AMarketCapNEW <- port6Euro_AMarketCap %>% filter(month >= 200409 & month <= 202412)
port6Euro_AMarketCapNEW[,2:7] <- port6Euro_AMarketCapNEW[,2:7] * (1 + ExchangeRateDataMonthly20[[4]])

port6Euro_AVWRmon20 <- merge(port6Euro_AVWRmon20, port6Euro_NFirms, by = "month", all.x = T, suffixes = c("", ".NFirms"))
port6Euro_AVWRmon20 <- merge(port6Euro_AVWRmon20, port6Euro_AMarketCapNEW, by = "month", all.x = T, suffixes = c("", ".AMCap"))

port6Euro_AVWRmon20 <- mutate(port6Euro_AVWRmon20, 
                              "numerator" = port6Euro_AVWRmon20[[2]]*port6Euro_AVWRmon20[[11]]*port6Euro_AVWRmon20[[17]]+
                                port6Euro_AVWRmon20[[3]]*port6Euro_AVWRmon20[[12]]*port6Euro_AVWRmon20[[18]]+
                                port6Euro_AVWRmon20[[4]]*port6Euro_AVWRmon20[[13]]*port6Euro_AVWRmon20[[19]]+
                                port6Euro_AVWRmon20[[5]]*port6Euro_AVWRmon20[[14]]*port6Euro_AVWRmon20[[20]]+
                                port6Euro_AVWRmon20[[6]]*port6Euro_AVWRmon20[[15]]*port6Euro_AVWRmon20[[21]]+
                                port6Euro_AVWRmon20[[7]]*port6Euro_AVWRmon20[[16]]*port6Euro_AVWRmon20[[22]],
                              "denominator" = port6Euro_AVWRmon20[[11]]*port6Euro_AVWRmon20[[17]]+
                                port6Euro_AVWRmon20[[12]]*port6Euro_AVWRmon20[[18]]+
                                port6Euro_AVWRmon20[[13]]*port6Euro_AVWRmon20[[19]]+
                                port6Euro_AVWRmon20[[14]]*port6Euro_AVWRmon20[[20]]+
                                port6Euro_AVWRmon20[[15]]*port6Euro_AVWRmon20[[21]]+
                                port6Euro_AVWRmon20[[16]]*port6Euro_AVWRmon20[[22]],
                              MKT = numerator/denominator - RF) %>% select(-c(numerator,denominator)) # MKT IS RISK-FREE

### b) ####

# Calculate factors and from USD->EURO, 25-portfolio - US
data25USEURO <- port25_AVWRmon
data25USEURO <- data25USEURO %>% filter(month >= 200409 & month <= 202412)

data25USEURO[,2:26] <- ((1 + data25USEURO[,2:26]/100) * (1 + ExchangeRateDataMonthly20[[4]])-1)*100
# Use long-factors from 6-portfolios
data25USEURO <- data25USEURO %>% mutate(data25USEURO,MKT = port6USEURO_AVWRmon20$MKT)
data25USEURO <- data25USEURO %>% mutate(data25USEURO,SMB_long = port6USEURO_AVWRmon20$SMB_long)
data25USEURO <- data25USEURO %>% mutate(data25USEURO,MOM_long = port6USEURO_AVWRmon20$MOM_long)
data25USEURO <- data25USEURO %>% mutate(data25USEURO,RF = collapsed1[[8]])

data25USEURO <- data25USEURO %>% mutate(MOM = port6USEURO_AVWRmon20$MOM)
data25USEURO <- data25USEURO %>% mutate(SMB = port6USEURO_AVWRmon20$SMB)

c(names(data25EURO)[27],names(data25EURO)[31],names(data25EURO)[32])



### Expected surplus return ####
port6USEURO_AVWRmon20 <- mutate(port6USEURO_AVWRmon20, 
                                MOM_surplus = MOM,
                                SMB_surplus = SMB)
port6Euro_AVWRmon20 <- mutate(port6Euro_AVWRmon20, 
                              MOM_surplus = MOM,
                              SMB_surplus = SMB)
mean(port6USEURO_AVWRmon20$MKT) # MKT (US)
mean(port6USEURO_AVWRmon20$MOM_surplus) # MOM (US)
mean(port6USEURO_AVWRmon20$SMB_surplus) # SMB (US)

mean(port6Euro_AVWRmon20$MKT) # MKT (EU)
mean(port6Euro_AVWRmon20$MOM_surplus) # MOM (EU)
mean(port6Euro_AVWRmon20$SMB_surplus) # SMB (EU)

dfCorrelation <- data.frame(MKT_US = port6USEURO_AVWRmon20$MKT,
                            SMB_US = port6USEURO_AVWRmon20$SMB,
                            MOM_US = port6USEURO_AVWRmon20$MOM,
                            MKT_EU = port6Euro_AVWRmon20$MKT,
                            SMB_EU = port6Euro_AVWRmon20$SMB,
                            MOM_EU = port6Euro_AVWRmon20$MOM)

cov_matrixFactors <- cov(dfCorrelation[, c("MKT_US", "MOM_US", "SMB_US", "MKT_EU", "MOM_EU", "SMB_EU")], use = "complete.obs");cov_matrixFactors # Choose the ones we want

# Scatterplot matrices Pearson correlations
cor.print <- function(x,y){panel.text(mean(range(x)),mean(range(y)),paste(round(cor(x,y),digits = 2),sep = ''))}
contVar <- c("MKT_US", "MOM_US", "SMB_US", "MKT_EU", "MOM_EU", "SMB_EU")
splom(na.omit(dfCorrelation)[, contVar],xlab = "",
      upper.panel = panel.hexbinplot, pscales = 0, xbins = 20, varnames = contVar, lower.panel = cor.print)

regtableB <- function(dataEU) {
  # Subtract the RF-rate already
  dataEU[,2:26] <- dataEU[,2:26]-dataEU[,30]
  # Run linear models for columns 2 to 26 with covariates 27 to 29
  models <- lapply(2:26, function(i) {
    lm(as.formula(paste(names(dataEU)[i], "~", 
                        paste(c(names(data25EURO)[27],names(data25EURO)[31],names(data25EURO)[32]), collapse = "+"))), 
       data = dataEU)
  })
  
  # Extract and round coefficients
  coef_table <- do.call(rbind, lapply(models, function(model) {
    round(coef(model), 2)
  }))
  
  # Extract and round p-values
  pval_table <- do.call(rbind, lapply(models, function(model) {
    round(summary(model)$coefficients[, 4], 3)
  }))
  
  # Label rows
  rownames(coef_table) <- rownames(pval_table) <- paste("Model_Y", names(dataEU)[2:26])  
  # Return both tables
  list(Coefficients = coef_table, P_Values = pval_table)
}

regtableB(data25USEURO)
regtableB(data25EURO)

### c) ####
### Add the MOM_long factor and SMB_long factor for US-market
port6USEURO_AVWRmon20 <- mutate(port6USEURO_AVWRmon20, 
                                MOM_long = BIG_HiPRIOR,
                                numerator = SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap*SMALL_LoPRIOR+ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap*ME1_PRIOR2+SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap*SMALL_HiPRIOR,
                                denominator = SMALL_LoPRIOR_Nfirms*SMALL_LoPRIOR_AMarketCap+ME1_PRIOR2_Nfirms*ME1_PRIOR2_AMarketCap+SMALL_HiPRIOR_Nfirms*SMALL_HiPRIOR_AMarketCap,
                                SMB_long = numerator/denominator) %>% select(-c(numerator,denominator))

### Add the MOM_long factor and SMB_long factor for EU-market
port6Euro_AVWRmon20 <- mutate(port6Euro_AVWRmon20, 
                              MOM_long = BIG.HiPRIOR,
                              numerator = SMALL.LoPRIOR.NFirms*SMALL.LoPRIOR.AMCap*SMALL.LoPRIOR+ME1.PRIOR2.NFirms*ME1.PRIOR2.AMCap*ME1.PRIOR2+SMALL.HiPRIOR.NFirms*SMALL.HiPRIOR.AMCap*SMALL.HiPRIOR,
                              denominator = SMALL.LoPRIOR.NFirms*SMALL.LoPRIOR.AMCap+ME1.PRIOR2.NFirms*ME1.PRIOR2.AMCap+SMALL.HiPRIOR.NFirms*SMALL.HiPRIOR.AMCap,
                              SMB_long = numerator/denominator) %>% select(-c(numerator,denominator))

### Expected surplus return ####
port6USEURO_AVWRmon20 <- mutate(port6USEURO_AVWRmon20, 
                                MOM_long_surplus = MOM_long-RF,
                                SMB_long_surplus = SMB_long-RF)
port6Euro_AVWRmon20 <- mutate(port6Euro_AVWRmon20, 
                              MOM_long_surplus = MOM_long-RF,
                              SMB_long_surplus = SMB_long-RF)

mean(port6USEURO_AVWRmon20$MKT) # MKT (US)
mean(port6USEURO_AVWRmon20$MOM_long_surplus) # Tech Stocks (US)
mean(port6USEURO_AVWRmon20$SMB_long_surplus) # Small Cap (US)

mean(port6Euro_AVWRmon20$MKT) # MKT (EU)
mean(port6Euro_AVWRmon20$MOM_long_surplus) # Tech Stocks (EU)
mean(port6Euro_AVWRmon20$SMB_long_surplus) # Small Cap (EU)

## COVARIANCE MATRIX - of recommended factor universe ##
dfCorrelation_long <- data.frame(MKT_US = port6USEURO_AVWRmon20$MKT,
                                 SMB_US = port6USEURO_AVWRmon20$SMB_long_surplus,
                                 MOM_US = port6USEURO_AVWRmon20$MOM_long_surplus,
                                 MKT_EU = port6Euro_AVWRmon20$MKT,
                                 SMB_EU = port6Euro_AVWRmon20$SMB_long_surplus,
                                 MOM_EU = port6Euro_AVWRmon20$MOM_long_surplus)
cov_matrixFactors_long <- cov(dfCorrelation_long[, c("MKT_US", "SMB_US", "MOM_US", "MKT_EU", "SMB_EU", "MOM_EU")], use = "complete.obs");cov_matrixFactors_long # Choose the ones we want


# Calculate factors and from USD->EURO, 25-portfolio - EURO
data25EURO <- port25Euro_AVWRmon
data25EURO <- data25EURO %>% filter(month >= 200409 & month <= 202412)

data25EURO[,2:26] <- ((1 + data25EURO[,2:26]/100) * (1 + ExchangeRateDataMonthly20[[4]])-1)*100
# Use long-factors from 6-portfolios
data25EURO <- data25EURO %>% mutate(MKT = port6Euro_AVWRmon20$MKT)
data25EURO <- data25EURO %>% mutate(SMB_long = port6Euro_AVWRmon20$SMB_long)
data25EURO <- data25EURO %>% mutate(MOM_long = port6Euro_AVWRmon20$MOM_long)
data25EURO <- data25EURO %>% mutate(data25EURO, RF = collapsed1[[8]])

data25EURO <- data25EURO %>% mutate(MOM = port6Euro_AVWRmon20$MOM)
data25EURO <- data25EURO %>% mutate(SMB = port6Euro_AVWRmon20$SMB)

regtableC <- function(dataUS) {
  # Subtract the RF-rate already
  dataUS[,2:26] <- dataUS[,2:26]-dataUS[,30]
  # Run linear models for columns 2 to 26 with covariates 27 to 29
  models <- lapply(2:26, function(i) {
    lm(as.formula(paste(names(dataUS)[i], "~", 
                        paste(names(dataUS)[27:29], collapse = "+"))), 
       data = dataUS)
  })
  
  # Extract and round coefficients
  coef_table <- do.call(rbind, lapply(models, function(model) {
    round(coef(model), 2)
  }))
  
  # Extract and round p-values
  pval_table <- do.call(rbind, lapply(models, function(model) {
    round(summary(model)$coefficients[, 4], 3)
  }))
  
  # Label rows
  rownames(coef_table) <- rownames(pval_table) <- paste("Model_Y", names(dataUS)[2:26])  
  # Return both tables
  list(Coefficients = coef_table, P_Values = pval_table)
}

regtableC(data25USEURO)
regtableC(data25EURO)
regtableC(data25USEURO)$Coefficients[,2]

# Scatterplot matrices with Pearson correlations
cor.print <- function(x,y){panel.text(mean(range(x)),mean(range(y)),paste(round(cor(x,y),digits = 2),sep = ''))}
contVar <- c("MKT_US", "SMB_US", "MOM_US", "MKT_EU", "SMB_EU", "MOM_EU")
splom(na.omit(dfCorrelation_long)[, contVar],xlab = "",
      upper.panel = panel.hexbinplot, pscales = 0, xbins = 20, varnames = contVar, lower.panel = cor.print)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## o ------------------------------------------------------------------- o ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### d) ####
#### DATA WE USE ####
dataD <- port6USEURO_AVWRmon20 %>% select(c(month)) %>% mutate(MKT_US = port6USEURO_AVWRmon20[["MKT"]]/100,
                                                               SMB_US_long_surplus = port6USEURO_AVWRmon20[["SMB_long_surplus"]]/100,
                                                               MOM_US_long_surplus = port6USEURO_AVWRmon20[["MOM_long_surplus"]]/100,
                                                               MKT_EU = port6Euro_AVWRmon20[["MKT"]]/100,
                                                               SMB_EU_long_surplus = port6Euro_AVWRmon20[["SMB_long_surplus"]]/100,
                                                               MOM_EU_long_surplus = port6Euro_AVWRmon20[["MOM_long_surplus"]]/100)
### MEAN-VARIANCE OPTIMIZED ####
dataD
muD <- c(mean(dataD$MKT_US),
         mean(dataD$SMB_US_long_surplus),
         mean(dataD$MOM_US_long_surplus),
         mean(dataD$MKT_EU),
         mean(dataD$SMB_EU_long_surplus),
         mean(dataD$MOM_EU_long_surplus))
covD <- cov(dataD[, c("MKT_US", "SMB_US_long_surplus", "MOM_US_long_surplus", "MKT_EU", "SMB_EU_long_surplus", "MOM_EU_long_surplus")], use = "complete.obs");covD

#### i) ####
### Minimum-Variance portfolio ####
n <- 1000
mu_opt <- matrix(rep(NA, 208*n), nrow=n, ncol=208)
var_opt <- matrix(rep(NA, 208*n), nrow=n, ncol=208)
mu_actual <- matrix(rep(NA, 208*n), nrow=n, ncol=208)
mu_minvarRES <- c()
var_minvarRES <- c()
w_minvarRES <- matrix(rep(NA), nrow = 208, ncol = 6)
minvarRES <- c()

for (i in 1:(244-36)){ # i RUN +1 MORE?????
  runData <- dataD[i:(i+35),]
  mu <- c(mean(runData[[2]]),
          mean(runData[[3]]),
          mean(runData[[4]]),
          mean(runData[[5]]),
          mean(runData[[6]]),
          mean(runData[[7]]))
  Sigma <- cov(runData[,c("MKT_US", "SMB_US_long_surplus", "MOM_US_long_surplus", "MKT_EU", "SMB_EU_long_surplus", "MOM_EU_long_surplus")], use = "complete.obs")
  Dmat <- 2 * Sigma
  dvec <- rep(0, 6)
  
  # Constraints: sum(w) = 1, w >= 0
  Amat_minvar <- cbind(rep(1, 6), diag(6))
  bvec_minvar <- c(1, rep(0, 6))
  
  res_minvar <- solve.QP(Dmat, dvec, Amat_minvar, bvec_minvar, meq = 1)
  
  w_minvar <- res_minvar$solution;w_minvar  # weights for minimum variance
  var_minvar <- res_minvar$value;var_minvar # actual minimum variance
  mu_minvar <- sum(w_minvar*mu);mu_minvar   # expected return for minimum variance
  
  mu_minvarRES[i] <- mu_minvar              # store expected return for minimum variance
  var_minvarRES[i] <- var_minvar            # store minimum variance
  w_minvarRES[i,] <- w_minvar
  
  runData <- dataD[i:(i+36),]
  Ret <- t(c(w_minvar))%*%c(unname(unlist(runData[37,2:7])))
  minvarRES[i] <- Ret
  runData <- dataD[i:(i+35),]
  
  # --- Quadratic programming setup ---
  stepsize <- (max(mu)-mu_minvar)/(n)
  
  for (j in 1:n) {
    muP <- mu_minvar+(j-1)*stepsize   # target portfolio mean return
    
    # Constraints: sum(w)=1, mu'w = muP, and w >= 0
    Amat <- cbind(rep(1, 6), mu, diag(6))
    bvec <- c(1, muP, rep(0, 6))
    
    # Solve
    res <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
    
    w_opt <- res$solution;w_opt
    var_opt[j,i] <- res$value;var_opt
    mu_opt[j,i] <- sum(w_opt*mu) # same as muP?
    
    
    # Get the values out
    # mu_actual
  }
}

### Maximum Expected portfolio ####
maxExpRetRES <- data.frame(Return = c(rep(NA,244-36)),
                           Variance = c(rep(NA,244-36)))
for (i in 1:(244-36)){
  runData <- dataD[i:(i+35),]
  mu <- c(mean(runData[[2]]),
          mean(runData[[3]]),
          mean(runData[[4]]),
          mean(runData[[5]]),
          mean(runData[[6]]),
          mean(runData[[7]]))
  Sigma <- cov(runData[,c("MKT_US", "SMB_US_long_surplus", "MOM_US_long_surplus", "MKT_EU", "SMB_EU_long_surplus", "MOM_EU_long_surplus")], use = "complete.obs")
  
  best_index <- which.max(mu)
  runData <- dataD[i:(i+36),]
  Ret <- runData[37,best_index+1]
  Var <- Sigma[best_index,best_index]
  
  maxExpRetRES[i,1] <- Ret
  maxExpRetRES[i,2] <- Var
  
  runData <- dataD[i:(i+35),]
}

#### ii) ####
rpRES <-  data.frame(Return = c(rep(NA,244-36)),
                     Variance = c(rep(NA,244-36)))
##### Risk-Parity (Unlevered) ####
rpWeights <- matrix(0, nrow = 244-36, ncol = 6)
rpMu <- matrix(0, nrow = 244-36, ncol = 1)
rpMu <- c(rep(NA,244-36))
for (i in 1:(244-36)){
  runData <- dataD[i:(i+35),]
  mu <- c(mean(runData[[2]]),
          mean(runData[[3]]),
          mean(runData[[4]]),
          mean(runData[[5]]),
          mean(runData[[6]]),
          mean(runData[[7]]))
  # Compute \hat{std}
  std <- sapply(runData[2:7], sd)
  # Calculate delta
  delta <- 1/(sum(1/std))
  # Calculate w
  rpWeights[i,] <- (1/unname(std))*delta
  rpMu[i] <- rpWeights[i,]%*%mu
  
  # best_index <- which.max(mu)
  runData <- dataD[i:(i+36),]
  # Ret <- runData[37,best_index+1]
  Ret <- t(c(rpWeights[i,]))%*%c(unname(unlist(runData[37,2:7])))
  # Var <- Sigma[best_index,best_index]
  
  rpRES[i,1] <- Ret[1,1]
  rpRES[i,2] <- Var
  
  runData <- dataD[i:(i+35),]
  
}

### e) ####
### REALIZED SHARP-RATIOS: ####
# Minimum variance
mean(minvarRES)/sd(minvarRES)
mean(minvarRES)/sd(minvarRES)*sqrt(12)

# Sharpe-ratio

# Maximum expected return
mean(maxExpRetRES$Return)/sd(maxExpRetRES$Return)

# Risk-Parity
mean(rpRES$Return)/sd(rpRES$Return)
mean(rpRES$Return)/sd(rpRES$Return)*sqrt(12)

### f) ####
# DATA-INPUT NAMES HAVE CHANGED!!!!
# Optimal w in min-var portfolio (SHORTING)
w_short <- matrix(rep(NA,208*10), nrow=208, ncol=10)

for(i in 1:208){
  EURInvestData2 <- EURInvestData[i:(i+35),]
  EUData2 <- EUData[i:(i+35),]
  Data <- data.frame(v1 = EURInvestData2$Mkt/100-EURInvestData2$RF/100, 
                     v2 = EURInvestData2$MOM2/100-EURInvestData2$RF/100, 
                     v3 = EURInvestData2$SMB2/100-EURInvestData2$RF/100, 
                     v4 = EUData2$Mkt/100-EUData2$RF/100, 
                     v5 = EUData2$MOM2/100-EUData2$RF/100, 
                     v6 = EUData2$SMB2/100-EUData2$RF/100,
                     v7 = EURInvestData2$MOM/100,
                     v8 = EURInvestData2$SMB/100,
                     v9 = EUData2$MOM/100,
                     v10 = EUData2$SMB/100)
  #Note above v1-v6 is already excess returns and v7-v10 is returns
  
  # --- Data summary ---
  mu <- c(mean(Data$v1),
          mean(Data$v2),
          mean(Data$v3),
          mean(Data$v4),
          mean(Data$v5),
          mean(Data$v6),
          mean(Data$v7),
          mean(Data$v8),
          mean(Data$v9),
          mean(Data$v10))     # mean excess returns
  
  Sigma <- cov(Data)
  
  Dmat <- 2 * Sigma
  dvec <- rep(0, 10)
  
  Amat <- cbind(
    c(rep(1, 6), rep(0, 4)),
    diag(10)
  )
  bvec <- c(1, rep(0, 10))
  
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  Investreturn_vector <- c(EURInvestData$Mkt[i+36]/100-EURInvestData$RF[i+36]/100, 
                           EURInvestData$MOM2[i+36]/100-EURInvestData$RF[i+36]/100, 
                           EURInvestData$SMB2[i+36]/100-EURInvestData$RF[i+36]/100,
                           EUData$Mkt[i+36]/100-EUData$RF[i+36]/100, 
                           EUData$MOM2[i+36]/100-EUData$RF[i+36]/100, 
                           EUData$SMB2[i+36]/100-EUData$RF[i+36]/100,
                           EURInvestData$MOM[i+36]/100,
                           EURInvestData$SMB[i+36]/100,
                           EUData$MOM[i+36]/100,
                           EUData$SMB[i+36]/100)
  for(k in 1:10){
    w_short[i,k] <- sol$solution[k]
  }
  returnMinVar$Shorting[i] <- sum(w_short[i,]*Investreturn_vector)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## o ------------------------------------------------------------------- o ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## 2) CPPI & Tie-In ####
### a) Baseline analysis of the empirical distribution of the accrual of guarantee in the current system ####

#### Choose the necessary data ####  
# Active portfolio is 100% European equity
data2 <- port6Euro_AVWRmon20 %>% select(c(month, RF, MKT, SMB_long, MOM_long))
data2 <- mutate(data2, MKT = MKT+RF)
data2 <- mutate(data2, ZC10_rate = exp(-fctSpot_rate(10, collapsed1$BETA0,collapsed1$BETA1,collapsed1$BETA2,collapsed1$BETA3,collapsed1$TAU1,collapsed1$TAU2)/100*10))

# We get 0.636. He has 0.644
80*(1/0.6434273)
# we get price 124.33 he has price 124.2

#### Minimum-Variance portfolio ####
mu_minvarRES2 <- c()
var_minvarRES2 <- c()
w_minvarRES2 <- matrix(rep(NA), nrow = 208, ncol = 3)
minvarRES2 <- c()

for (i in 1:(244-36)){ # i RUN +1 MORE?????
  runData <- data2[i:(i+35),]
  mu <- c(mean(runData[[3]]),
          mean(runData[[4]]),
          mean(runData[[5]]))
  Sigma <- cov(runData[,c("MKT", "SMB_long", "MOM_long")], use = "complete.obs")
  Dmat <- 2 * Sigma
  dvec <- rep(0, 3)
  
  # Constraints: sum(w) = 1, w >= 0
  Amat_minvar <- cbind(rep(1, 3), diag(3))
  bvec_minvar <- c(1, rep(0, 3))
  
  res_minvar <- solve.QP(Dmat, dvec, Amat_minvar, bvec_minvar, meq = 1)
  
  w_minvar <- res_minvar$solution;w_minvar  # weights for minimum variance
  var_minvar <- res_minvar$value;var_minvar # actual minimum variance
  mu_minvar <- sum(w_minvar*mu);mu_minvar   # expected return for minimum variance
  
  mu_minvarRES2[i] <- mu_minvar              # store expected return for minimum variance
  var_minvarRES2[i] <- var_minvar            # store minimum variance
  w_minvarRES2[i,] <- w_minvar
  
  runData <- data2[i:(i+36),]
  Ret <- t(c(w_minvar))%*%c(unname(unlist(runData[37,3:5])))
  minvarRES2[i] <- Ret
  runData <- data2[i:(i+35),]
}

#### Accrual of guarantee in current system ####
head(data2)
L_target <- 1.25
L_trigger <- 1.30
c_0 <- 100
A <- c()
a <- c()
N <- c()
R <- c()
L <- c()
etaA <- c()
etaR <- c()

# Initial guarantee
G_0 <- (c_0/L_target)/data2[36,"ZC10_rate"];G_0
# Initial values
L[1] <- L_target
etaA[1] <- c_0-(c_0/L_target)
etaR[1] <- c_0-(c_0/L_target)
A[1] <- etaA[1]*1
R[1] <- etaR[1]*1/(L_target-1)

accrualGuaranteesCurrentSystem <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 89))
MVAcurrent <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 89))
MVRcurrent <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 89))
Lcurrent <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 89))

for (j in 0:88){ # 88 different months
  N[1] <- (c_0/L_target)/data2[36+j,"ZC10_rate"]
  for (i in 1:121){
    
    TTM <- 10-(1/12)*i
    marketValueP <- exp(-TTM*fctSpot_rate(TTM, collapsed1$BETA0[35+i+j],collapsed1$BETA1[35+i+j],
                                          collapsed1$BETA2[35+i+j],collapsed1$BETA3[35+i+j],
                                          collapsed1$TAU1[35+i+j],collapsed1$TAU2[35+i+j])/100)
    if (i<120){marketValueP = marketValueP}else{marketValueP=1}
    
    a[i] <- data2[35+i+j,"MKT"]/100  # a
    A[i+1] <- (1+a[i])*A[i]          # A
    R[i+1] <- N[i]*marketValueP      # R
    etaA[i+1] <- 20                  # etaA
    etaR[i+1] <- 20                  # etaR
    L[i+1] <- (A[i+1]+R[i+1])/R[i+1] # L
    W[i+1] <- A[i+1]+R[i+1]
    
    # N_t is number of zero coupon bonds
    if (L[i+1]>L_trigger){
      N[i+1] = (A[i+1]+R[i+1])/(L_target*marketValueP)
      A[i+1] = (A[i+1]+R[i+1])*(1-1/L_target)
    } else {
      N[i+1] <- N[i]
    }
  }
  accrualGuaranteesCurrentSystem[,j+1] <- N[1:121] #### CHANGE THESE
  # accrualGuaranteesCurrentSystem[,j+1] <- W[1:121] #### CHANGE THESE
  MVAcurrent[,j+1] <- A[1:121]
  MVRcurrent[,j+1] <- R[1:121]
  Lcurrent[,j+1] <- L[1:121]
}

### b) Allow for a CPPI strategy ####
#### NEW DATASET BASED ON ADVICE IN PREVIOUS SECTIONS ####
# Choose the necessary data. Active portfolio is both European and American equity
data2new <- data.frame(month       = port6Euro_AVWRmon20$month,
                       RF          = port6Euro_AVWRmon20$RF,
                       MKT_EU      = port6Euro_AVWRmon20$MKT + port6Euro_AVWRmon20$RF,
                       # SMB_EU      = port6Euro_AVWRmon20$SMB,
                       # MOM_EU      = port6Euro_AVWRmon20$MOM,
                       SMB_long_EU = port6Euro_AVWRmon20$SMB_long,
                       MOM_long_EU = port6Euro_AVWRmon20$MOM_long,
                       MKT_US      = port6USEURO_AVWRmon20$MKT + port6USEURO_AVWRmon20$RF,
                       # SMB_US      = port6USEURO_AVWRmon20$SMB,
                       # MOM_US      = port6USEURO_AVWRmon20$MOM,
                       SMB_long_US = port6USEURO_AVWRmon20$SMB_long,
                       MOM_long_US = port6USEURO_AVWRmon20$MOM_long)
data2new <- mutate(data2new, ZC10_rate = exp(-fctSpot_rate(10, collapsed1$BETA0,collapsed1$BETA1,collapsed1$BETA2,collapsed1$BETA3,collapsed1$TAU1,collapsed1$TAU2)/100*10))

#### Minimum-Variance portfolio ####
mu_minvarRES2new <- c()
var_minvarRES2new <- c()
w_minvarRES2new <- matrix(rep(NA), nrow = 208, ncol = 6)
minvarRES2new <- c()

for (i in 1:(244-36)){ # i RUN +1 MORE?????
  runData <- data2new[i:(i+35),]
  mu <- c(mean(runData[[3]]),
          mean(runData[[4]]),
          mean(runData[[5]]),
          mean(runData[[6]]),
          mean(runData[[7]]),
          mean(runData[[8]]))
  Sigma <- cov(runData[,c("MKT_EU", "SMB_long_EU", "MOM_long_EU", "MKT_US", "SMB_long_US", "MOM_long_US")], use = "complete.obs")
  Dmat <- 2 * Sigma
  dvec <- rep(0, 6)
  
  # Constraints: sum(w) = 1, w >= 0
  Amat_minvar <- cbind(rep(1, 6), diag(6))
  bvec_minvar <- c(1, rep(0, 6))
  
  res_minvar <- solve.QP(Dmat, dvec, Amat_minvar, bvec_minvar, meq = 1)
  
  w_minvar <- res_minvar$solution;w_minvar  # weights for minimum variance
  var_minvar <- res_minvar$value;var_minvar # actual minimum variance
  mu_minvar <- sum(w_minvar*mu);mu_minvar   # expected return for minimum variance
  
  mu_minvarRES2new[i] <- mu_minvar              # store expected return for minimum variance
  var_minvarRES2new[i] <- var_minvar            # store minimum variance
  w_minvarRES2new[i,] <- w_minvar
  
  runData <- data2new[i:(i+36),]
  Ret <- t(c(w_minvar))%*%c(unname(unlist(runData[37,3:8])))
  minvarRES2new[i] <- Ret
  runData <- data2new[i:(i+35),]
}

#### Accrual of guarantee in new system and with CPPI ####
# Initial guarantees
G_0 <- Floor[1]/data2new[36:123,"ZC10_rate"];G_0

## Things to modify for the strategy
L_target <- 1.25
L_trigger <- 1.30
m <- 1

median(unname(unlist(Nnew[1,])))
mean(unname(unlist(Nnew[1,])))

NewSystem <- function(m, L_target, L_trigger){
  # m=3; L_target=1.35; L_trigger=1.35*1.1 # <-- CHANGE THESE
  m=1; L_target=1.25; L_trigger=1.3 # <-- CHANGE THESE
  # Define vectors for the needed sizes
  A <- c();a <- c();N <- c();R <- c();L <- c();etaA <- c();etaR <- c()
  Floor <- c();W <- c();C <- c();E <- c();MVA <- c();MVR <- c()
  
  # Initial values - R[1] is in the loop
  A[1] <- 1
  L[1] <- L_target
  W[1] <- 100
  Floor[1] <- 1/L_target*W[1]
  C[1] <- W[1]-Floor[1]
  E[1] <- min(m*C[1],W[1])
  etaA[1] <- E[1]/A[1]
  MVA[1] <- etaA[1]*A[1]
  MVR[1] <- W[1]-E[1]
  
  # Matrices to store values
  Nnew <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 88))
  Wnew <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 88))
  Lnew <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 88))
  Fnew <- data.frame(matrix(c(rep(NA)), nrow = 121, ncol = 88))
  
  # Rolling loop for each starting month, 88 different months: j=0 Starting in August 2007
  for (j in 0:87){
    # Initial values depending on ZC-rate
    R[1] <- data2new[36+j,"ZC10_rate"]
    N[1] <- (W[1]/L_target)/R[1]
    etaR[1] <- (W[1]-E[1])/R[1]
    
    # Rolling loop 10 years
    for (i in 1:120){
      # Calculate new ZC-rate for the rolling month
      TTM <- 10-(1/12)*i
      marketValueP <- exp(-TTM*fctSpot_rate(TTM, collapsed1$BETA0[36+i+j],collapsed1$BETA1[36+i+j],
                                            collapsed1$BETA2[36+i+j],collapsed1$BETA3[36+i+j],
                                            collapsed1$TAU1[36+i+j],collapsed1$TAU2[36+i+j])/100)
      if (i<120){marketValueP = marketValueP}else{marketValueP=1}
      
      # Things to update
      a[i] <- minvarRES2new[i+j]/100            # return
      A[i+1] <- (1+a[i])*A[i]                   # A
      R[i+1] <- marketValueP                    # R
      W[i+1] <- etaA[i]*A[i+1] + etaR[i]*R[i+1] # Wealth
      Floor[i+1] <- N[i]*R[i+1]                 # Floor
      L[i+1] <- W[i+1]/Floor[i+1]               # Funded ratio L
      C[i+1] <- max(W[i+1]-Floor[i+1],0)        # Cushion
      E[i+1] <- min(m*C[i+1],W[i+1])            # Exposure
      MVA[i+1] <- etaA[i]*A[i+1]                # Market value of Active
      MVR[i+1] <- etaR[i]*R[i+1]                # Market value of Reserve
      
      # --- Tie-in --- #
      if (L[i+1]>L_trigger){
        N[i+1] = W[i+1]/(L_target*R[i+1])
      } else {
        N[i+1] <- N[i]
      }
      etaA[i+1] <- E[i+1]/A[i+1]                # etaA
      etaR[i+1] <- (W[i+1]-E[i+1])/R[i+1]       # etaR
      
    }
    # Save results  
    Nnew[,j+1] <- N[1:121]
    Fnew[,j+1] <- Floor[1:121]
    Wnew[,j+1] <- W[1:121]
    Lnew[,j+1] <- L[1:121]
  }
  # RUN UNTIL HERE
  # Take the values at the end, i.e. N_120 and Floor_120 and Wealth_120
  RESULTATnew <- data.frame(Nend = unname(unlist(Nnew[121,])),
                            Fend = unname(unlist(Fnew[121,])),
                            # Lend = unname(unlist(Lnew[121,])),
                            Wend = unname(unlist(Wnew[121,])))
  # RESULTATnew <- data.frame(Lend = unname(unlist(Lnew[,20])))
  return(RESULTATnew)
}
NewSystem(1,1.25,1.30)

### Tables to choose the m multiplier ####
# Guarantee
# wrapper: calls NewSystem for each m and names the results
simulate_roll_multi <- function(m_vec, L_target, L_trigger) {
  setNames(lapply(m_vec, function(m) NewSystem(m, L_target, L_trigger)),
           paste0("m_", m_vec))
}
# RESULTATnew for each m, choose the desired m's
out_list <- simulate_roll_multi(seq(1,4,0.25), 1.25, 1.3)

# Extract the preferred columns and do the desired function on each vector
# Kør unname() for at få dataet til en vektor
sapply(lapply(out_list, '[[', 1), mean)
sapply(lapply(out_list, '[[', 1), sd)
sapply(lapply(out_list, '[[', 2), mean)
sapply(lapply(out_list, '[[', 2), sd)
sapply(lapply(out_list, '[[', 3), mean)
sapply(lapply(out_list, '[[', 3), sd)
unname(sapply(lapply(out_list, '[[', 3), sd))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## o ------------------------------------------------------------------- o ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
