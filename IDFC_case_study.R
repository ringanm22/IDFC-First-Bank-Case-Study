library(dplyr)

set.seed(1)
## Reading the devdata csv 
dat <- read.csv("case_study_devdata.csv")


## Handling NA values
na_count <- sapply(dat, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)

## Omiting columns which have more than 35000 NA value.
na_count <- subset(na_count, na_count < 15000) %>% as.data.frame()



dat <- dat[, c(row.names(na_count), "has_taken_emi_before", "count_of_emi_before")]

## Finding the columns with NA values

new_na_count <- subset(na_count, na_count > 0) %>% as.data.frame()
na_dat <- dat[, row.names(new_na_count)]

########################### Imputation of missing values #####################################

## credit limit

plot(table(na_dat$credit_limit))           ## not symmetric
median(na.omit(na_dat$credit_limit))      ## median better replacement for 
dat$credit_limit[is.na(dat$credit_limit)] <- median(na.omit(na_dat$credit_limit))

## revolve

table(na_dat$revolve_1m) 
table(na_dat$revolve_3m) 
table(na_dat$revolve_6m) 

dat$revolve_1m[is.na(dat$revolve_1m)] <- 0
dat$revolve_3m[is.na(dat$revolve_3m)] <- 0
dat$revolve_6m[is.na(dat$revolve_6m)] <- 0

# The number of customers that have revolved has increased over months


## 
mode <- function(x, na.rm = FALSE) {
  
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

mode(na.omit(na_dat$util_1m))
mode(na.omit(na_dat$util_3m))
mode(na.omit(na_dat$util_6m))

dat$util_1m[is.na(dat$util_1m)] <- mode(na.omit(na_dat$util_1m))
dat$util_3m[is.na(dat$util_3m)] <- mode(na.omit(na_dat$util_3m))
dat$util_6m[is.na(dat$util_6m)] <- mode(na.omit(na_dat$util_6m))

## payment

mode(na.omit(na_dat$payment_ratio_1m))
median(na.omit(na_dat$payment_ratio_1m))

mode(na.omit(na_dat$payment_ratio_3m))
median(na.omit(na_dat$payment_ratio_3m))

mode(na.omit(na_dat$payment_ratio_6m))
median(na.omit(na_dat$payment_ratio_6m))

dat$payment_ratio_1m[is.na(dat$payment_ratio_1m)] <- mode(na.omit(na_dat$payment_ratio_1m))
dat$payment_ratio_3m[is.na(dat$payment_ratio_3m)] <- mode(na.omit(na_dat$payment_ratio_3m))
dat$payment_ratio_6m[is.na(dat$payment_ratio_6m)] <- mode(na.omit(na_dat$payment_ratio_6m))

## paymed

mode(na.omit(na_dat$paymad_1m))
mode(na.omit(na_dat$paymad_3m))
mode(na.omit(na_dat$paymad_6m))

dat$paymad_1m[is.na(dat$paymad_1m)] <- mode(na.omit(na_dat$paymad_1m))
dat$paymad_3m[is.na(dat$paymad_3m)] <- mode(na.omit(na_dat$paymad_3m))
dat$paymad_6m[is.na(dat$paymad_6m)] <- mode(na.omit(na_dat$paymad_6m))

## Age

## age is given negative. making it positive

median(-na.omit(dat$age))
mode_cont <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found onRSiteSearch)	
  x<-data
  lim.inf=min(x)-1; lim.sup=max(x)+1
  
  hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.2))
  s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
  n<-length(s$y)
  v1<-s$y[1:(n-2)];
  v2<-s$y[2:(n-1)];
  v3<-s$y[3:n]
  ix<-1+which((v1<v2)&(v2>v3))
  
  lines(s$x,s$y,col="red")
  points(s$x[ix],s$y[ix],col="blue")
  
  md <- s$x[which(s$y==max(s$y))] 
  
  md
}

mode_cont(na.omit(dat$age))
dat$age[is.na(dat$age)] <- mode_cont(na.omit(dat$age))
dat$age <- -dat$age

## bureau all amt ever
mean(na.omit(na_dat$Bureau_all_amt_ever))
mode(na.omit(na_dat$Bureau_all_amt_ever))
median(na.omit(na_dat$Bureau_all_amt_ever))

## x <- na_dat$Bureau_all_amt_ever[(na_dat$Bureau_all_amt_ever) < 5e7]
## t <- table(na.omit(na_dat$Bureau_all_amt_ever))

## 2000 and 11700000 has very similar frequency and the come in 1st nd secind position in the frequency distribution.
## but most of the high freq loan amt is over 1 crores.
## meadian is 1531777 and mean is 7890301. so taking mode as a replacement

#t %>%
#  as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab

dat$Bureau_all_amt_ever[is.na(dat$Bureau_all_amt_ever)] <- mode(na.omit(na_dat$Bureau_all_amt_ever))

## bureau all amt live

mean(na.omit(na_dat$Bureau_all_amt_live))
mode(na.omit(na_dat$Bureau_all_amt_live))
median(na.omit(na_dat$Bureau_all_amt_live))

#t <- table(na.omit(na_dat$Bureau_all_amt_live))
#t %>%
# as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab

#hist(na_dat$Bureau_all_amt_live[na_dat$Bureau_all_amt_live < 1e4])

dat$Bureau_all_amt_live[is.na(dat$Bureau_all_amt_live)] <- mode(na.omit(na_dat$Bureau_all_amt_live))

## avg sa balance

#hist(na.omit(na_dat$avg_sa_balance_1m))

mode(na.omit(na_dat$avg_sa_balance_1m))
median(na.omit(na_dat$avg_sa_balance_1m))
mean(na.omit(na_dat$avg_sa_balance_1m))

dat$avg_sa_balance_1m[is.na(dat$avg_sa_balance_1m)] <- median(na.omit(na_dat$avg_sa_balance_1m))
dat$avg_sa_balance_3m[is.na(dat$avg_sa_balance_3m)] <- median(na.omit(na_dat$avg_sa_balance_3m))
dat$avg_sa_balance_6m[is.na(dat$avg_sa_balance_6m)] <- median(na.omit(na_dat$avg_sa_balance_6m))
dat$avg_sa_balance_12m[is.na(dat$avg_sa_balance_12m)] <- median(na.omit(na_dat$avg_sa_balance_12m))

## pctchg curr sa avg sa

## have to take advice

#hist(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
#hist(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m[na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m < 20])

#t <- table(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

#t %>%
#  as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab


mean(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
median(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))


dat$pctchg_curr_sa_bal_avg_sa_bal_1m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
dat$pctchg_curr_sa_bal_avg_sa_bal_3m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_3m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m))
dat$pctchg_curr_sa_bal_avg_sa_bal_6m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_6m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

#sum(is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m))

### checking

n_na_count <- sapply(dat, function(y) sum(is.na(y)))
n_na_count <- data.frame(n_na_count)


dat$merchant_name <- as.factor(dat$merchant_name)
dat$merchant_country <- as.factor(dat$merchant_country)
dat$Merchant_category <- as.factor(dat$Merchant_category)
dat$product <- as.factor(dat$product)

# merchant name

foo <- length(levels(dat$merchant_name))
propthis <- length(levels(dat$merchant_name))
for(i in 1:foo){
  this <-  levels(dat$merchant_name)[i] 
  ythis <- dat[which(dat$merchant_name == this), 5]
  
  propthis[i] <- sum(ythis)
  
}
sort_mer <- sort(propthis, decreasing = TRUE)
x<- c()
for (i in 1:10) {
  x[i] <- (which(propthis == sort_mer[i]))
}

x <- unique(x)
ymname <- dat[x, 3]

dat$new_merchant_name <- ifelse(dat[, 3] == ymname, "danger", "not danger") %>% as.factor()

## merchant country

foo <- length(levels(dat$merchant_country))
propthis <- length(levels(dat$merchant_country))
for(i in 1:foo){
  this <-  levels(dat$merchant_country)[i] 
  ythis <- dat[which(dat$merchant_country == this), 5]
  
  propthis[i] <- sum(ythis)
  
}
sort_mer <- sort(propthis, decreasing = TRUE)
x <- c()
for (i in 1:5) {
  x[i] <- (which(propthis == sort_mer[i]))
}
x <- unique(x)
ymcountry <- dat[x, 4]
dat$new_merchant_country <- ifelse(dat[, 4] == ymcountry, "danger", "not danger") %>% as.factor()

## merchant category

foo <- length(levels(dat$Merchant_category))
propthis <- length(levels(dat$Merchant_category))
for(i in 1:foo){
  this <-  levels(dat$Merchant_category)[i] 
  ythis <- dat[which(dat$Merchant_category == this), 5]
  
  propthis[i] <- sum(ythis)
  
}
sort_mer <- sort(propthis, decreasing = TRUE)

x <- c()
for (i in 1:10) {
  x[i] <- (which(propthis == sort_mer[i]))
}
x <- unique(x)
ymcat <- dat[x, 6]

dat$new_merchant_category <- ifelse(dat[, 6] == ymcat, "danger", "not danger") %>% as.factor()

## ASSET OWNERSHIP

dat$ASSET_OWNERSHIP <- as.integer(dat$ASSET_OWNERSHIP)

## Emi taken before

dat$has_taken_emi_before[is.na(dat$has_taken_emi_before)] <- 0

dat$has_taken_emi_before <- as.factor(dat$has_taken_emi_before)
 

dat$count_of_emi_before[is.na(dat$count_of_emi_before)] <- 0


model <- glm(target_variable ~ transaction_amount + revolve_interest_rate + credit_limit + age +
               spends_12m + util_6m + payment_ratio_6m + paymad_1m + paymad_6m + factor(has_taken_emi_before) + (count_of_emi_before) + 
               Bureau_all_amt_ever + product + new_merchant_category + new_merchant_country + new_merchant_name +
               Hotels_1m + Electronics_1m + Retail_1m + Fuel_1m + RentPayments_1m + DeptStores_1m + Insurance_1m + Utility_1m +
               Hotels_3m + Electronics_3m + Retail_3m + Fuel_3m + RentPayments_3m + DeptStores_3m + Insurance_3m + Utility_3m +
               Hotels_amt_1m + Electronics_amt_1m + Retail_amt_1m + Fuel_amt_1m + RentPayments_amt_1m + DeptStores_amt_1m + Insurance_amt_1m + Utility_amt_1m +
               Hotels_amt_3m + Electronics_amt_3m + Retail_amt_3m + Fuel_amt_3m + RentPayments_amt_3m + DeptStores_amt_3m + Insurance_amt_3m + Utility_amt_3m,
             data = dat, 
             family = binomial(link = "logit"))

summary(model)

dat$modelprob <- predict(model, dat , type = "response")
dat <- dat  %>% mutate(model_pred = 1*(modelprob >= 0.5) + 0)
dat <- dat %>% mutate(accurate = 1*(model_pred == target_variable) + 0)

sum(dat$accurate)/nrow(dat)

t <- table(dat$target_variable, dat$model_pred)/nrow(dat)
accuracy <- sum(dat$accurate)/nrow(dat)
precision <- t[2,2]/(t[2,2] + t[2,1])
recall <- t[2,2]/(t[2,2] + t[1,2])
F1_score <- ( 2 * recall * precision )/ (recall + precision)

accuracy; precision ; recall; F1_score


sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train  <- subset(dat, sample == TRUE)
test   <- subset(dat, sample == FALSE)

library(DMwR)

train$new_target <- as.factor(train$target_variable)
df <- SMOTE(new_target ~ ., train, perc.over = 1000, perc.under = 200, k = 5)


model <- glm(target_variable ~ transaction_amount + revolve_interest_rate + credit_limit + age +
               spends_12m + util_6m + payment_ratio_6m + paymad_1m + paymad_6m + factor(has_taken_emi_before) + (count_of_emi_before) + 
               Bureau_all_amt_ever + product + new_merchant_category + new_merchant_country + new_merchant_name +
               Hotels_1m + Electronics_1m + Retail_1m + Fuel_1m + RentPayments_1m + DeptStores_1m + Insurance_1m + Utility_1m +
               Hotels_3m + Electronics_3m + Retail_3m + Fuel_3m + RentPayments_3m + DeptStores_3m + Insurance_3m + Utility_3m +
               Hotels_amt_1m + Electronics_amt_1m + Retail_amt_1m + Fuel_amt_1m + RentPayments_amt_1m + DeptStores_amt_1m + Insurance_amt_1m + Utility_amt_1m +
               Hotels_amt_3m + Electronics_amt_3m + Retail_amt_3m + Fuel_amt_3m + RentPayments_amt_3m + DeptStores_amt_3m + Insurance_amt_3m + Utility_amt_3m,
             data = df, 
             family = binomial(link = "logit"))

summary(model)



df$modelprob <- predict(model, df , type = "response")
df <- df  %>% mutate(model_pred = 1*(modelprob >= 0.5) + 0)
df <- df %>% mutate(accurate = 1*(model_pred == target_variable) + 0)

sum(df$accurate)/nrow(df)

t <- table(df$target_variable, df$model_pred)/nrow(df)
accuracy <- sum(df$accurate)/nrow(df)
precision <- t[2,2]/(t[2,2] + t[2,1])
recall <- t[2,2]/(t[2,2] + t[1,2])
F1_score <- ( 2 * recall * precision )/ (recall + precision)

accuracy; precision ; recall; F1_score



## predictions for test

test$modelprob <- predict(model, test , type = "response")
test <- test  %>% mutate(model_pred = 1*(modelprob >= 0.5) + 0)
test <- test %>% mutate(accurate = 1*(model_pred == target_variable) + 0)

accuracy <- sum(test$accurate)/nrow(test)

t <- table(test$target_variable, test$model_pred)/nrow(test)
accuracy <- sum(test$accurate)/nrow(test)
precision <- t[2,2]/(t[2,2] + t[2,1])
recall <- t[2,2]/(t[2,2] + t[1,2])
F1_score <- ( 2 * recall * precision )/ (recall + precision)

accuracy; precision ; recall; F1_score


####################### actual train data ########################################

dat$new_target <- as.factor(dat$target_variable)
df <- SMOTE(new_target ~ ., dat, perc.over = 1000, perc.under = 200, k = 5)

model <- glm(target_variable ~ transaction_amount + revolve_interest_rate + credit_limit + age +
               spends_12m + util_6m + payment_ratio_6m + paymad_1m + paymad_6m +
               Bureau_all_amt_ever + product + new_merchant_category + new_merchant_country + new_merchant_name + factor(has_taken_emi_before) + (count_of_emi_before) + 
               Hotels_1m + Electronics_1m + Retail_1m + Fuel_1m + RentPayments_1m + DeptStores_1m + Insurance_1m + Utility_1m +
               Hotels_3m + Electronics_3m + Retail_3m + Fuel_3m + RentPayments_3m + DeptStores_3m + Insurance_3m + Utility_3m +
               Hotels_amt_1m + Electronics_amt_1m + Retail_amt_1m + Fuel_amt_1m + RentPayments_amt_1m + DeptStores_amt_1m + Insurance_amt_1m + Utility_amt_1m +
               Hotels_amt_3m + Electronics_amt_3m + Retail_amt_3m + Fuel_amt_3m + RentPayments_amt_3m + DeptStores_amt_3m + Insurance_amt_3m + Utility_amt_3m,
             data = df, 
             family = binomial(link = "logit"))

summary(model)



df$modelprob <- predict(model, df , type = "response")
df <- df  %>% mutate(model_pred = 1*(modelprob >= 0.5) + 0)
df <- df %>% mutate(accurate = 1*(model_pred == target_variable) + 0)

sum(df$accurate)/nrow(df)

t <- table(df$target_variable, df$model_pred)/nrow(df)
accuracy <- sum(df$accurate)/nrow(df)
precision <- t[2,2]/(t[2,2] + t[2,1])
recall <- t[2,2]/(t[2,2] + t[1,2])
F1_score <- ( 2 * recall * precision )/ (recall + precision)

accuracy; precision ; recall; F1_score



## predictions for validation data

dat <- read.csv("case_study_validation.csv")


## Handling NA values
na_count <- sapply(dat, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)

## Omiting columns which have more than 35000 NA value.
na_count <- subset(na_count, na_count < 15000) %>% as.data.frame()

dat <- dat[, c(row.names(na_count), "has_taken_emi_before", "count_of_emi_before")]


## Finding the columns with NA values

new_na_count <- subset(na_count, na_count > 0) %>% as.data.frame()
na_dat <- dat[, row.names(new_na_count)]

########################### Imputation of missing values #####################################

## credit limit

plot(table(na_dat$credit_limit))           ## not symmetric
median(na.omit(na_dat$credit_limit))      ## median better replacement for 
dat$credit_limit[is.na(dat$credit_limit)] <- median(na.omit(na_dat$credit_limit))

## revolve

table(na_dat$revolve_1m) 
table(na_dat$revolve_3m) 
table(na_dat$revolve_6m) 

dat$revolve_1m[is.na(dat$revolve_1m)] <- 0
dat$revolve_3m[is.na(dat$revolve_3m)] <- 0
dat$revolve_6m[is.na(dat$revolve_6m)] <- 0

# The number of customers that have revolved has increased over months


## 
mode <- function(x, na.rm = FALSE) {
  
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

mode(na.omit(na_dat$util_1m))
mode(na.omit(na_dat$util_3m))
mode(na.omit(na_dat$util_6m))

dat$util_1m[is.na(dat$util_1m)] <- mode(na.omit(na_dat$util_1m))
dat$util_3m[is.na(dat$util_3m)] <- mode(na.omit(na_dat$util_3m))
dat$util_6m[is.na(dat$util_6m)] <- mode(na.omit(na_dat$util_6m))

## payment

mode(na.omit(na_dat$payment_ratio_1m))
median(na.omit(na_dat$payment_ratio_1m))

mode(na.omit(na_dat$payment_ratio_3m))
median(na.omit(na_dat$payment_ratio_3m))

mode(na.omit(na_dat$payment_ratio_6m))
median(na.omit(na_dat$payment_ratio_6m))

dat$payment_ratio_1m[is.na(dat$payment_ratio_1m)] <- mode(na.omit(na_dat$payment_ratio_1m))
dat$payment_ratio_3m[is.na(dat$payment_ratio_3m)] <- mode(na.omit(na_dat$payment_ratio_3m))
dat$payment_ratio_6m[is.na(dat$payment_ratio_6m)] <- mode(na.omit(na_dat$payment_ratio_6m))

## paymed

mode(na.omit(na_dat$paymad_1m))
mode(na.omit(na_dat$paymad_3m))
mode(na.omit(na_dat$paymad_6m))

dat$paymad_1m[is.na(dat$paymad_1m)] <- mode(na.omit(na_dat$paymad_1m))
dat$paymad_3m[is.na(dat$paymad_3m)] <- mode(na.omit(na_dat$paymad_3m))
dat$paymad_6m[is.na(dat$paymad_6m)] <- mode(na.omit(na_dat$paymad_6m))

## Age

## age is given negative. making it positive

median(-na.omit(dat$age))
mode_cont <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found onRSiteSearch)	
  x<-data
  lim.inf=min(x)-1; lim.sup=max(x)+1
  
  hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.2))
  s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
  n<-length(s$y)
  v1<-s$y[1:(n-2)];
  v2<-s$y[2:(n-1)];
  v3<-s$y[3:n]
  ix<-1+which((v1<v2)&(v2>v3))
  
  lines(s$x,s$y,col="red")
  points(s$x[ix],s$y[ix],col="blue")
  
  md <- s$x[which(s$y==max(s$y))] 
  
  md
}

mode_cont(na.omit(dat$age))
dat$age[is.na(dat$age)] <- mode_cont(na.omit(dat$age))
dat$age <- -dat$age

## bureau all amt ever
mean(na.omit(na_dat$Bureau_all_amt_ever))
mode(na.omit(na_dat$Bureau_all_amt_ever))
median(na.omit(na_dat$Bureau_all_amt_ever))

## x <- na_dat$Bureau_all_amt_ever[(na_dat$Bureau_all_amt_ever) < 5e7]
## t <- table(na.omit(na_dat$Bureau_all_amt_ever))

## 2000 and 11700000 has very similar frequency and the come in 1st nd secind position in the frequency distribution.
## but most of the high freq loan amt is over 1 crores.
## meadian is 1531777 and mean is 7890301. so taking mode as a replacement

#t %>%
#  as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab

dat$Bureau_all_amt_ever[is.na(dat$Bureau_all_amt_ever)] <- mode(na.omit(na_dat$Bureau_all_amt_ever))

## bureau all amt live

mean(na.omit(na_dat$Bureau_all_amt_live))
mode(na.omit(na_dat$Bureau_all_amt_live))
median(na.omit(na_dat$Bureau_all_amt_live))

#t <- table(na.omit(na_dat$Bureau_all_amt_live))
#t %>%
# as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab

#hist(na_dat$Bureau_all_amt_live[na_dat$Bureau_all_amt_live < 1e4])

dat$Bureau_all_amt_live[is.na(dat$Bureau_all_amt_live)] <- mode(na.omit(na_dat$Bureau_all_amt_live))

## avg sa balance

#hist(na.omit(na_dat$avg_sa_balance_1m))

mode(na.omit(na_dat$avg_sa_balance_1m))
median(na.omit(na_dat$avg_sa_balance_1m))
mean(na.omit(na_dat$avg_sa_balance_1m))

dat$avg_sa_balance_1m[is.na(dat$avg_sa_balance_1m)] <- median(na.omit(na_dat$avg_sa_balance_1m))
dat$avg_sa_balance_3m[is.na(dat$avg_sa_balance_3m)] <- median(na.omit(na_dat$avg_sa_balance_3m))
dat$avg_sa_balance_6m[is.na(dat$avg_sa_balance_6m)] <- median(na.omit(na_dat$avg_sa_balance_6m))
dat$avg_sa_balance_12m[is.na(dat$avg_sa_balance_12m)] <- median(na.omit(na_dat$avg_sa_balance_12m))

## pctchg curr sa avg sa

## have to take advice

#hist(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
#hist(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m[na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m < 20])

#t <- table(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

#t %>%
#  as.data.frame() %>% 
#  arrange(desc(Freq))
#sorted_tab


mean(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
median(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))


dat$pctchg_curr_sa_bal_avg_sa_bal_1m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
dat$pctchg_curr_sa_bal_avg_sa_bal_3m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_3m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m))
dat$pctchg_curr_sa_bal_avg_sa_bal_6m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_6m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

#sum(is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m))

### checking

n_na_count <- sapply(dat, function(y) sum(is.na(y)))
n_na_count <- data.frame(n_na_count)


dat$merchant_name <- as.factor(dat$merchant_name)
dat$merchant_country <- as.factor(dat$merchant_country)
dat$Merchant_category <- as.factor(dat$Merchant_category)
dat$product <- as.factor(dat$product)

# merchant name

dat$new_merchant_name <- ifelse(dat[, 3] == c("STAR BAZAAR", "RAJA RAJESHWARI BAR AND", "DMART BOMMASANDRA", 
                                              "AAROGYAINDIAHEACASHFREE", "NAGARJUNA SCHOOL", "FILOS HOSPITALITY", 
                                              "ANBU UNAVAGAM", "PAYTM", "APOLLO PHARMACY", "PAYTM RENTAL"), "danger", "not danger") %>% as.factor()

## merchant country

dat$new_merchant_country <- ifelse(dat[, 4] == "IN", "danger", "not danger") %>% as.factor()

## merchant category

dat$new_merchant_category <- ifelse(dat[, 5] == ymcat, "danger", "not danger") %>% as.factor()

## ASSET OWNERSHIP

dat$ASSET_OWNERSHIP <- as.integer(dat$ASSET_OWNERSHIP)

## Emi taken before

dat$has_taken_emi_before[is.na(dat$has_taken_emi_before)] <- 0

dat$has_taken_emi_before <- as.factor(dat$has_taken_emi_before)
## Count EMI taken before <- 

dat$count_of_emi_before[is.na(dat$count_of_emi_before)] <- 0

## probability predictions 

dat$modelprob <- predict(model, dat, type = "response")

output <- dat[, c("primary_key", "modelprob")]

write.csv(output, file = "Validation_Probabilities.csv")



