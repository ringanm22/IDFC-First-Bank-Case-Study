library(dplyr)
library(modeest)

## Reading the devdata csv 
dat <- read.csv("case_study_devdata.csv")


## Handling NA values
na_count <- sapply(dat, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)

## Omiting columns which have more than 35000 NA value.
na_count <- subset(na_count, na_count < 15000) %>% as.data.frame()

dat <- dat[, row.names(na_count)]

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

dat$util_1m[is.na(dat$util_1m)] <- 0
dat$util_3m[is.na(dat$util_3m)] <- 0
dat$util_6m[is.na(dat$util_6m)] <- 0

## payment

mode(na.omit(na_dat$payment_ratio_1m))
median(na.omit(na_dat$payment_ratio_1m))

mode(na.omit(na_dat$payment_ratio_3m))
median(na.omit(na_dat$payment_ratio_3m))

mode(na.omit(na_dat$payment_ratio_6m))
median(na.omit(na_dat$payment_ratio_6m))

dat$payment_ratio_1m[is.na(dat$payment_ratio_1m)] <- 1
dat$payment_ratio_3m[is.na(dat$payment_ratio_3m)] <- 1
dat$payment_ratio_6m[is.na(dat$payment_ratio_6m)] <- 1

## paymed

mode(na.omit(na_dat$paymad_1m))
mode(na.omit(na_dat$paymad_3m))
mode(na.omit(na_dat$paymad_6m))

plot(table(na.omit(na_dat$paymad_6m)))

sum(na.omit(na_dat$paymad_6m) == 0)

dat$paymad_1m[is.na(dat$paymad_1m)] <- 1
dat$paymad_3m[is.na(dat$paymad_3m)] <- 1
dat$paymad_6m[is.na(dat$paymad_6m)] <- 20

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

plot(density(-na.omit(dat$age)))
mode_cont(na.omit(dat$age))

dat$age[is.na(dat$age)] <- mode_cont(na.omit(dat$age))
dat$age <- -dat$age

## bureau all amt ever
mean(na.omit(na_dat$Bureau_all_amt_ever))
mode(na.omit(na_dat$Bureau_all_amt_ever))
median(na.omit(na_dat$Bureau_all_amt_ever))

x <- na_dat$Bureau_all_amt_ever[(na_dat$Bureau_all_amt_ever) < 5e7]

t <- table(na.omit(na_dat$Bureau_all_amt_ever))

## 2000 and 11700000 has very similar frequency and the come in 1st nd secind position in the frequency distribution.
## but most of the high freq loan amt is over 1 crores.
## meadian is 1531777 and mean is 7890301. so taking mode as a replacement

t %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
sorted_tab

dat$Bureau_all_amt_ever[is.na(dat$Bureau_all_amt_ever)] <- mode(na.omit(na_dat$Bureau_all_amt_ever))

## bureau all amt live

mean(na.omit(na_dat$Bureau_all_amt_live))
mode(na.omit(na_dat$Bureau_all_amt_live))
median(na.omit(na_dat$Bureau_all_amt_live))

t <- table(na.omit(na_dat$Bureau_all_amt_live))
t %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
sorted_tab

hist(na_dat$Bureau_all_amt_live[na_dat$Bureau_all_amt_live < 1e4])

dat$Bureau_all_amt_live[is.na(dat$Bureau_all_amt_live)] <- mode(na.omit(na_dat$Bureau_all_amt_live))

## avg sa balance

hist(na.omit(na_dat$avg_sa_balance_1m))

mode(na.omit(na_dat$avg_sa_balance_1m))
median(na.omit(na_dat$avg_sa_balance_1m))
mean(na.omit(na_dat$avg_sa_balance_1m))

dat$avg_sa_balance_1m[is.na(dat$avg_sa_balance_1m)] <- median(na.omit(na_dat$avg_sa_balance_1m))
dat$avg_sa_balance_3m[is.na(dat$avg_sa_balance_3m)] <- median(na.omit(na_dat$avg_sa_balance_3m))
dat$avg_sa_balance_6m[is.na(dat$avg_sa_balance_6m)] <- median(na.omit(na_dat$avg_sa_balance_6m))
dat$avg_sa_balance_12m[is.na(dat$avg_sa_balance_12m)] <- median(na.omit(na_dat$avg_sa_balance_12m))

## pctchg curr sa avg sa

## have to take advice

hist(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
hist(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m[na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m < 20])

t <- table(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

t %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
sorted_tab

plot


mean(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
median(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))


dat$pctchg_curr_sa_bal_avg_sa_bal_1m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_1m))
dat$pctchg_curr_sa_bal_avg_sa_bal_3m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_3m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_3m))
dat$pctchg_curr_sa_bal_avg_sa_bal_6m[is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_6m)] <- mode(na.omit(na_dat$pctchg_curr_sa_bal_avg_sa_bal_6m))

sum(is.na(dat$pctchg_curr_sa_bal_avg_sa_bal_1m))

### checking

n_na_count <- sapply(dat, function(y) sum(is.na(y)))
n_na_count <- data.frame(n_na_count)

View(dat)

########################## Spliting the devdata dataset into test and train sets. ##################################
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train  <- subset(dat, sample == TRUE)
test   <- subset(dat, sample == FALSE)

train <- na.omit(train)
log_reg <- glm(target_variable ~., data = train, family = binomial)

 View(train)

sum(is.na(train))

install.packages("xfun")

