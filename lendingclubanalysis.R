## lending club data

library(tidyverse)
library(data.table)
library(caret)
library(randomForest)
library(readr)
library(lubridate)
library(Rborist)

##accept_lc <- read_csv('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\lc_accepted_clean.csv',col_types = cols(member_id = col_skip(), desc = col_skip(), zip_code = col_skip()))

accept_lc <- fread('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\lc_accepted_clean.csv')
# 
# saveRDS(accept_lc, 'C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\accept_lc.rds')
# 
accept_lc <- readRDS('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\accept_lc.rds')

fedrates <- read_csv('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\fed-funds-rate-historical-chart.csv',col_types = cols(date = col_date(format = "%m/%d/%Y"),value = col_double())) %>% filter(year(date) >= 1990)

names(fedrates)[names(fedrates)=='value'] <- 'fed_rate'



accepted_lc <- accept_lc %>% select(-member_id,-desc,-zip_code,-url,-mths_since_last_delinq,-mths_since_last_record,-next_pymnt_d,-mths_since_last_major_derog,-dti_joint,-annual_inc_joint,-verification_status_joint,-mths_since_recent_revol_delinq,-purpose,-title,-revol_bal_joint,-emp_title,-c(116:144)) %>% 
  mutate(issue_d = parse_date_time(issue_d, orders = 'Ymd HMS')) %>% filter(
    issue_d >= '2012-1-1' &
      application_type == 'Individual' &
      loan_status %in% c('Charged Off', 'Fully Paid'))  %>% 
  mutate(
    thrtyDaysB4Issue_d = issue_d - days(30),
    charge_off_fctr = as.factor(ifelse(loan_status == 'Charged Off', 1, 0)),
    calc_tot_amount = pmt * term_months,
    expc_interest = calc_tot_amount - funded_amnt,
    int_rate = int_rate / 100,
    calc_perc_lst_pymt =  funded_amnt / last_pymnt_amnt ,
    grade = as.numeric(as.factor(grade)),
    sub_grade = as.factor(sub_grade),
    emp_length = as.factor(emp_length),
    home_ownership = as.factor(home_ownership),
    verification_status = as.factor(verification_status),
    loan_status = as.factor(loan_status),
    pymnt_plan = as.factor(pymnt_plan),
    addr_state = as.factor(addr_state),
    earliest_cr_line = parse_date_time(earliest_cr_line, orders = 'Ymd HMS'),
    initial_list_status = as.factor(initial_list_status),
    last_pymnt_d = parse_date_time(last_pymnt_d, orders = 'Ymd HMS'),
    last_credit_pull_d = parse_date_time(last_credit_pull_d, orders = 'Ymd HMS'),
    policy_code = as.factor(policy_code),
    dti_ovr_36 = as.factor(ifelse(dti > 36, 1, 0)),
    annual_debt = annual_inc * dti,
    issue_year = year(issue_d),
    lngth_credit_mnths = (issue_d - earliest_cr_line) / 365 * 12,
    fico_low_zscore = (last_fico_range_low - mean(last_fico_range_low, na.rm = TRUE)) / sd(last_fico_range_low, na.rm = TRUE)
  )

accepted_lc <- left_join(accepted_lc,fedrates,copy=FALSE,by=c('thrtyDaysB4Issue_d' = 'date')) 

accepted_lc <- accepted_lc %>% filter(!is.na(fed_rate))

set.seed(1,sample.kind = 'Rounding')
train_index <- createDataPartition(accepted_lc$loan_status, p=0.8, list=FALSE)
train_set <- accepted_lc[train_index,]
test_set <- accepted_lc[-train_index,]


##logistic regression
glm_chrgoff <- train_set %>% mutate(y=as.numeric(charge_off_fctr == 1)) %>% glm(y~int_rate+dti+term_months+last_fico_range_low+fed_rate, data = . , family = 'binomial')


predict_glm <- predict(glm_chrgoff, newdata = test_set, type="response")

y_hat_glm <- ifelse(predict_glm > 0.5, 1, 0) %>% factor

confusionMatrix(y_hat_glm,test_set$charge_off_fctr)

qplot(y_hat_glm,predict_glm)

##k-nearest neighbor
knn_fit <- train_set %>% mutate(charge_off_fctr=as.numeric(charge_off_fctr == 1)) %>% knn3(charge_off_fctr~int_rate+dti+term_months+last_fico_range_low+fed_rate, data = . ,k = 5)

predict_knn <- predict(knn_fit,new_data = test_set,type = 'class')


####classification tree
train_rpart <- train(charge_off_fctr~int_rate+dti+term_months+last_fico_range_low+fed_rate,method = 'rpart',tuneGrid = data.frame(cp = seq(0.0,0.1,len=25)),data = train_set)

plot(train_rpart)

confusionMatrix(predict(train_rpart,test_set),test_set$charge_off_fctr)$overall['Accuracy']

###Random Forest
rf_fit <- train_set %>% mutate(charge_off_fctr=as.numeric(charge_off_fctr == 1)) %>% randomForest(charge_off_fctr~int_rate+dti+term_months+last_fico_range_low+fed_rate,data = .)

