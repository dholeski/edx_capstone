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

#fedrates <- read_csv('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\fed-funds-rate-historical-chart.csv',col_types = cols(date = col_date(format = "%m/%d/%Y"),value = col_double())) %>% filter(year(date) >= 1990)

#download historical fed rates from kaggle.
fedrates <- read_csv('https://storage.googleapis.com/kagglesdsdata/datasets/1166232/1953977/fed-funds-rate-historical-chart.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210218%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210218T004044Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=3af56bc0f190fc6b6102495fd3c5afbf79fa714350fc68ab4a8fc02e011218c259c63cdb3918c01996e4cf3954464bffcbad534ec5d2be0fa3e67e311bd2e1b4e94013fb0f727a9d7a1310d26cf8ef1aa6242be28499da0a9012695e4c1b372e78bb315cc632a5c971bb1201aa1c61aa898c0861f24654abbd82a0391990691ddc2bfa03e48a4ea8cfae3f003725eb2bbd058530f86c399449c9575f3ed885ffca52a278d47fc110448623bb1bedecb7cb07ec33a8101f008f4b55f03ce8b3019cb2ef7039250ed2b4e9cc792562d123c34673348a00dd430813273956d1c34bca904206a3b9f3c648682ac9e6459c0f09dc221201a9d423596b861a8fea959d',col_types = cols(date = col_date(format = "%m/%d/%Y"),value = col_double())) %>% filter(year(date) >= 1990) 

fedrates <- data.table(fedrates)

names(fedrates)[names(fedrates)=='value'] <- 'fed_rate'

#clean dataset and remove irrelavent columns. AVERAGE CREDIT PROCESSING IS 30 DAYS
accepted_lc <- accept_lc %>% select(-member_id,-desc,-url,-mths_since_last_delinq,-mths_since_last_record,-next_pymnt_d,-mths_since_last_major_derog,-dti_joint,-annual_inc_joint,-verification_status_joint,-mths_since_recent_revol_delinq,-purpose,-title,-revol_bal_joint,-emp_title,-c(116:144)) %>% 
  mutate(issue_d = parse_date_time(issue_d, orders = 'Ymd HMS')) %>% filter(
    issue_d >= '2012-1-1' &
      application_type == 'Individual' &
      loan_status %in% c('Charged Off', 'Fully Paid'))  %>% 
  mutate(
    thrtyDaysB4Issue_d = issue_d - days(30),
    charge_off_fctr = as.factor(ifelse(loan_status == 'Charged Off', 1, 0)),
    calc_tot_amount = pmt * term_months,
    avg_fico_hilow = (last_fico_range_low + last_fico_range_high)/2,
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

accepted_lc_rate <- left_join(accepted_lc,fedrates,copy=FALSE,by=c('thrtyDaysB4Issue_d' = 'date'))

setDT(accepted_lc)[,join_date := thrtyDaysB4Issue_d]
setDT(fedrates)[,join_date := date]
accepted_lc_rate <- accepted_lc[fedrates, on = .(thrtyDaysB4Issue_d = date),roll = 'nearest']

accepted_lc_rate <- accepted_lc_rate %>% filter(!is.na(fed_rate))

set.seed(1,sample.kind = 'Rounding')
train_index <- createDataPartition(accepted_lc$loan_status, p=0.8, list=FALSE)
train_set <- accepted_lc[train_index,]
test_set <- accepted_lc[-train_index,]


##logistic regression, binomial
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

