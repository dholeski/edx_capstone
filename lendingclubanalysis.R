## lending club data

library(tidyverse)
library(data.table)
library(caret)
library(randomForest)
library(readr)
library(lubridate)

##accept_lc <- read_csv('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\lc_accepted_clean.csv',col_types = cols(member_id = col_skip(), desc = col_skip(), zip_code = col_skip()))

accept_lc <- fread('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\lc_accepted_clean.csv')

saveRDS(accept_lc, 'C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\accept_lc.rds')

accepted_lc <- accept_lc %>% select(-member_id,-desc,-zip_code,-url,-mths_since_last_delinq,-mths_since_last_record,-next_pymnt_d,-mths_since_last_major_derog,-dti_joint,-annual_inc_joint,-verification_status_joint,-mths_since_recent_revol_delinq,-revol_bal_joint,-c(116:144)) %>% mutate(issue_d = parse_date_time(issue_d,orders = 'Ymd HMS')) %>% filter(issue_d >= '2012-1-1' & application_type == 'Individual')  %>% mutate(calc_tot_amount = pmt * term_months, expc_interest = calc_tot_amount - funded_amnt,int_rate = int_rate / 100,calc_perc_lst_pymt = last_pymnt_amnt / funded_amnt, grade = as.factor(grade),sub_grade = as.factor(sub_grade),emp_length = as.factor(emp_length),home_ownership = as.factor(home_ownership),verification_status = as.factor(verification_status),loan_status = as.factor(loan_status),pymnt_plan = as.factor(pymnt_plan),addr_state = as.factor(addr_state), earliest_cr_line = parse_date_time(earliest_cr_line,orders = 'Ymd HMS'),initial_list_status = as.factor(initial_list_status),last_pymnt_d = parse_date_time(last_pymnt_d,orders = 'Ymd HMS'),last_credit_pull_d = parse_date_time(last_credit_pull_d,orders = 'Ymd HMS'),policy_code=as.factor(policy_code),dti_undr_36 = ifelse(dti < 36,1,0),annual_debt = annual_inc * dti, issue_year = year(issue_d),lngth_credit_mnths = (issue_d - earliest_cr_line)/365*12,fico_low_zscore = (last_fico_range_low - mean(last_fico_range_low,na.rm = TRUE)) / sd(last_fico_range_low,na.rm = TRUE))

reject_lc <- read_csv('C:\\Users\\Dennis\\Desktop\\edx_capstone\\lendclub\\lc_rejected_clean.csv')

#issue_d >= year(2012)&


