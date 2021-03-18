library(tidyverse)
library(jsonlite)
library(data.table)

finmodprepAPI <- 'c4ff7413dae8e0f35450dba77c2a2e51'

fortune500list <- read_csv('C:/Users/Dennis/Desktop/edx_capstone/fortune500list.csv') %>% data.table(.)

exchanges <- c('NYSE', 'Nasdaq Global Select', 'New York Stock Exchange', 'Nasdaq Global Market', 'Nasdaq capital Market', 'NYSE American','NASDAQ Global Market')

colnames(fortune500list)[colnames(fortune500list) == 'Revenues (millions)'] <- 'Revenue_Millions'

SP_symbols <- unique(SP500Merged$symbol)



daily_dcf <-  read_json(paste('https://financialmodelingprep.com/api/v3/historical-daily-discounted-cash-flow/',symbols,'?limit=100&apikey=c4ff7413dae8e0f35450dba77c2a2e51',sep = ' '),simplifyVector = TRUE)

earnings_sup <- read_json('https://financialmodelingprep.com/api/v3/earnings-surpises/CVX?apikey=c4ff7413dae8e0f35450dba77c2a2e51', simplifyVector = TRUE) %>% mutate(surprise = actualEarningResult - estimatedEarning,surprise_pct = round(surprise / estimatedEarning,2))


avail_symbs <- read_json('https://financialmodelingprep.com/api/v3/stock/list?apikey=c4ff7413dae8e0f35450dba77c2a2e51',simplifyVector = TRUE) 
  #filter(exchange %IN% c('NYSE', 'New York Stock Exchange', 'Other OTC','Nasdaq Global Select','Nasdaq Global Market', 'NYSE American'))

avail <- data.table(avail_symbs)[!`symbol` %like% '%.%']

incStms <- read_json(
  paste('https://financialmodelingprep.com/api/v3/income-statement/',symbols,'?limit=120&apikey=c4ff7413dae8e0f35450dba77c2a2e51', sep = '')
  ,simplifyVector = TRUE)

incStm_gwth <- read_json(
  paste('https://financialmodelingprep.com/api/v3/income-statement-growth/',symbols,'?limit=40&apikey=c4ff7413dae8e0f35450dba77c2a2e51', sep = '')
  ,simplifyVector = TRUE)

symbols <- c('AMD', 'INTC','NVDA')

datalist = list()

for (i in symbols) {
  # ... make some data
  incStm_gwth <- read_json(
    paste('https://financialmodelingprep.com/api/v3/income-statement-growth/',i,'?limit=40&apikey=c4ff7413dae8e0f35450dba77c2a2e51', sep = '')
    ,simplifyVector = TRUE)
  datalist[[i]] <- incStm_gwth # add it to your list
}

big_data = do.call(rbind, datalist)

for (i in SP_symbols) {
  # ... make some data
  incStm <- read_json(
    paste('https://financialmodelingprep.com/api/v3/income-statement-growth/',i,'?limit=40&apikey=c4ff7413dae8e0f35450dba77c2a2e51', sep = '')
    ,simplifyVector = TRUE)
  datalist[[i]] <- incStm # add it to your list
}

big_income_data <- do.call(rbind, datalist)
