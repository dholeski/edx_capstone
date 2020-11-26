data("zip_code_db")

airbnb <- Arbnb_dt %>% group_by(host_id) %>% mutate(num_host_props = n()) %>%  ungroup() %>% group_by(neighbourhood) %>% mutate(num_props_group = n()) %>%  ungroup() %>% mutate(calc_check = ifelse(calculated_host_listings_count == num_host_props,TRUE,FALSE),last_review = lubridate::parse_date_time(last_review,orders = "dmy"))

# zip_code_db <- zip_code_db %>% filter(zipcode_type == "Standard")

zipdata_bnb <-  subset(zip_code_db,major_city %in% airbnb$city) 

zipdata_bnb <- zip_code_db %>% filter(!is.na(bounds_west)) %>% select(-common_city_list,-area_code_list)

zipdata_bnb <- zipdata_bnb %>% filter(state %in% bnbstates)

abnb_nozip <- airbnb %>% filter(is.na(neighbourhood))

a_zipcount <- airbnb %>% group_by(city) %>%  summarize(n=n())



write_labelled_xlsx(zipdata_bnb,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\zipdata_bnbstates.xlsx")

# write_labelled_xlsx(abnb_nozip,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\abnb_nozip.xlsx")

# longitude -- larger west to smaller east
# latitude -- larger north to smaller south

lim_abnb <- airbnb[1:100,]

lat <- 32.6000
long <- -85.2000


zpdf <- as.data.frame(zipdata_bnb)
abdf <- as.data.frame(airbnb)


arbnb_zip_join1 <- setDT(zipdata_bnb)[data.table(airbnb),on = .( bounds_west<=  longitude, bounds_east >=longitude  ,  bounds_north >=latitude, bounds_south <=latitude )]

arbnb_zip_join <- setDT(airbnb)[data.table(zipdata_bnb),on = .(longitude >= bounds_west, longitude <= bounds_east,  latitude <=bounds_north, latitude >=bounds_south ),nomatch =0]




zipdata_bnb$state <-sub('.*,\\s*', '', zipdata_bnb$post_office_city)


grp_ranked <- arbnb_zip_join %>% mutate(zipcode_fill = ifelse(is.na(neighbourhood),zipcode,neighbourhood)) %>%  group_by(id) %>% mutate(id_rank = order(median_household_income,decreasing = TRUE) , lag_rank = lag(id_rank,n=1L,order_by = id,default = 1)) %>% ungroup()



bnbstates<- c("MA","RI","CT","NY","NJ","DC","OH","NC","TN","IL","MN","CO","WA","OR","NV","CA","TX","LA","FL","HI")