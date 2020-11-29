data("zip_code_db")
data("state.x77")
data("state_codes")

airbnb <- Arbnb_dt %>% group_by(host_id) %>% mutate(num_host_props = n()) %>%  ungroup() %>% group_by(neighbourhood) %>% mutate(num_props_group = n()) %>%  ungroup() %>% mutate(calc_check = ifelse(calculated_host_listings_count == num_host_props,TRUE,FALSE),last_review = lubridate::parse_date_time(last_review,orders = "dmy"))

# zip_code_db <- zip_code_db %>% filter(zipcode_type == "Standard")

zip_code_db$state <- sub('.*,\\s*', '', zip_code_db$post_office_city)

zipdata_bnb <-  subset(zip_code_db,major_city %in% airbnb$city) 

zipdata_bnb1 <- zip_code_db %>% filter(!is.na(bounds_west)) %>% select(-common_city_list,-area_code_list) %>% left_join(state_codes[,c("state_name","state_abbr")],by = c("state" = "state_abbr"))

zipdata_bnb <- zipdata_bnb %>% filter(state %in% bnbstates)

abnb_nozip <- airbnb %>% filter(is.na(neighbourhood))

a_zipcount <- airbnb %>% group_by(city) %>%  summarize(n=n())


################################
CalcDistanceMiles <- function(Lat1, Long1 , Lat2 , Long2) {
  Lat1 <- format(Lat1,nsmall=5)
  Long1 <- format(Long1, nsmall= 5)
  Lat2 <- format(Lat2,nsmall = 5)
  Long2 <- format(Long2,nsmall = 5)
  ### convert to radians ###

  Lat1 <- Lat1 / 57.2958
  Long1 <- Long1 / 57.2958
  Lat2 <- Lat2 / 57.2958
  Long2 <- Long2 / 57.2958
  ## Calc distance
  distance < (sin(Lat1) * sin(Lat2)) + (cos(Lat1) * cos(Lat2) * cos(Long2 - Long1))
  ## Convert to miles
  miles <- ifelse(distance != 0, 3958.75 * Atan(Sqrt(1 - distance**2) / distance), NA)
  return(miles)
}
###########################



write_labelled_xlsx(zipdata_bnb,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\zipdata_bnbstates.xlsx")

# write_labelled_xlsx(abnb_nozip,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\abnb_nozip.xlsx")

# longitude -- larger west to smaller east
# latitude -- larger north to smaller south

boundzips <- USAboundaries::us_zipcodes()

boundzips2 <- st_drop_geometry(boundzips2)
boundzips2 <- boundzips %>% unnest_wider(geometry)



arbnb_zip_join1 <- setDT(zipdata_bnb)[data.table(airbnb),on = .( bounds_west<=  longitude, bounds_east >=longitude  ,  bounds_north >=latitude, bounds_south <=latitude )]

arbnb_zip_join <- setDT(airbnb)[data.table(zipdata_bnb),on = .(longitude >= bounds_west, longitude <= bounds_east,  latitude <=bounds_north, latitude >=bounds_south ),nomatch =0]




zipdata_bnb$state <-sub('.*,\\s*', '', zipdata_bnb$post_office_city)


grp_ranked <- arbnb_zip_join %>% mutate(zipcode_fill = ifelse(is.na(neighbourhood),zipcode,neighbourhood)) %>%  group_by(id) %>% mutate(id_rank = order(median_household_income,decreasing = TRUE) , lag_rank = lag(id_rank,n=1L,order_by = id,default = 1)) %>% ungroup()



bnbstates<- c("MA","RI","CT","NY","NJ","DC","OH","NC","TN","IL","MN","CO","WA","OR","NV","CA","TX","LA","FL","HI")