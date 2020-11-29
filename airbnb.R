data("zip_code_db")
data("state.x77")
data("state_codes")

airbnb <- Arbnb_dt %>% group_by(host_id) %>% mutate(num_host_props = n()) %>%  ungroup() %>% group_by(neighbourhood) %>% mutate(num_props_group = n()) %>%  ungroup() %>% mutate(calc_check = ifelse(calculated_host_listings_count == num_host_props,TRUE,FALSE),last_review = lubridate::parse_date_time(last_review,orders = "dmy")
  ,percent_avail = round(availability_365 / 365,3)
  ,'months_existence(check)' = number_of_reviews * reviews_per_month
  ,min_cost_stay = price*minimum_nights )

# zip_code_db <- zip_code_db %>% filter(zipcode_type == "Standard")

zip_code_db$state <- sub('.*,\\s*', '', zip_code_db$post_office_city)

zipdata_bnb <-  subset(zip_code_db,major_city %in% airbnb$city) 

# map all coordinates with QGIS and document the states present in the dataset
bnbstates<- c("MA","RI","CT","NY","NJ","DC","OH","NC","TN","IL","MN","CO","WA","OR","NV","CA","TX","LA","FL","HI")

bnbstatejoin <- state_codes %>% filter(state_abbr %in% bnbstates)


zipdata_bnb1 <- zip_code_db %>% filter(!is.na(bounds_west)) %>% select(-common_city_list,-area_code_list) %>% left_join(state_codes[,c("state_name","state_abbr")],by = c("state" = "state_abbr")) %>% mutate(lat_centroid = (bounds_north + bounds_south)/2, long_centroid = (bounds_west + bounds_east)/2,land_water_sqmi = land_area_in_sqmi + water_area_in_sqmi)                                                         

zipdata_bnb <- zipdata_bnb %>% filter(state %in% bnbstates)

abnb_nozip <- airbnb %>% filter(is.na(neighbourhood))

a_zipcount <- airbnb %>% group_by(city) %>%  summarize(n=n())


############# Distance Function #########
CalcDistanceMiles <- function(Lat1, Long1 , Lat2 , Long2) {
  Lat1 <- as.numeric(format(Lat1,nsmall = 5))
  Long1 <- as.numeric(format(Long1, nsmall = 5))
  Lat2 <- as.numeric(format(Lat2,nsmall = 5))
  Long2 <- as.numeric(format(Long2,nsmall = 5))
  ### convert to radians ###

  Lat1 <- Lat1 / 57.2958
  Long1 <- Long1 / 57.2958
  Lat2 <- Lat2 / 57.2958
  Long2 <- Long2 / 57.2958
  ## Calc distance
  distance <- (sin(Lat1) * sin(Lat2)) + (cos(Lat1) * cos(Lat2) * cos(Long2 - Long1))
  ## Convert to miles
  miles <- ifelse(distance != 0, 3958.75 * atan(sqrt(1 - distance**2) / distance), NA)
  return(miles)
}
###########################
#CalcDistanceMiles(Lat1 =  40.76659, Long1 =  -73.95821,Lat2 = 40.77504, Long2 = -76.94797)


write_labelled_xlsx(zipdata_bnb,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\zipdata_bnbstates.xlsx")

# write_labelled_xlsx(abnb_nozip,file = "C:\\Users\\Dennis\\Desktop\\edx_capstone\\abnb_nozip.xlsx")

# longitude -- larger west to smaller east
# latitude -- larger north to smaller south

boundzips <- USAboundaries::us_zipcodes()

boundzips2 <- st_drop_geometry(boundzips2)
boundzips2 <- boundzips %>% unnest_wider(geometry)



arbnb_zip_join1 <- setDT(zipdata_bnb)[data.table(airbnb),on = .( bounds_west<=  longitude, bounds_east >=longitude  ,  bounds_north >=latitude, bounds_south <=latitude )]

arbnb_zip_join <- setDT(airbnb)[data.table(zipdata_bnb),on = .(longitude >= bounds_west, longitude <= bounds_east,  latitude <=bounds_north, latitude >=bounds_south ),nomatch =0]

adi_zipdata <- zipcode_ADI_index %>% filter(year == max(year) & !is.na(area_deprivation_index_percent)) %>% inner_join(zip_code_db,by = 'zipcode')  %>% mutate(FOM = year, year = year(year), lat_centroid = (bounds_north + bounds_south)/2, long_centroid = (bounds_west + bounds_east)/2)


opq_string(q)

zipdata_bnb$state <-sub('.*,\\s*', '', zipdata_bnb$post_office_city)


grp_ranked <- arbnb_zip_join %>% mutate(zipcode_fill = ifelse(is.na(neighbourhood),zipcode,neighbourhood)) %>%  group_by(id) %>% mutate(id_rank = order(median_household_income,decreasing = TRUE) , lag_rank = lag(id_rank,n=1L,order_by = id,default = 1)) %>% ungroup()


places <- fromJSON("C:\\Users\\Dennis\\Downloads\\datapackage.json")

ref_zip_bnb_st <- zipdata_bnb %>% distinct(.$post_office_city,.keep_all = TRUE) %>% left_join(bnbstatejoin, by = c("state" = "state_abbr")) %>% select(zipcode, post_office_city,major_city,state_name,state) %>% mutate(city_state = paste(major_city, state_name,sep = ", "),city_state_US = paste(major_city, state_name,"United States",sep = ", "))


q <- opq(bbox = 'Los Angeles, California, United States',timeout = 1200) %>% 
  add_osm_feature(key = 'amenity', value = 'university') %>% osmdata_sf()

qt <- q$osm_polygons %>% filter(!is.na(name))

qt2 <- qt %>% gsub(names(.),pattern = "\\.",replacement = "_") %>%  mutate(state = "Rhode Island")


osm_func <- function(ctyst){
  
  q <- opq(bbox = paste(ctyst,"United States", sep = ", "),timeout = 1200) %>% 
    add_osm_feature(key = 'amenity', value = 'university') %>% add_osm_feature(key = 'tourism', value = 'yes') %>% 
    add_osm_feature(key = 'tourism', value = 'attraction') %>% add_osm_feature(key = 'tourism', value = 'motel') %>% 
    add_osm_feature(key = 'tourism', value = 'theme_park') %>% add_osm_feature(key = 'tourism', value = 'viewpoint') %>%
    add_osm_feature(key = 'leisure', value = 'stadium') %>% add_osm_feature(key = 'public_transport', value = 'station') %>%
    add_osm_feature(key = 'natural', value = 'beach') %>% add_osm_feature(key = 'leisure', value = 'waterpark') %>%
    add_osm_feature(key = 'leisure', value = 'beach_resort') %>% add_osm_feature(key = 'historical', value = 'monument') %>%
    add_osm_feature(key = 'historical', value = 'memorial') %>% add_osm_feature(key = 'historical', value = 'building') %>%
    osmdata::osmdata_sf() %>% .$osm_polygons %>% sf::st_drop_geometry()  %>% setNames(gsub(names(.),pattern = "\\.",replacement = "_"))# %>% mutate(state = ctyst, zipcode = addr_postcode)

 
}

wtf <- ref_zip_bnb_st[1:2,]

qt2 <-  sapply(wtf$city_state,osm_func)

qt3 <- do.call("bind_rows",qt2)

getbb("Warren, Ohio, United States")
