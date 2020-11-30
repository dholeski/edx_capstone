library(bigrquery)
library(lubridate)
library(tidyverse)
library(data.table)
library(sf)
library(USAboundaries)

write_csv(Us_blockgroups, 'C:\\Users\\Dennis\\Desktop\\edx_capstone\\us_blockgroups.csv')

data('zip_code_db')
data('state.x77')
data('state_codes')
cens_data <- load('C:\\Users\\Dennis\\Desktop\\Covproj\\census_zip2018.rds')

codes <- state_codes %>% filter(state_abbr %in% bnbstates) %>% t()

# airbnb <- data.table(airbnb_dt) %>% group_by(host_id) %>% mutate(num_host_props = n()) %>%  ungroup() %>% group_by(neighbourhood) %>% mutate(num_props_group = n()) %>%  ungroup() %>% mutate(calc_check = ifelse(calculated_host_listings_count == num_host_props,TRUE,FALSE),last_review = lubridate::parse_date_time(last_review,orders = 'dmy'),percent_avail = round(availability_365 / 365,3),'months_existence(check)' = number_of_reviews * reviews_per_month, min_cost_stay = price*minimum_nights)

zip_code_db$state <- sub('.*,\\s*', '', zip_code_db$post_office_city)

zipdata_bnb <-  subset(zip_code_db,major_city %in% airbnb$city) 

# map all coordinates with QGIS and document the states present in the dataset
bnbstates<- c('MA','RI','CT','NY','NJ','DC','OH','NC','TN','IL','MN','CO','WA','OR','NV','CA','TX','LA','FL','HI','WI')

bnbstatejoin <- state_codes %>% filter(state_abbr %in% bnbstates)


zip_bnb <- zip_code_db %>% filter(!is.na(bounds_west)) %>% select(-common_city_list,-area_code_list) %>% right_join(bnbstatejoin[,c('state_name','state_abbr')],by = c('state' = 'state_abbr')) %>% mutate(lat_centroid = (bounds_north + bounds_south)/2, long_centroid = (bounds_west + bounds_east)/2,land_water_sqmi = land_area_in_sqmi + water_area_in_sqmi,xymin =paste(.$bounds_west,.$bounds_south,sep = ' '),xymax=paste(.$bounds_east,.$bounds_north,sep=' '))                                                         

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


write_labelled_xlsx(zipdata_bnb,file = 'C:\\Users\\Dennis\\Desktop\\edx_capstone\\zipdata_bnbstates.xlsx')

# write_labelled_xlsx(abnb_nozip,file = 'C:\\Users\\Dennis\\Desktop\\edx_capstone\\abnb_nozip.xlsx')

# longitude -- larger west to smaller east
# latitude -- larger north to smaller south

boundzips <- USAboundaries::us_zipcodes()

boundzips2 <- st_drop_geometry(boundzips2)
boundzips2 <- boundzips %>% unnest_wider(geometry)



arbnb_zip_join1 <- setDT(zipdata_bnb)[data.table(airbnb),on = .( bounds_west<=  longitude, bounds_east >=longitude  ,  bounds_north >=latitude, bounds_south <=latitude )]

arbnb_zip_join <- setDT(airbnb)[data.table(zipdata_bnb),on = .(longitude >= bounds_west, longitude <= bounds_east,  latitude <=bounds_north, latitude >=bounds_south ),nomatch =0]

adi_zipdata <- zipcode_ADI_index %>% filter(year == max(year) & !is.na(area_deprivation_index_percent)) %>% inner_join(zip_code_db,by = 'zipcode')  %>% mutate(FOM = year, year = year(year), lat_centroid = (bounds_north + bounds_south)/2, long_centroid = (bounds_west + bounds_east)/2)

abnb_grp <- airbnb %>% group_by(city) %>% summarize(n= n(),case_when(city = 'New York City'~'New York',city == 'Washington D.C.', 'District of Columbia'))


zipdata_bnb$state <-sub('.*,\\s*', '', zipdata_bnb$post_office_city)


grp_ranked <- arbnb_zip_join %>% mutate(zipcode_fill = ifelse(is.na(neighbourhood),zipcode,neighbourhood)) %>%  group_by(id) %>% mutate(id_rank = order(median_household_income,decreasing = TRUE) , lag_rank = lag(id_rank,n=1L,order_by = id,default = 1)) %>% ungroup()




ref_zip_bnb_st <- zipdata_bnb %>% distinct(.$post_office_city,.keep_all = TRUE) %>% left_join(bnbstatejoin, by = c('state' = 'state_abbr')) %>% select(zipcode, post_office_city,major_city,state_name,state) %>% mutate(city_state = paste(major_city, state_name,sep = ', '),city_state_US = ifelse(state_name == 'District of Columbia',paste(state_name, 'United States',sep = ', '), paste(major_city, state_name,'United States',sep = ', ')))


q <- opq(bbox = 'Youngstown, Ohio, United States',timeout = 1200) %>% 
  add_osm_feature(key = 'amenity', value = 'university') %>% osmdata_sf() %>% .$osm_polygons  %>% as.data.frame()

qt <- q$osm_polygons %>% filter(!is.na(name))

qt2 <- qt %>% gsub(names(.),pattern = '\\.',replacement = '_') %>%  mutate(state = 'Rhode Island')


osm_func <- function(st){
  
  q <- opq(bbox = paste(st, 'United States', sep = ', '),timeout = 1200) %>% 
    add_osm_feature(key = 'amenity', value = 'university') %>% add_osm_feature(key = 'tourism', value = 'yes') %>% 
    add_osm_feature(key = 'tourism', value = 'attraction') %>% add_osm_feature(key = 'tourism', value = 'motel') %>% 
    add_osm_feature(key = 'tourism', value = 'theme_park') %>% add_osm_feature(key = 'tourism', value = 'viewpoint') %>%
    add_osm_feature(key = 'leisure', value = 'stadium') %>% add_osm_feature(key = 'public_transport', value = 'station') %>%
    add_osm_feature(key = 'natural', value = 'beach') %>% add_osm_feature(key = 'leisure', value = 'waterpark') %>%
    add_osm_feature(key = 'leisure', value = 'beach_resort') %>% add_osm_feature(key = 'historical', value = 'monument') %>%
    add_osm_feature(key = 'historical', value = 'memorial') %>% add_osm_feature(key = 'historical', value = 'building') %>%
    osmdata::osmdata_sf() %>% .$osm_polygons %>% sf::st_drop_geometry()  %>% setNames(gsub(names(.),pattern = '\\.',replacement = '_')) %>% mutate(state = st)

 
}

wtf <- ref_zip_bnb_st[1:2,]

qt2 <-  sapply(ref_zip_bnb_st$city_state_US,q_func)

qt3 <- do.call('bind_rows',qt2)



q_func <- function(ctystus){
  
  a <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'amenity', value = 'university') %>% osmdata_sf() %>% .$osm_polygons %>% sf::st_drop_geometry() %>% mutate(type = 'university', plc = ctystus)
  b <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'yes') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'tourism_yes', plc = ctystus)
  c <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'attraction') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'attraction', plc = ctystus)
  d <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'viewpoint') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'viewpoint', plc = ctystus)
  e <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'leisure', value = 'stadium') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'stadium', plc = ctystus)
  f <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'theme_park') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'theme_park', plc = ctystus)
  g <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'motel') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'motel', plc = ctystus)
  h <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'public_transport', value = 'station') %>% osmdata_sf()%>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'station', plc = ctystus)
  i <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'leisure', value = 'waterpark') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'waterpark', plc = ctystus)
  j <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'leisure', value = 'beach_resort') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'beach_resort', plc = ctystus)
  k <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'historical', value = 'monument') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'monument', plc = ctystus)
  l <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'historical', value = 'building') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'hist_building', plc = ctystus)
  m <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'historical', value = 'memorial') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'hist_memorial', plc = ctystus)
  n <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'natural', value = 'beach') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'natural_beach', plc = ctystus)
  o <- opq(bbox = ctystus,timeout = 1200) %>%  add_osm_feature(key = 'tourism', value = 'hotel') %>% osmdata_sf() %>% .$osm_polygons%>% sf::st_drop_geometry() %>% mutate(type = 'hotel', plc = ctystus)
  dat <- bind_rows(list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) %>% as.data.frame()
  
}
bnb_mini <- sf_bnb[6000:8000,]

zp_mini<- sf_zip[6000:8000,] %>% mutate(pnt1 =c(.$bounds_west,.$bounds_south),pnt2=c(.$bounds_east,.$bounds_north))

sf_bnbzip <- st_join(bnb_mini,zp_mini,st_nn, k=2)

stnn <- st_nn(bnb_mini,zp_mini,k=1,returnDist = TRUE,sparse = FALSE)


sf_bnb <-  st_as_sf(airbnb,coords= c('longitude', 'latitude'),crs = 4326)
sf_zip <-  st_as_sf(zip_bnb,coords= c('long_centroid', 'lat_centroid'),crs = 4326)

sf_zip2 <-  sf_zip

sf_zip2$geom <-  st_sfc(sf_zip2$xymin,sf_zip2$xymax)

unclass(sf_zipbounds$zip_code_geom)

polygon <- sf_zip2 %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast('POLYGON')

sf_zip2$geom <- do.call(rbind, list(sf_zip2$bounds_west,sf_zip2$bounds_south,sf_zip2$bounds_east,sf_zip2$bounds_north))

billing <- 'dennish-project-1'
censuszip_bounds <- bq_table_download(bq_project_query(billing,"SELECT * FROM bigquery-public-data.geo_us_boundaries.zip_codes where state_code IN ('MA','RI','CT','NY','NJ','DC','OH','NC','TN','IL','MN','CO','WA','OR','NV','CA','TX','LA','FL','HI','WI') "),max_results = Inf,quiet = FALSE)

# zipq_bounds <- bq_table_download(qry_bounds,max_results = Inf,quiet = FALSE)

sf_zipbounds <- st_as_sf(zipq_bounds, coords =c('internal_point_lon', 'internal_point_lat'))

sf_zipbounds <- st_as_sf(sf_zipbounds, wkt = 'zip_code_geom')

sf_zipbounds<- sf::st_set_geometry(sf_zipbounds,value = sf_zipbounds$zip_code_geom)


Us_blockgroups <- bq_table_download(bq_project_query(billing,"SELECT *  FROM bigquery-public-data.geo_census_blockgroups.us_blockgroups_national where state_fips_code IN ('06','08','09','11','12','15','17','22','25','27','32','34','36','37','39','41','44','47','48','53','55')"),max_results = Inf,quiet = FALSE)
