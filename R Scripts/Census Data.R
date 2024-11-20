


#####################################################
############ Census and Spatial Weights ############
####################################################



####### uploading control census data #########


library(readxl)
library(dplyr)
load("~/Desktop/R files submission GY460/R datasets/election_coalmines_constituencies.RData")



############
## 1981 ####
############


geo_election_data_1983 <- election_coalmines_constituencies %>% filter(year == "1983") %>% select(c("id", "Constituency", "year"))



#### spatial weights

coordinates_1983 <- st_read("~/Desktop/Westminster_Parliamentary_Constituencies_2001_and_2005/PCON_DEC_2001_GB_BFE.shp")


### preparing data for join- changing the names to match election dataset

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Bedfordshire"] <- "Bedfordshire Mid"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Bedfordshire"] <- "Bedfordshire North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Luton"] <- "Luton North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South West Bedfordshire"] <- "Bedfordshire South West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "East Berkshire"] <- "Berkshire East"

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North East Derbyshire"] <- "Derbyshire North East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Derbyshire"] <- "Derbyshire West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Cornwall"] <- "Cornwall North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Derbyshire"] <- "Derbyshire West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South East Cornwall"] <- "Cornwall South East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Devon"] <- "Devon North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Dorset"] <- "Dorset South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Dorset"] <- "Dorset West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Dorset"] <- "Dorset North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "East Hampshire"] <- "Hampshire East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North West Hampshire"] <- "Hampshire North West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South West Hertfordshire"] <- "Hertfordshire South West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Hertfordshire"] <- "Hertfordshire West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Thanet"] <- "Thanet North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Thanet"] <- "Thanet South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "East Surrey"] <- "Surrey East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North West Surrey"] <- "Surrey North West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South West Surrey"] <- "Surrey South West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North West Norfolk"] <- "Norfolk North West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South West Norfolk"] <- "Norfolk South West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Norfolk"] <- "Norfolk North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Norfolk"] <- "Norfolk South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Norfolk"] <-"Norfolk Mid"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South East Cambridgeshire"] <- "Cambridgeshire South East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North East Cambridgeshire"] <- "Cambridgeshire North East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South West Cambridgeshire"] <- "Cambridgeshire South West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Newcastle upon Tyne North"] <- "Newcastle Upon Tyne North" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Newcastle upon Tyne East"] <- "Newcastle Upon Tyne East" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Newcastle upon Tyne Central"] <- "Newcastle Upon Tyne Central" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Newport"] <- "Newport West" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "East Newport"] <- "Newport East" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "City of Chester"] <- "Chester City of"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Stoke on Trent Central"] <- "Stoke-on-Trent Central"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Stoke on Trent North"] <- "Stoke-on-Trent North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Stoke on Trent South"] <- "Stoke-on-Trent South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South East Staffordshire"] <- "Staffordshire South East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Staffordshire"] <- "Staffordshire South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Staffordshire"] <- "Staffordshire Mid"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North West Durham"  ] <- "Durham North West"  
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Durham" ] <- "Durham North"  
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "City of Durham" ] <- "Durham City of"  
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Kingston upon Thames"] <- "Kingston Upon Thames"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Central Suffolk"  ] <- "Suffolk Central"   
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Suffolk"  ] <- "Suffolk South"   
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Richmond"  ] <- "Richmond & Barnes"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Kent"  ] <- "Kent Mid"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Sussex"  ] <- "Sussex Mid"

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Derbyshire"] <- "Derbyshire South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Worcestershire"] <- "Worcestershire South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Mid Worcestershire"] <- "Worcestershire Mid"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Gloucestershire"] <- "Gloucestershire West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North West Leicestershire"] <- "Leicestershire North West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Hertfordshire"] <- "Hertfordshire North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "West Lancashire"] <- "Lancashire West"

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Colchester"] <- "Colchester North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "South Colchester and Maldon"] <- "Colchester South & Maldon"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Kingston upon Hull West and Hessle"] <- "Hull West"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Kingston upon Hull East"] <- "Hull East"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Kingston upon Hull North"] <- "Hull North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Ashton under Lyne"] <- "Ashton-under-Lyne"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Stratford on Avon"] <- "Stratford-on-Avon"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Weston super Mare" ] <- "Weston-Super-Mare" 

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Aldridge - Brownhills"] <- "Aldridge-Brownhills"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Berwick upon Tweed"] <- "Berwick-Upon-Tweed"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Shropshire"] <- "Shropshire North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Warwickshire"] <- "Warwickshire North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "North Wiltshire"] <- "Wiltshire North"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Bury St Edmunds"] <- "Bury St. Edmunds"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "East Lindsey"] <- "Lindsey East" 
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "The Wrekin"] <- "Wrekin the"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Torridge and West Devon"] <- "Devon West & Torridge"   
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Morley and Leeds South"] <- "Leeds South & Morley"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "The City of London and Westminster South"] <- "City of London & Westminster South"
coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Penrith and the Borders"] <- "Penrith & the Border"

coordinates_1983$PCON01NM[coordinates_1983$PCON01NM == "Barnsley East and Mexborough"] <- "Barnsley East"



coordinates_1983$PCON01NM <- gsub(" and", " &", coordinates_1983$PCON01NM)
coordinates_1983$PCON01NM <- gsub(", ", " ", coordinates_1983$PCON01NM)




######################################
########### Spatial Join #############
######################################

##### merging 1983 coordinates with constituency names for census data later

geo_coordinates_election_1983 <- coordinates_1983 %>%
  right_join(geo_election_data_1983, by = c("PCON01NM" = "Constituency"))


#################################
###### preparing Census 1981 data 
#################################

### getting mean age ##



census_uk_1981_v2 <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data /census_uk_1981_v2.xlsx", 
                                 skip = 5)



census_uk_1981_v2 <- census_uk_1981_v2 %>% pivot_longer(cols = starts_with("Aged"),
                                                        names_to = "Age Group",
                                                        values_to = "Age Population")


census_uk_1981_v2 <- census_uk_1981_v2 %>%
  mutate(`Age Group` = str_remove_all(`Age Group`, "Aged")) %>%  
  mutate(`Age Group` = str_remove_all(`Age Group`, ": Total persons"))


census_uk_1981_v2 <- census_uk_1981_v2 %>%
  mutate(Age_Group_Start = sub("-.*|\\+$", "", `Age Group`),
         Age_Group_End = ifelse(grepl("\\+", `Age Group`), Age_Group_Start, 
                                sub(".* - ", "", `Age Group`)),
         Age_Group_End = ifelse(is.na(Age_Group_End), Age_Group_Start, Age_Group_End))



census_uk_1981_v2$Age_Group_Start <- as.numeric(census_uk_1981_v2$Age_Group_Start)
census_uk_1981_v2$Age_Group_End <- as.numeric(census_uk_1981_v2$Age_Group_End)


census_uk_1981_v2$Age_Group_End[is.na(census_uk_1981_v2$Age_Group_End)] <- 85
census_uk_1981_v2$Age_Group_Start[is.na(census_uk_1981_v2$Age_Group_Start)] <- 85



census_uk_1981_v2 <- census_uk_1981_v2 %>%
mutate(Midpoint = ifelse(grepl("\\+", `Age Group`), Age_Group_Start + 5, 
                         (Age_Group_Start + Age_Group_End) / 2))



census_uk_1981_v2 <- census_uk_1981_v2 %>% group_by(`parliamentary constituency 1983 revision`) %>%
  mutate(Mean_Age_1981 = sum(Midpoint * `Age Population`) / sum(`Age Population`)) %>%
  ungroup()
  


###########################

census_1981 <- census_uk_1981_v2 %>% dplyr::select(`parliamentary constituency 1983 revision`, `All present 1981 : Total persons`,`All resident 1981 : Total persons`, `All ages : Total persons: not in employment`,`Economically active: seeking work : Total Persons`,`Economically inactive: Total : Total Persons`, `Econ. active: Total : Total persons`, `Schools and colleges : Total Persons`, Mean_Age_1981)

census_1981 <- rename(census_1981, Constituency =  `parliamentary constituency 1983 revision`)

census_1981$Constituency[census_1981$Constituency == "Mid Bedfordshire"] <- "Bedfordshire Mid"
census_1981$Constituency[census_1981$Constituency == "North Bedfordshire"] <- "Bedfordshire North"
census_1981$Constituency[census_1981$Constituency == "North Luton"] <- "Luton North"
census_1981$Constituency[census_1981$Constituency == "South West Bedfordshire"] <- "Bedfordshire South West"
census_1981$Constituency[census_1981$Constituency == "East Berkshire"] <- "Berkshire East"

census_1981$Constituency[census_1981$Constituency == "North East Derbyshire"] <- "Derbyshire North East"
census_1981$Constituency[census_1981$Constituency == "West Derbyshire"] <- "Derbyshire West"
census_1981$Constituency[census_1981$Constituency == "North Cornwall"] <- "Cornwall North"
census_1981$Constituency[census_1981$Constituency == "West Derbyshire"] <- "Derbyshire West"
census_1981$Constituency[census_1981$Constituency == "South East Cornwall"] <- "Cornwall South East"
census_1981$Constituency[census_1981$Constituency == "North Devon"] <- "Devon North"
census_1981$Constituency[census_1981$Constituency == "South Dorset"] <- "Dorset South"
census_1981$Constituency[census_1981$Constituency == "West Dorset"] <- "Dorset West"
census_1981$Constituency[census_1981$Constituency == "North Dorset"] <- "Dorset North"
census_1981$Constituency[census_1981$Constituency == "East Hampshire"] <- "Hampshire East"
census_1981$Constituency[census_1981$Constituency == "North West Hampshire"] <- "Hampshire North West"
census_1981$Constituency[census_1981$Constituency == "South West Hertfordshire"] <- "Hertfordshire South West"
census_1981$Constituency[census_1981$Constituency == "West Hertfordshire"] <- "Hertfordshire West"
census_1981$Constituency[census_1981$Constituency == "North Thanet"] <- "Thanet North"
census_1981$Constituency[census_1981$Constituency == "South Thanet"] <- "Thanet South"
census_1981$Constituency[census_1981$Constituency == "East Surrey"] <- "Surrey East"
census_1981$Constituency[census_1981$Constituency == "North West Surrey"] <- "Surrey North West"
census_1981$Constituency[census_1981$Constituency == "South West Surrey"] <- "Surrey South West"
census_1981$Constituency[census_1981$Constituency == "North West Norfolk"] <- "Norfolk North West"
census_1981$Constituency[census_1981$Constituency == "South West Norfolk"] <- "Norfolk South West"
census_1981$Constituency[census_1981$Constituency == "North Norfolk"] <- "Norfolk North"
census_1981$Constituency[census_1981$Constituency == "South Norfolk"] <- "Norfolk South"
census_1981$Constituency[census_1981$Constituency == "Mid Norfolk"] <-"Norfolk Mid"
census_1981$Constituency[census_1981$Constituency == "South East Cambridgeshire"] <- "Cambridgeshire South East"
census_1981$Constituency[census_1981$Constituency == "North East Cambridgeshire"] <- "Cambridgeshire North East"
census_1981$Constituency[census_1981$Constituency == "South West Cambridgeshire"] <- "Cambridgeshire South West"
census_1981$Constituency[census_1981$Constituency == "Newcastle upon Tyne North"] <- "Newcastle Upon Tyne North" 
census_1981$Constituency[census_1981$Constituency == "Newcastle upon Tyne East"] <- "Newcastle Upon Tyne East" 
census_1981$Constituency[census_1981$Constituency == "Newcastle upon Tyne Central"] <- "Newcastle Upon Tyne Central" 
census_1981$Constituency[census_1981$Constituency == "West Newport"] <- "Newport West" 
census_1981$Constituency[census_1981$Constituency == "East Newport"] <- "Newport East" 
census_1981$Constituency[census_1981$Constituency == "City of Chester"] <- "Chester City of"
census_1981$Constituency[census_1981$Constituency == "Stoke on Trent Central"] <- "Stoke-on-Trent Central"
census_1981$Constituency[census_1981$Constituency == "Stoke on Trent North"] <- "Stoke-on-Trent North"
census_1981$Constituency[census_1981$Constituency == "Stoke on Trent South"] <- "Stoke-on-Trent South"
census_1981$Constituency[census_1981$Constituency == "South East Staffordshire"] <- "Staffordshire South East"
census_1981$Constituency[census_1981$Constituency == "South Staffordshire"] <- "Staffordshire South"
census_1981$Constituency[census_1981$Constituency == "Mid Staffordshire"] <- "Staffordshire Mid"
census_1981$Constituency[census_1981$Constituency == "North West Durham"  ] <- "Durham North West"  
census_1981$Constituency[census_1981$Constituency == "North Durham" ] <- "Durham North"  
census_1981$Constituency[census_1981$Constituency == "City of Durham" ] <- "Durham City of"  
census_1981$Constituency[census_1981$Constituency == "Kingston upon Thames"] <- "Kingston Upon Thames"
census_1981$Constituency[census_1981$Constituency == "Central Suffolk"  ] <- "Suffolk Central"   
census_1981$Constituency[census_1981$Constituency == "South Suffolk"  ] <- "Suffolk South"   
census_1981$Constituency[census_1981$Constituency == "Richmond"  ] <- "Richmond & Barnes"
census_1981$Constituency[census_1981$Constituency == "Mid Kent"  ] <- "Kent Mid"
census_1981$Constituency[census_1981$Constituency == "Mid Sussex"  ] <- "Sussex Mid"

census_1981$Constituency[census_1981$Constituency == "South Derbyshire"] <- "Derbyshire South"
census_1981$Constituency[census_1981$Constituency == "South Worcestershire"] <- "Worcestershire South"
census_1981$Constituency[census_1981$Constituency == "Mid Worcestershire"] <- "Worcestershire Mid"
census_1981$Constituency[census_1981$Constituency == "West Gloucestershire"] <- "Gloucestershire West"
census_1981$Constituency[census_1981$Constituency == "North West Leicestershire"] <- "Leicestershire North West"
census_1981$Constituency[census_1981$Constituency == "North Hertfordshire"] <- "Hertfordshire North"
census_1981$Constituency[census_1981$Constituency == "West Lancashire"] <- "Lancashire West"

census_1981$Constituency[census_1981$Constituency == "North Colchester"] <- "Colchester North"
census_1981$Constituency[census_1981$Constituency == "South Colchester and Maldon"] <- "Colchester South & Maldon"
census_1981$Constituency[census_1981$Constituency == "Kingston upon Hull West"] <- "Hull West"
census_1981$Constituency[census_1981$Constituency == "Kingston upon Hull East"] <- "Hull East"
census_1981$Constituency[census_1981$Constituency == "Kingston upon Hull North"] <- "Hull North"
census_1981$Constituency[census_1981$Constituency == "Ashton under Lyne"] <- "Ashton-under-Lyne"
census_1981$Constituency[census_1981$Constituency == "Stratford on Avon"] <- "Stratford-on-Avon"
census_1981$Constituency[census_1981$Constituency == "Weston super Mare" ] <- "Weston-Super-Mare" 

census_1981$Constituency[census_1981$Constituency == "Aldridge - Brownhills"] <- "Aldridge-Brownhills"
census_1981$Constituency[census_1981$Constituency == "Berwick upon Tweed"] <- "Berwick-Upon-Tweed"
census_1981$Constituency[census_1981$Constituency == "North Shropshire"] <- "Shropshire North"
census_1981$Constituency[census_1981$Constituency == "North Warwickshire"] <- "Warwickshire North"
census_1981$Constituency[census_1981$Constituency == "North Wiltshire"] <- "Wiltshire North"
census_1981$Constituency[census_1981$Constituency == "Bury St Edmunds"] <- "Bury St. Edmunds"
census_1981$Constituency[census_1981$Constituency == "East Lindsey"] <- "Lindsey East" 
census_1981$Constituency[census_1981$Constituency == "The Wrekin"] <- "Wrekin the"
census_1981$Constituency[census_1981$Constituency == "Torridge and West Devon"] <- "Devon West & Torridge"   
census_1981$Constituency[census_1981$Constituency == "Morley and Leeds South"] <- "Leeds South & Morley"
census_1981$Constituency[census_1981$Constituency == "The City of London and Westminster South"] <- "City of London & Westminster South"
census_1981$Constituency[census_1981$Constituency == "Penrith and the Borders"] <- "Penrith & the Border"


census_1981$Constituency <- gsub(" and", " &", census_1981$Constituency)

census_1981 <- distinct(census_1981)



###### testing names matches #######



census_1981_constituency <- census_1981$Constituency
geo_election_data_1983_constituency <- geo_election_data_1983$Constituency
#constituency_2019 <- census_merged_final_2019$Constituency



list_towns <- c()

for(town in geo_election_data_1983_constituency){
  if(!(town %in% census_1981_constituency))
    list_towns <- append(list_towns, town)
}



### new variable names 

names(census_1981) <-  c("Constituency", "present_population_1981","resident_population_1981",  "unemployed_1981", "seeking_work_1981", "econ_inactive_1981", "econ_active_1981", "schools_college_1981", "mean_Age_1981")




#########################################
### merging coordinates with census #####
#########################################


geo_coordinates_election_1983_census <- left_join(geo_coordinates_election_1983, census_1981, by = c("PCON01NM"= "Constituency"))
                                   
               
geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>% 
  mutate(seeking_work_1981_perc = seeking_work_1981/econ_active_1981*100) %>%
  mutate(unemployed_1981_perc = unemployed_1981/econ_active_1981*100)
  



geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>% filter(!is.na(unemployed_1981_perc)) 
geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>% filter(!is.na(seeking_work_1981_perc)) 


geo_coordinates_election_1983_census$empty_geometries <- st_is_empty(geo_coordinates_election_1983_census)

geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>% filter(empty_geometries == "FALSE") 





######################
## Spatial Weights ###
######################



library(spdep)


# Construct neighbours list from polygon list
nb_queen_1983 <- poly2nb(geo_coordinates_election_1983_census, queen = FALSE) # queen= FALSE means more than one boundary point is required 

# adding spatial weights to the neighbourhood list 

# Spatial weights for neighbours lists
spatial_weights_1983 <- nb2listw(nb_queen_1983, style = "W", zero.policy = TRUE)


geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>%
  mutate(w_unemployed_1981_perc = lag.listw(spatial_weights_1983, unemployed_1981_perc, zero.policy = T))


geo_coordinates_election_1983_census <-  geo_coordinates_election_1983_census %>%
  mutate(w_seeking_work_1981_perc = lag.listw(spatial_weights_1983, seeking_work_1981_perc, zero.policy = T))




######### Moran's I ############

# moran(x, listw, n, S0) --> x = variable, listw = spatial weight list, n = number of observations, S0 = scaling factor
# [1] = first element is extracted as moran() returns 2 elements


I <- moran(geo_coordinates_election_1983_census$unemployed_1981_perc, spatial_weights_1983, length(nb_queen_1983), Szero(spatial_weights_1983))[1]

I_2 <- moran(geo_coordinates_election_1983_census$seeking_work_1981_perc, spatial_weights_1983, length(nb_queen_1983), Szero(spatial_weights_1983))[1]

I_3<- moran(geo_coordinates_election_1983_census$mean_Age_1981, spatial_weights_1983, length(nb_queen_1983), Szero(spatial_weights_1983))[1]





### getting data on coal mines ###


election_coalmines_constituencies_1983 <- election_coalmines_constituencies %>% filter(year == "1983") %>% 
  select(id, coalmine, number_coalmines, treatment)

election_coalmines_constituencies_1983_treated <- election_coalmines_constituencies_1983 %>% filter(treatment == "treated")
election_coalmines_constituencies_1983_treated <- distinct(election_coalmines_constituencies_1983_treated)


election_coalmines_constituencies_1983_coal <- election_coalmines_constituencies_1983 %>% filter(treatment == "coalmines")
election_coalmines_constituencies_1983_coal <- distinct(election_coalmines_constituencies_1983_coal)

election_coalmines_constituencies_1983_control <- election_coalmines_constituencies_1983 %>% filter(treatment == "no coalmines")
election_coalmines_constituencies_1983_control <- distinct(election_coalmines_constituencies_1983_control)

election_coalmines_constituencies_1983_final <- rbind(election_coalmines_constituencies_1983_treated,election_coalmines_constituencies_1983_coal )

election_coalmines_constituencies_1983_final <- rbind(election_coalmines_constituencies_1983_final, election_coalmines_constituencies_1983_control)

election_coalmines_constituencies_1983_final <- election_coalmines_constituencies_1983_final %>%
  distinct(Constituency, .keep_all = TRUE)


moran_geo_coordinates_election_1983_census <- left_join(geo_coordinates_election_1983_census, election_coalmines_constituencies_1983_final, by = c("id"))


# Recall that the Moran’s I value is the slope of the line that best fits the relationship between neighboring income values and each polygon’s income in the dataset.


####################################
## Calculate Moran's I for coalmines
####################################


I_coalmines <- moran(moran_geo_coordinates_election_1983_census$number_coalmines, spatial_weights_1983, length(nb_queen_1983), Szero(spatial_weights_1983))[1]
I_coalmines2 <- moran(moran_geo_coordinates_election_1983_census$coalmine, spatial_weights_1983, length(nb_queen_1983), Szero(spatial_weights_1983))[1]



# Create spatial lag of coalmines
moran_geo_coordinates_election_1983_census$lag_coalmines <- lag.listw(spatial_weights_1983, moran_geo_coordinates_election_1983_census$number_coalmines)

# Create Moran's I scatter plot
ggplot(moran_geo_coordinates_election_1983_census, aes(x = number_coalmines, y = lag_coalmines)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(x = "number_coalmines", y = "Spatial Lag of Coalmines",
       title = "Moran's I Scatter Plot") +
  theme_minimal()



################################################
####### final dataset with spatial weights 1983
################################################


# Identify rows where the merge by 'id' failed
election_data_2010 <- election_coalmines_constituencies %>% filter(year > 2005)

election_merged_all <- left_join(election_coalmines_constituencies, geo_coordinates_election_1983_census, by ="id")

election_merged_all <- distinct(election_merged_all)
election_merged_all<-election_merged_all %>% select(-year.y)
election_merged_all<-election_merged_all %>% rename("year" = "year.x")

election_merged_all <- election_merged_all %>% filter(year < 2010)


# Merge by 'Constituency' where the 'id' merge failed and produced NAs
election_merged_all2 <- left_join(election_data_2010, geo_coordinates_election_1983_census, by =c("Constituency"= "PCON01NM"))
election_merged_all2 <- distinct(election_merged_all2)


# Remove the redundant columns after merge
election_merged_all2 <- election_merged_all2 %>% select(-id.y, year.y)

election_merged_all2 <- election_merged_all2 %>% rename("year" = "year.x")


election_merged_all <- rbind(election_merged_all, election_merged_all2)


election_merged_all <- election_merged_all %>% select(-id.x, -year.x, year.y, -empty_geometries, -PCON01NM, - PCON01CD)



############
## 1991 ####
############

census_1991 <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data /1991_constituency_data.xlsx",
                          skip = 5)

census_1991 <- rename(census_1991, Constituency =  `parliamentary constituency 1983 revision`)


census_1991$Constituency[census_1991$Constituency == "Mid Bedfordshire"] <- "Bedfordshire Mid"
census_1991$Constituency[census_1991$Constituency == "North Bedfordshire"] <- "Bedfordshire North"
census_1991$Constituency[census_1991$Constituency == "North Luton"] <- "Luton North"
census_1991$Constituency[census_1991$Constituency == "South West Bedfordshire"] <- "Bedfordshire South West"
census_1991$Constituency[census_1991$Constituency == "East Berkshire"] <- "Berkshire East"


census_1991$Constituency <- gsub("and", "&", census_1991$Constituency)


testtest1 <- left_join(geo_election_data_1983, census_1991, by="Constituency")




###########################################
#### Spatial Weights and Census for 2011 ##
############################################



############
## 2011 ####
############


### 2011 data is almost full, just very few NAs

Population_density_2011 <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data / 2011_Population density.xlsx", 
                                 skip = 7)

Age_structure_2011 <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data /Age structure_2011.xlsx", 
                                   skip = 7)

Industry_2011 <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data /2011_Industry.xlsx", 
                       skip = 7)

unemployment_2011_constituency <- read_excel("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/coal mining literature/census data /unemployment_2011_constituency.xlsx", 
                                             skip = 7)

Age_structure_2011 <- rename(Age_structure_2011, Constituency = `parliamentary constituency 2010`)
Industry_2011 <- rename(Industry_2011, Constituency = `parliamentary constituency 2010`)
unemployment_2011_constituency <- rename(unemployment_2011_constituency, Constituency = `parliamentary constituency 2010`)
Population_density_2011 <- rename(Population_density_2011, Constituency = `parliamentary constituency 2010`)


census_2011 <- left_join(Population_density_2011, Age_structure_2011, by = "Constituency")
census_2011 <- left_join(census_2011, Industry_2011, by = "Constituency")
census_2011 <- left_join(census_2011, unemployment_2011_constituency, by = "Constituency")



census_2011_small <- census_2011 %>% select(c(Constituency, `Density (number of persons per hectare)`,`Mean Age`, ...5, ...7, ...13 ))


names(census_2011_small) <- c("Constituency", "Density_2011", "Mean_age_2011", "employed_2011", "unemployed_2011", "long_term_unemployed_2011")



#### 2.  Construct neighbourhood lists from polygon list using contiguity matrix

#`nb2listw()` Syntax: 
# `nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)` 
# `style` can take values “W”, “B”, “C”, “U”, “minmax” and “S”

library(spdep)

constituencies_geo <- st_read("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/UK_Constituency_2019/PCON_DEC_2019_UK_BFC.shp")


constituencies_geo_england <- constituencies_geo %>%
  dplyr::filter(str_starts(PCON19CD, "E"))


geo_census_merged_2019 <- left_join(constituencies_geo_england, census_2011_small, by = c("PCON19NM"= "Constituency"))

geo_census_merged_2019 <-  geo_census_merged_2019 %>% filter(!is.na(unemployed_2011)) 

??poly2nb()
# Construct neighbours list from polygon list
nb_queen <- poly2nb(geo_census_merged_2019, queen = FALSE) # queen= FALSE means more than one boundary point is required 

?poly2nb()
# adding spatial weights to the neighbourhood list 
?nb2listw()

# Spatial weights for neighbours lists
spatial_weights <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)


#### 3.  Create spatially lag variables


# `lag.listw()` -\> Spatial lag of a numeric vector
# `qtm()`-/> Quick thematic map plot (similar to ggplot)

geo_census_merged_2019$unemployed_2011 <- as.numeric(geo_census_merged_2019$unemployed_2011)
geo_census_merged_2019$long_term_unemployed_2011 <- as.numeric(geo_census_merged_2019$long_term_unemployed_2011)

geo_census_merged_2019 <-  geo_census_merged_2019 %>%
  #mutate(w_unemployed_1981 = lag.listw(spatial_weights, unemployed_1981_perc, zero.policy = T)) %>% # "if TRUE assign zero to the lagged value of zones without neighbours"
  mutate(w_unemployed_2011 = lag.listw(spatial_weights, unemployed_2011, zero.policy = T))


geo_census_merged_2019 <-  geo_census_merged_2019 %>%
  mutate(w_long_term_unemployed_2011 = lag.listw(spatial_weights, long_term_unemployed_2011, zero.policy = T))



#########################################################
####### final census dataset merged #####################
#########################################################

election_merged_all <- election_merged_all %>%
  mutate(treatment = case_when(closing_time == "during strike" ~ "treated",
                               is.na(closing_time) ~ "no coalmines", 
                               closing_time == "before strike" | closing_time == "after strike" ~ "coalmines"))



census_merged_all <- left_join(election_merged_all, geo_census_merged_2019, by = c("Constituency" = "PCON19NM"))


census_merged_all <- census_merged_all %>% select(-year.y)

save(census_merged_all, file = "census_merged_final.RData")

