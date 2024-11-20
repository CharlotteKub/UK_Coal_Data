


####################################
## Coal Mining Data 
###################################

library(dplyr)
library(tidyverse)

## Loading Data 

load("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/R datasets/election_coalmines_constituencies.RData")


election_coalmines_constituencies <- election_coalmines_constituencies %>% 
  group_by(Constituency, year) %>%
  mutate(number_closed_mines = sum(closing_time == "before strike"))

election_coalmines_constituencies %>% group_by(year, number_closed_mines) %>% summarize(frequency = n())


####### attention when deduplicating: keep at least one "during strike" observation for Barnsley Central 


##### creating treatment group #######

election_coalmines_constituencies <- election_coalmines_constituencies %>%
  mutate(treatment = case_when(closing_time == "during strike" ~ "treated",
                               is.na(closing_time) ~ "no coalmines", 
                               closing_time == "before strike" | closing_time == "after strike" ~ "coalmines"))


table(election_coalmines_constituencies$treatment)



###### i have to deduplicate in a way that at least one coalmine with status "during strike" remains as an observation
###### otherwise constituency would fall out of the treated category

# election_coalmines_constituencies_deduplicated <- election_coalmines_constituencies %>%
#  distinct(Constituency, .keep_all = TRUE)


election_coalmines_constituencies_deduplicated <- election_coalmines_constituencies %>%
  group_by(Constituency) %>%
  distinct(.keep_all = TRUE) %>%
  filter(!("during strike" %in% closing_time & closing_time != "during strike"))




##########################################
###### Coal Mine Decline in Numbers ######
##########################################


number_coalmines <- election_coalmines_constituencies %>%
  group_by(year) %>%
  mutate(count_coalmines = sum(active_coalmines == 1, na.rm = TRUE))


number_coalmines$year <- as.Date(paste0(number_coalmines$year, "-01-01")) 

number_coalmines %>% group_by(year, count_coalmines) %>% summarize(frequency = n())



##########################
## Descriptives & Plotting
##########################



conservative_labour_voteshare <- election_coalmines_constituencies_deduplicated %>% group_by(treatment, year) %>% 
  mutate(avg_conservatives = mean(`Conservative Vote share`)) %>%
  mutate(avg_labour = mean(`Labour Vote share`)) %>% 
  mutate(avg_turnout = mean(Turnout)) %>%
  select(avg_conservatives, avg_labour,avg_turnout, year, treatment, Constituency)


conservative_labour_ukip_voteshare <- election_coalmines_constituencies_deduplicated %>% group_by(treatment, year) %>% 
  mutate(avg_conservatives = mean(`Conservative Vote share`)) %>%
  mutate(avg_labour = mean(`Labour Vote share`)) %>% 
  mutate(avg_ukip = mean(`UKIP Vote share`)) %>% 
  select(avg_conservatives, avg_labour, avg_ukip, avg_turnout, treatment, year, Constituency)



cortonwood <- election_coalmines_constituencies_deduplicated %>% filter(Constituency == "Barnsley" | Constituency == "Barnsley Central")
ggplot() + geom_line(data = cortonwood, aes(x = factor(year), y = Turnout, group = 1))
ggplot() + geom_line(data = cortonwood, aes(x = factor(year), y = `UKIP Vote share`, group = 1))



election_coalmines_constituencies_deduplicated %>% group_by(Constituency, closing_time) %>%
  count(closing_time) %>% print(n = 1390)


##### looking at average voter turnout ###


turnout <- election_coalmines_constituencies_deduplicated %>% group_by(year, coalmine) %>%
  mutate(avg_turnout = mean(Turnout))

ggplot() +
  geom_line(data = turnout, aes(x = factor(year), y = avg_turnout, group = factor(coalmine), color = factor(coalmine))) + 
  geom_line(data = cortonwood, aes(x = factor(year), y = Turnout, group = 1))




##### ukip #####


ggplot() +
  geom_line(data = conservative_labour_voteshare, aes(x = factor(year), y = avg_ukip, group = factor(coalmine), color = factor(coalmine))) + 
  geom_line(data = cortonwood, aes(x = factor(year), y = `UKIP Vote share`, group = 1))




##################
### pivoting data
##################

voteshares <- pivot_longer(conservative_labour_ukip_voteshare, 
                           cols = c(avg_labour, avg_conservatives, avg_ukip, avg_turnout),
                           names_to = "Party",
                           values_to = "votes")


voteshares_no_ukip <-  pivot_longer(conservative_labour_voteshare, 
                                    cols = c(avg_labour, avg_conservatives, avg_turnout),
                                    names_to = "Party",
                                    values_to = "votes")
  
  





ggplot(conservative_labour_ukip_voteshare) + 
  geom_line(aes(x = as.factor(year), y = avg_conservatives, group = treatment, color = as.factor(treatment), linetype= "avg_conservatives"))+
  geom_line(aes(x = as.factor(year), y = avg_labour, group = as.factor(treatment), color = as.factor(treatment), linetype = "avg_labour"))+
  geom_line(aes(x = as.factor(year), y = avg_ukip, group = as.factor(treatment), color = as.factor(treatment), linetype = "avg_ukip"))+
  geom_line(aes(x = factor(year), y = avg_turnout, group = as.factor(treatment), color = as.factor(treatment),linetype = "avg_turnout")) +
  scale_linetype_manual(values = c("avg_conservatives" = "solid", "avg_labour" = "dashed", "avg_ukip" = "dotted", "avg_turnout" ="twodash")) +
  theme_bw()


ggplot(voteshares_no_ukip) + 
  geom_line(aes(x = factor(year), y = votes, group = as.factor(treatment), color = as.factor(treatment))) +
  scale_color_manual(values = c("pink", "darkred", "salmon"))+
  #scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  ylab("Vote Share")+
  xlab("Election Year")+
  theme_bw() +
  facet_wrap(~ Party) +
  labs(color = "Treatment")

ggplot(voteshares_no_ukip) + 
  geom_line(aes(x = factor(year), y = votes, group = as.factor(Party), color = as.factor(Party))) +
  #scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
  theme_bw() +
  facet_wrap(~ treatment) +
  labs(color = "Voting Behavior")


?scale_color_manual()
ggplot(voteshares_no_ukip) + 
  geom_line(aes(x = factor(year), y = votes, group = treatment, linetype = as.factor(treatment))) +
  scale_linetype_manual(values = c("treated" = "solid", "no coalmines" = "dashed", "coalmines"="dotted")) +
  theme_bw() +
  facet_wrap(~ Party) +
  labs(color = "Treatment", linetype = "Treatment Status")




##############################
###### using this plot #######
##############################




ggplot(voteshares_no_ukip, aes(x = factor(year), y = votes,group = interaction(Party, treatment))) +
  geom_line(aes(linetype = as.factor(treatment), color = as.factor(Party)), linewidth = 0.5) +
  labs(x = "Year", y = "Mean Vote Share (in %)") +
  #ggtitle("Mean Vote Share in Coal Mine and other Regions Over Time") +
  scale_color_manual(values = c("blue3", "red3", "pink2"), labels = c("Conservatives", "Labour", "Turnout"), name = "Voting Behavior") +
  scale_linetype_manual(values = c( "dashed", "solid", "dotted"), labels = c("Coal", "No Coal", "Treated"), name = "Group") +
  geom_vline(aes(xintercept = "1983"),  color = "grey", linetype = "dashed")+
  annotate("text", x = "1987", y = 0.2, label = "1983", vjust = 0.1, color = "darkgrey") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("voteshares_descriptive.png", dpi = 95)




#################################
## Plotting decline of coalmines
###################################

ggplot(data = number_coalmines, aes(x = year, y = count_coalmines)) + geom_line() +
  theme_bw() + labs(title = "Decrease of Coalmines from 1964 to 2020") +
  ylab("Number of Coalmines") +
  xlab("Year")


ggsave("coalmine_decline.png", dpi=95)



###########################
## Mapping 
###########################

## load constituency geo data for England 

library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(tmaptools)


constituencies_geo <- st_read("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/UK_Constituency_2019/PCON_DEC_2019_UK_BFC.shp")


geojson_south <- st_read("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1974_constituencies_south.geojson")
geojson_south <- geojson_south %>% select(-fid)

geojson_north <- st_read("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1974_constituencies_north.geojson")
geojson_midlands <- st_read("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1974_constituencies_midlands.geojson")

geojson_all <- rbind(geojson_south, geojson_north,geojson_midlands)


ggplot() +
  geom_sf(data = constituencies_geo)

tm_shape(geojson_all) + tm_polygons()

#plot(geojson_all)


####### get data for 2019

data_2019 <- election_coalmines_constituencies %>% filter(year == "2019")

table(data_2019$treatment)

data_2019_deduplicated <- data_2019 %>%
  group_by(Constituency) %>%
  distinct(.keep_all = TRUE) %>%
  filter(!("during strike" %in% closing_time & closing_time != "during strike"))


data_2019_deduplicated <- data_2019_deduplicated %>% select(id, Constituency, coalmine, treatment,  number_coalmines, year, number_closed_mines)

data_2019_deduplicated <- as.data.frame(data_2019_deduplicated)

constituencies_geo <- constituencies_geo %>% rename("id" = "PCON19CD")

# Join data and transform

geo_election_merged <- constituencies_geo %>%
  sf::st_transform(27700) %>%
  right_join(data_2019_deduplicated, by = "id")



geo_election_merged$number_closed_mines[is.na(geo_election_merged$number_closed_mines)] <- -1


#######################################################################
########## plot all constituencies who had coalmines in it ###########
#######################################################################


library(RColorBrewer)
custom_colors1 <- brewer.pal(3, "Pastel1") 
custom_colors1 <- c("#FCB398","#FFF5F0","#940A12")


geo_election_merged$treatment <- as.factor(geo_election_merged$treatment)

tm_shape(geo_election_merged) +
  tm_polygons(fill = "treatment",
              palette= custom_colors1,
              title = "Former Coal Constituency",
              labels = c("Coalmines", "No Coalmines","Treated"))




tm_shape(geo_election_merged) +
  tm_polygons(fill = "number_coalmines", fill.scale = tm_scale_discrete(values = "OrRd")) +  # Plot polygons based on 'number_coalmines'
  tm_dots(size= "number_closed_mines", col= "red", alpha= 0.5)+
  labs(title = "Number of active and closed Coalmines per constituency in 1979")



ggplot() +
  geom_sf(data = geo_election_merged, aes(fill = factor(treatment))) +
  labs(fill = "Former Coalmining constituency", # title = "2019 Constituencies in England and their Coalmine Status",
       caption = "Treated constituencies are all those that have had a coalmine closure during the strike.")+
  scale_fill_manual(values = custom_colors1)+
  theme_bw()+
  theme(panel.grid = element_blank(),  
        axis.text = element_blank(),   
        axis.title = element_blank()) 

ggsave("map_coalmines.png", dpi = 200)


###############################################
###### Plotting number of closed mines ########
###############################################

custom_colors <- brewer.pal(9, name= "OrRd")

ggplot() +
  geom_sf(data = geo_election_merged, aes(fill = factor(number_closed_mines))) +
  #scale_fill_viridis_c(name = "Count of Mine Closures") +
  scale_fill_manual(values= custom_colors, labels = c("No Coalmine", "0 Closures", "1 Closure","2 Closures","3 Closures","4 Closures","5 Closures","6 Closures","7 Closures")) +
  theme_bw() +
  labs(title = "Number of Mine Closures in Each Constituency",
       fill = "Count of Mine Closures",
       caption = "Source: Your Source")+
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_blank(),   # Remove axis text
        axis.title = element_blank()) # Remove axis titles


##################################################
##### Plotting it all together ###################
##################################################


library(RColorBrewer)
library(colorspace)

custom_colors2 <- brewer.pal(9, name= "OrRd")
ggplot() + geom_sf(data = geo_election_merged, aes(fill = as.factor(number_coalmines))) +
  #geom_sf_label(aes(label = Constituency)) +
  scale_fill_manual(values = custom_colors2)+
  labs(fill = "Number of Active Coalmines in 1979", title = "Number of Active Coalmines per Constituency in England 1979")+
  theme_bw()+
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_blank(),   # Remove axis text
        axis.title = element_blank())# Remove axis titles



library(tmap)
tmap_mode("view")

#tm_shape(geo_election_merged_1979) +
#  tm_polygons("number_coalmines", breaks = c(0,1,2,4,8,10)) +  # Plot polygons based on 'number_coalmines'
#tm_symbols(size = "number_closed_mines", col="number_closed_mines", shape="number_closed_mines")


#tm_shape(geo_election_merged_1979) +
#  tm_polygons("number_coalmines", breaks = c(0,1,2,4,8,10)) +  # Plot polygons based on 'number_coalmines'
#  tm_dots("number_closed_mines", col = "number_closed_mines")



breaks <- sort(unique(geo_election_merged_1979$number_coalmines))

tm_shape(geo_election_merged) +
  tm_polygons(fill = "number_coalmines", fill.scale = tm_scale_discrete(values = "OrRd")) +  # Plot polygons based on 'number_coalmines'
  tm_dots(size= "number_closed_mines", col= "red", alpha= 0.5)+
  labs(title = "Number of active and closed Coalmines per constituency in 1979")



#### final plot ####


library(tmap)
library(RColorBrewer)


geo_election_merged$number_closed_mines <- as.factor(geo_election_merged$number_closed_mines)


palette <- colorRampPalette(brewer.pal(9, "Reds"))(12)

custom_labels <- c("No Coalmines", "O Coalmine","1 Coalmine", "2 Coalmines","3 Coalmines","4 Coalmines","5 Coalmines",
                   "6 Coalmines", "7 Coalmines", "9 Coalmines", "11 Coalmines", "2O Coalmines")

tm_shape(geo_election_merged) +
  tm_polygons(fill= "number_closed_mines", 
              palette =  palette,
              labels = custom_labels,
              title = "No. Closed Mines",
              legend.show = TRUE) +
  #tm_borders(col = "grey4", lwd = 0, lty = "solid", alpha = NA)+
  tm_shape(geo_election_merged[geo_election_merged$treatment == "treated",]) +
  tm_dots(size = 0.3, col = "black", fill = "black") + tm_add_legend(fill = "black", title = "Treatment Status", labels = "Treated")
  #tm_title("Number of Closed Coalmines per Constituency before Coal Miners Strike in 1984")




ggsave("map_closed_mines.jpg", dpi = 300)

