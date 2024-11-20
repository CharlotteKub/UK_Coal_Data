


############################################
############ Analysis ######################
############################################




load("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/R datasets/census_merged_final.RData")


######## descriptive spatial autocorrelation

census_merged_final_cor <- census_merged_final
census_merged_final_cor$id <- as.numeric(census_merged_final_cor$id)

census_merged_final_cor <- census_merged_final_cor %>% select(id, coalmine, year, Constituency)
census_merged_final_cor <- na.omit(census_merged_final_cor)
cor(census_merged_final_cor$id, census_merged_final_cor$coalmine)



#### event study #####

# comparing affected coal mines behavior to all other coal mines, leaving out no coalmines

event_study_data <- election_coalmines_constituencies %>% filter(treatment != "no coalmines")

# comparing affected coal mines behavior to constituencies with no coalmines

event_study_data2 <- election_coalmines_constituencies %>% filter(treatment != "coalmines")

election_coalmines_constituencies2 <- election_coalmines_constituencies %>% mutate(treatment2 = case_when(treatment == "coalmines" | treatment == "treated" ~ "treatment",
                                                                                         treatment == "no coalmines" ~"control"))

table(election_coalmines_constituencies2$treatment2)

##################
## plotting 
##################

turnout <- lm(Turnout ~ relevel(factor(year), ref = "1983")*treatment, data = election_coalmines_constituencies2) %>%
  broom::tidy(conf.int = T)


plot_turnout <- turnout %>%
  filter(stringr::str_detect(term, ":treatmenttreated")) %>%
  mutate(year = stringr::str_extract(term, "\\d{4}:"),
         year = as.integer(stringr::str_remove(year, ":"))) %>%
  ggplot(aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  ylim(-0.25, 0.25) +
  geom_vline(aes(xintercept = 1983), linetype = "dashed") + theme_bw()+
  ggtitle("Turnout")



############################
## trying different approach
#############################


# Fit regression model
model <- lm(Turnout ~ year * treatment, data = event_study_data)

# Calculate residuals (abnormal turnout)
event_study_data$abnormal_turnout <- residuals(model)

# Aggregate data by year and treatment
agg_data <- aggregate(abnormal_turnout ~ year + treatment, data = event_study_data, mean)

# Create event study plot
ggplot(agg_data, aes(x = year, y = abnormal_turnout, color = treatment, group = treatment)) +
  geom_line() +
  labs(title = "Event Study: Impact of Coal Miners' Strike on Voter Turnout",
       x = "Year",
       y = "Abnormal Turnout",
       color = "Treatment Group") +
  theme_minimal()

#############################################################

UKIP <- lm(`UKIP Vote share` ~ relevel(factor(year), ref = "1983")*treatment, data = event_study_data) %>%
  broom::tidy(conf.int = T)

plot_UKIP <- UKIP %>% 
  filter(stringr::str_detect(term, ":treatmenttreated")) %>% 
  mutate(year = stringr::str_extract(term, "\\d{4}:"),
         year = as.integer(stringr::str_remove(year, ":"))) %>% 
  ggplot2::ggplot(aes(year, estimate, ymin = conf.low, ymax = conf.high))+
  geom_pointrange()+ ylim(-.25,.25)+ theme_bw() +geom_vline(aes(xintercept = 1983), linetype = "dashed") +
  ggtitle("UKIP Voteshare")


Labour <- lm(`Labour Vote share` ~ relevel(factor(year), ref = "1983")*treatment, data = event_study_data) %>%
  broom::tidy(conf.int = T)

plot_labour <- Labour %>% 
  filter(stringr::str_detect(term, ":treatmenttreated")) %>% 
  mutate(year = stringr::str_extract(term, "\\d{4}:"),
         year = as.integer(stringr::str_remove(year, ":"))) %>% 
  ggplot2::ggplot(aes(year, estimate, ymin = conf.low, ymax = conf.high))+
  geom_pointrange()+ ylim(-.5,.5)+ theme_bw() + geom_vline(aes(xintercept = 1983), linetype = "dashed") +
  ggtitle("Labour Voteshare")



Tories <- lm(`Conservative Vote share` ~ relevel(factor(year), ref = "1983")*treatment, data = event_study_data) %>%
  broom::tidy(conf.int = T)

plot_tories <- Tories %>% 
  filter(stringr::str_detect(term, ":treatmenttreated")) %>% 
  mutate(year = stringr::str_extract(term, "\\d{4}:"),
         year = as.integer(stringr::str_remove(year, ":"))) %>% 
  ggplot2::ggplot(aes(year, estimate, ymin = conf.low, ymax = conf.high))+
  geom_pointrange()+ ylim(-.5,.5) + theme_bw()+geom_vline(aes(xintercept = 1983), linetype = "dashed") +
  ggtitle("Conservative Voteshare")



LibDem <- lm(`Liberal Vote share` ~ relevel(factor(year), ref = "1983")*treatment, data = event_study_data) %>%
  broom::tidy(conf.int = T)

plot_libdem <- LibDem %>% 
  filter(stringr::str_detect(term, ":treatmenttreated")) %>% 
  mutate(year = stringr::str_extract(term, "\\d{4}:"),
         year = as.integer(stringr::str_remove(year, ":"))) %>% 
  ggplot2::ggplot(aes(year, estimate, ymin = conf.low, ymax = conf.high))+
  geom_pointrange()+ ylim(-.5,.5) + theme_bw()+geom_vline(aes(xintercept = 1983), linetype = "dashed") +
  ggtitle("Liberal Voteshare")


library(cowplot)
library(patchwork)

wrap_plots(plot_tories, plot_labour, plot_turnout) +
  plot_annotation(
    title = "", 
    caption = "These plots show the effect of the coal miners strike 1984/85 on voting behavior of affected coalmine regions compared to non-affected 
    coalmine regions."
  )


ggsave("Event_studies.png", dpi = 95)




######################################
######## Summary statistics ##########
######################################

library(vtable)



election_merged_treatment <- census_merged_all %>% filter(treatment == "treated")
election_merged_control <- census_merged_all %>% filter(treatment == "no coalmines")
election_merged_coalmines <- census_merged_all %>% filter(treatment == "coalmines")



election_merged_treatment <- election_merged_treatment %>% distinct(Constituency, year, .keep_all = TRUE)
election_merged_control <- election_merged_control %>% distinct(Constituency,year, .keep_all = TRUE)
election_merged_coalmines <- election_merged_coalmines %>% distinct(Constituency,year, .keep_all = TRUE)



na_election_merged_treatment <- na.omit(election_merged_treatment)



###################################
###################################
###################################


census_merged_final <- rbind(election_merged_treatment, election_merged_control)
census_merged_final <- rbind(census_merged_final, election_merged_coalmines)



summnames <- c('N', 'NAs', 'Mean', 'Std.Dev', 'Min', 'Pctl.25', 'Pctl.75', 'Max')



    
sumtable(election_merged_treatment,vars = c('mean_Age_1981', 'Mean_age_2011', 'present_population_1981', 'Density_2011', 'seeking_work_1981_perc', 'unemployed_1981_perc',  'unemployed_2011', 'long_term_unemployed_2011','schools_college_1981'),
        # labels = labels,
         summ = c('notNA(x)', 'countNA(x)', 'mean(x)',   'sd(x)', 'min(x)', 'pctile(x)[25]', 'pctile(x)[75]', 'max(x)'),
         summ.names = summnames,
         title='Summary Statistics: Treatment Group',
         out = "latex")



sumtable(election_merged_control,vars = c('mean_Age_1981', 'Mean_age_2011', 'present_population_1981', 'Density_2011', 'seeking_work_1981_perc', 'unemployed_1981_perc',  'unemployed_2011', 'long_term_unemployed_2011','schools_college_1981'),
         # labels = labels,
         summ = c('notNA(x)', 'countNA(x)', 'mean(x)',   'sd(x)', 'min(x)', 'pctile(x)[25]', 'pctile(x)[75]', 'max(x)'),
         summ.names = summnames,
         title='Summary Statistics: Control Group',
         out = "latex")


sumtable(election_merged_coalmines,vars = c('mean_Age_1981', 'Mean_age_2011', 'present_population_1981', 'Density_2011', 'seeking_work_1981_perc', 'unemployed_1981_perc',  'unemployed_2011', 'long_term_unemployed_2011','schools_college_1981'),
         # labels = labels,
         summ = c('notNA(x)', 'countNA(x)', 'mean(x)',   'sd(x)', 'min(x)', 'pctile(x)[25]', 'pctile(x)[75]', 'max(x)'),
         summ.names = summnames,
         title='Summary Statistics: Coalmines Group',
         out = "latex")




#### Diff-in-Diff #####



## creating After variable 



census_merged_final <- census_merged_final %>%
  mutate(After = case_when(year < 1986 ~ 0,
                           year > 1983 ~ 1))




######### using census_merged_final for analysis

census_merged_final$treatment <- as.factor(census_merged_final$treatment)

census_merged_final <- census_merged_final %>% rename("labour_share" = "Labour Vote share")
census_coalmines_only <- census_coalmines_only%>% rename("labour_share" = "Labour Vote share")

census_merged_final <- census_merged_final %>% rename("conservative_share" = "Conservative Vote share")
census_coalmines_only <- census_coalmines_only%>% rename("conservative_share" = "Conservative Vote share")


census_merged_final$treatment <- relevel(census_merged_final$treatment, ref = "no coalmines")

census_coalmines_only <- census_merged_final %>% filter(treatment != "no coalmines")


mod1 <- lm(labour_share ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_merged_final)
stargazer::stargazer(mod1, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod2 <- lm(labour_share ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod2, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")



# Lagrange Multiplier Tests on OLS to see if a SEM or SAR model might be needed:

census_merged_final<- census_merged_final %>% select(-geometry.y)
census_merged_final<- census_merged_final %>% rename("geometry" = "geometry.x")

nb_queen_all <- poly2nb(census_merged_final, queen = FALSE) # queen= FALSE means more than one boundary point is required 


spdep::lm.LMtests(mod1,spatial_weights_1983,test= "all")

?lm.LMtests()


#################################################################################


mod3 <- lm(Turnout ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_merged_final)
stargazer::stargazer(mod3, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod4 <- lm(Turnout ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod4, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")


mod5 <- lm(conservative_share ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_merged_final)
stargazer::stargazer(mod5, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod6 <- lm(conservative_share ~ as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod6, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")




se_mod1 <- lmtest::coeftest(mod1, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod2 <- lmtest::coeftest(mod2, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod3 <- lmtest::coeftest(mod3, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod4 <- lmtest::coeftest(mod4, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod5 <- lmtest::coeftest(mod5, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod6 <- lmtest::coeftest(mod6, vcov = vcovCL, cluster = ~ Constituency)[,2]



stargazer::stargazer(mod1, mod2, mod5, mod6, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), se=list(se_mod1,se_mod2,se_mod5,se_mod6), type = "latex")

stargazer::stargazer(mod3, mod4, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), se=list(se_mod3,se_mod4), type = "latex")



library(fixest)

###################################
####### Spatial Analysis ##########
###################################


## Estimate contextual/SLX specification

## Include spatial weights 


mod1_slx <- lm(labour_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ year + Constituency, data = census_merged_final)
stargazer::stargazer(mod1_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod2_slx <- lm(labour_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011+ w_unemployed_2011+year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod2_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod3_slx <- lm(labour_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_merged_final)
stargazer::stargazer(mod3_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")


mod4_slx <- lm(labour_share~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod4_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")




mod5_slx <- lm(Turnout ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ year + Constituency, data = census_merged_final)
stargazer::stargazer(mod5_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod6_slx <- lm(Turnout ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011+ w_unemployed_2011+year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod6_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")

mod7_slx <- lm(Turnout ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_merged_final)
stargazer::stargazer(mod7_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")


mod8_slx <- lm(Turnout~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod8_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")




mod9_slx <- lm(conservative_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_merged_final)
stargazer::stargazer(mod9_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")


mod10_slx <- lm(conservative_share~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 +year + Constituency, data = census_coalmines_only)
stargazer::stargazer(mod10_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), type = "text")


summary(mod3_slx, cluster = "Constituency")

library(sandwich)

se_mod3_slx <- lmtest::coeftest(mod3_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod4_slx <- lmtest::coeftest(mod4_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod7_slx <- lmtest::coeftest(mod7_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod8_slx <- lmtest::coeftest(mod8_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod9_slx <- lmtest::coeftest(mod9_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod10_slx <- lmtest::coeftest(mod10_slx, vcov = vcovCL, cluster = ~ Constituency)[,2]



stargazer::stargazer(mod3_slx,mod4_slx,mod9_slx,mod10_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), se= list(se_mod3_slx, se_mod4_slx, se_mod7_slx, se_mod8_slx), type = "latex")
stargazer::stargazer(mod7_slx,mod8_slx, omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), se= list(se_mod7_slx, se_mod8_slx), type = "latex")

stargazer::stargazer(mod4_slx,mod10_slx,mod8_slx,  omit =c("Constituency", "year"), omit.labels = c("Constituency", "year"), se= list(se_mod4_slx, se_mod10_slx, se_mod8_slx), type = "latex")




########################################
### shorter time frame: 1964-1995 ######
########################################


census_merged_short <- census_merged_final %>% filter(year < 2001)
census_coalmines_short <- census_coalmines_only %>% filter(year < 2001)


mod1_small <- lm(labour_share ~as.factor(treatment)*as.factor(After) + year + Constituency, data = census_merged_short)
stargazer::stargazer(mod1_small, omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "text")


mod2_small <- lm(Turnout ~as.factor(treatment)*as.factor(After) + year + Constituency, data = census_merged_short)
stargazer::stargazer(mod2_small, omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "text")


mod3_small <- lm(labour_share ~as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_short)
stargazer::stargazer(mod3_small, omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "text")


mod4_small <- lm(Turnout ~as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_short)
stargazer::stargazer(mod4_small, omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "text")


mod5_small <- lm(conservative_share ~as.factor(treatment)*as.factor(After) + year + Constituency, data = census_coalmines_short)
stargazer::stargazer(mod5_small, omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "text")


se_mod3_small <- lmtest::coeftest(mod3_small, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod4_small <- lmtest::coeftest(mod4_small, vcov = vcovCL, cluster = ~ Constituency)[,2]
se_mod5_small <- lmtest::coeftest(mod5_small, vcov = vcovCL, cluster = ~ Constituency)[,2]




stargazer::stargazer(mod3_small, mod4_small, mod5_small, se = list(se_mod3_small, se_mod4_small, se_mod5_small), 
                     omit.labels = c("year", "Constituency"), omit = c("year", "Constituency"), type = "latex")




########### using feols 

library(fixest)

modA_slx <- feols(labour_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 | Constituency+ year, 
                  data = census_merged_final)

modB_slx <- feols(labour_share ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 | Constituency+ year, 
                  data = census_coalmines_only)

modC_slx <- feols(Turnout ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 | Constituency+ year, 
                  data = census_merged_final)

modD_slx <- feols(Turnout ~ as.factor(treatment)*as.factor(After) + present_population_1981 +unemployed_1981_perc +w_unemployed_1981_perc+ unemployed_2011 + w_unemployed_2011+ long_term_unemployed_2011+ w_long_term_unemployed_2011 | Constituency+ year, 
                  data = census_coalmines_only)


etable(modA_slx, modB_slx, modC_slx, modD_slx, tex = TRUE)

dict1 <- c("Coalmines", "Treated", "After", "Population_1981", "Unemployed_1981", "W_unemployed_1981", "Unemployed_2011", "W_unemployed_2011","long_unemployed_2011", "W_long_unemployed_2011", "Coalmines x After",
          "Treated x After")

etable(modA_slx,  modB_slx, modC_slx, modD_slx,style.df = style.df(depvar.title = "", fixef.title = "", 
                                fixef.suffix = " fixed effect", yesNo = "yes"))

#### Clustered Standard Errors ####

### Change the type of standard errors:

# By default, linear models assume your regression errors ($\varepsilon$) are i.i.d (independent and identically distributed). This simplifies the
# calculation but it can be a non-realistic assumption in some cases. We will now relax this assumption by assuming that

# a)  They are not longer identically distributed (i.e. they are heteroskedastic) or/and
# b)  They are not fully independent

# For further guidance look at <https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#the-vcov-argument>

