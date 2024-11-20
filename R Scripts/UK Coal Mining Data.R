

####################################
### Webscrape data on Coal Mines ###
####################################

library(RSQLite)
library(rvest)
library(RSelenium)
library(stringr)
library(dplyr)
library(readxl)


###################################################################
### getting list of historical coal mines using static webscraping
###################################################################




xpath <-  '//*[@id="ajax-content-wrap"]/div[2]/div/div/table/tbody'


coal_mines <- read_html("https://www.nmrs.org.uk/resources/britains-nationalised-coal-mines-from-1947/ncb-collieries-england/") %>% 
  html_element(xpath = "//*[@id=\"ajax-content-wrap\"]/div[2]/div/div/table/tbody") %>%
  html_table()


# cleaning data

names <- coal_mines[1,]
names <- as.vector(names)

coal_mines <-coal_mines[-1,]

names(coal_mines) <- names 




####################################
## saving coal mines data as R file
###################################

save(coal_mines, file = "coal_mines.RData")



################################################
#### loading coal mines data and small changes 
################################################

load("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/R datasets/coal_mines.RData")

coal_mines <- coal_mines %>% filter(`Closed(Merged)` > 1965)

coal_mines$Town <- sub("Mary Port", "Maryport", coal_mines$Town)



################################
### wards and constituencies ###
################################


## trying to get data on wards and constituencies 

numbers <- c(1:557)

# Create an empty dataframe to store the data
data_df <- data.frame()

# Loop through each number in numbers
for (number in numbers) {
  # Read the HTML content from the webpage
  html_content <- read_html("https://en.wikipedia.org/wiki/List_of_electoral_wards_in_England_by_constituency")
  
  # Extract the HTML element using XPath
  html_element <- html_content %>%
    html_element(xpath = paste0('//*[@id="mw-content-text"]/div[1]/p[', number, ']'))
  
  # Extract the text from the HTML element
  data_text <- html_text(html_element)
  
  # Create a dataframe with the extracted text
  data_row <- data.frame(data = data_text, stringsAsFactors = FALSE)
  
  # Append the dataframe to the main dataframe
  data_df <- rbind(data_df, data_row)
}


## using regular expressions to split the data into one constituency row and several ward columns


# stringr::str_extract(data_df$data[43], "(?:\\b\\w+[,\\s]*)+:")

# new_dataframe <- data.frame()
# extracted_data <- 0

 for(i in 1:nrow(data_df)) {
  extracted_data <- stringr::str_extract(data_df$data[i], "(?:\\b\\w+[,\\s]*)+:")
  new_dataframe <- rbind(new_dataframe, extracted_data)
}
  

uk_constituencies <- data.frame()

for(i in 1:nrow(data_df)) {
  
# Split the data into two columns
split_data <- str_split(data_df$data[i], ":\\s*")  # Split by colon followed by optional whitespace

# Extract text before the colon
first_column <- sapply(split_data, function(x) x[1])

# Extract words after the colon and split by commas
second_column <- sapply(split_data, function(x) unlist(str_split(x[2], ",\\s*")))

# Create a new dataframe
new_dataframe <- data.frame(
  Constituency = first_column,
  Ward = second_column,
  stringsAsFactors = FALSE
)

uk_constituencies <- rbind(uk_constituencies, new_dataframe)
}


uk_constituencies <- uk_constituencies[-1,]


uk_constituencies <- uk_constituencies %>%
  mutate(Ward = str_remove_all(Ward, "[.]"))



#################################################
### saving and loading uk constituencies data 
#################################################


save(uk_constituencies, file = "uk_constituencies.RData")
load("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/uk_constituencies.RData")

# //*[@id="mw-content-text"]/div[1]/p[1]

# //*[@id="mw-content-text"]/div[1]/p[557]


#### merging coal mines and wards data 

coal_mines$Town %in% uk_constituencies$Ward
partial_matches <- sapply(coal_mines$Town, function(town) any(str_detect(uk_constituencies$Ward, town)))
table(partial_matches)

# problem: some towns are "villages" thus too small to be in a ward

coal_mines[720,3]

length(unique(uk_constituencies$Constituency))




#######################################################
################### Election Data #####################
#######################################################

# getting election data from 1950s until 2020

years_2 <-  c("2010", "2015", "2017", "2019")

years_1 <- c("1964", "1966", "1970", "1974F", "1974O", "1979", "1983", "1987", "1992", "1997", "2001", "2005")

election_data_all <- data.frame()
remove(election_data)

for(year in years_1) {
election_data <- read_excel("Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1918-2019election_results_by_pcon.xlsx", 
                        sheet = year, range = "B4:BH634")
election_data <- election_data %>% select(id, Constituency, County, `Country/Region`, Electorate, `Conservative Votes`, `Conservative Vote share`, `Labour Votes`, `Labour Vote share`, `Liberal Votes`, `Liberal Vote share`, `Total votes`, Turnout)

election_data <- election_data %>% filter(`Country/Region`!="Scotland" &`Country/Region`!= "Wales" & `Country/Region`!="Northern Ireland")

#names(election_data) <- c("id", "Constituency", "County", "Country/Region", "Electorate", "Conservatives_Votes", "Conservatives_Voteshare", "Labour_Votes", "Labour_Voteshare", "Liberal_Votes", "Liberal_Voteshare", "Nationalist_Votes", "Nationalist_Voteshare", "Other_Votes", "Other_Voteshare", "Total_votes", "Turnout")

# election_data <- election_data %>% select_if(~ !any(is.na(.)))
election_data$year <- year

election_data_all <- rbind(election_data_all, election_data)
}

election_data_all2 <- data.frame()

for(year in years_2) {
  election_data2 <- read_excel("Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1918-2019election_results_by_pcon.xlsx", 
                              sheet = year, range = "B4:BH634")
  election_data2 <- election_data2 %>% select(id, Constituency, County, `Country/Region`, Electorate, `Conservative Votes`, `Conservative Vote share`, `Labour Votes`, `Labour Vote share`, `Liberal Votes`, `Liberal Vote share`, `UKIP Votes`, `UKIP Vote share`,`Green Votes`, `Green Vote share`, `Total votes`, Turnout)
  
  election_data2 <- election_data2 %>% filter(`Country/Region`!="Scotland" &`Country/Region`!= "Wales" & `Country/Region`!="Northern Ireland")
  
  #names(election_data) <- c("id", "Constituency", "County", "Country/Region", "Electorate", "Conservatives_Votes", "Conservatives_Voteshare", "Labour_Votes", "Labour_Voteshare", "Liberal_Votes", "Liberal_Voteshare", "Nationalist_Votes", "Nationalist_Voteshare", "Other_Votes", "Other_Voteshare", "Total_votes", "Turnout")
  
  # election_data <- election_data %>% select_if(~ !any(is.na(.)))
  election_data2$year <- year
  
  election_data_all2 <- rbind(election_data_all2, election_data2)
}



######## merging both data sets into 1 #######


election_data_all$`UKIP Votes` <- NA
election_data_all$`UKIP Vote share` <- NA
election_data_all$`Green Votes` <- NA
election_data_all$`Green Vote share` <- NA

election_data_merged <- rbind(election_data_all, election_data_all2)

election_data_merged <- election_data_merged %>%  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))



### changing constituency names to merge later with coal mine towns

election_data_merged$Constituency <- tolower(election_data_merged$Constituency)
election_data_merged$Constituency <- tools::toTitleCase(election_data_merged$Constituency)


# renaming certain constituency names that have changed

election_data_merged$Constituency[election_data_merged$Constituency == "Stoke on Trent, central"] <- "Stoke-on-Trent Central"
election_data_merged$Constituency[election_data_merged$Constituency == "Stoke on Trent, north"] <- "Stoke-on-Trent North"
election_data_merged$Constituency[election_data_merged$Constituency == "Stoke on Trent, south"] <- "Stoke-on-Trent South"


election_data_merged$Constituency[election_data_merged$Constituency == "Nottingham,central"] <- "Nottingham Central"
election_data_merged$Constituency[election_data_merged$Constituency == "Nottingham,west"] <- "Nottingham West"
election_data_merged$Constituency[election_data_merged$Constituency == "Nottingham,south"] <- "Nottingham South"
election_data_merged$Constituency[election_data_merged$Constituency == "Nottingham,north"] <- "Nottingham North"

election_data_merged$Constituency[election_data_merged$Constituency == "Blyth"] <- "Blyth Valley"


#####

# Ashfield -> Sherwood
# Newark -> Sherwood
# Carlton -> Sherwood
# Carlton -> Gedlin 
# Doncaster -> Doncaster North
# Doncaster -> Doncaster Central 
# Dearne Valley -> Wentworth
# Newton <- St Helens North
# Whitehaven -> Copeland
# # rename 
# Blyth -> Blyth Valley
# Morpeth -> Wansbeck

####
  
#################################################
### saving and loading uk election data 
#################################################


save(election_data_merged, file = "election_data_uk.RData")
load("~/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/R datasets/election_data_uk.RData")
  


########################################
####### Version 2 - not used ##########
#######################################

# Create an empty list to store datasets for each year
election_data_list <- list()

# Loop over each year
for(year in years_1) {
  # Read data from Excel file
  data_election <- read_excel("Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/1918-2019election_results_by_pcon.xlsx", 
                              sheet = year, range = "B4:X634")
  data_election <- data_election %>% filter( Country == "England")
  

  names(data_election) <- c("id", "Constituency", "County", "Country/Region", "Electorate", "Conservatives_Votes", "Conservatives_Voteshare", "Labour_Votes", "Labour_Voteshare", "Liberal_Votes", "Liberal_Voteshare", "Nationalist_Votes", "Nationalist_Voteshare", "Other_Votes", "Other_Voteshare", "Total_votes", "Turnout")
  
  
  # Assign the year to a new column
  data_election$year <- year
  
  # Store the dataset in the list
  election_data_list[[year]] <- data_election
}

# Combine all datasets into one
election_data <- do.call(rbind, election_data_list)





##########################################################
###### connecting coal mine towns with constituencies ####
##########################################################


# creating vector with unique town names 

Towns <- unique(coal_mines$Town)


# loop over each town, click on town wikipedia webpage, get data from table for column: UK Parliament
# using R selenium as a dynamic webscraper
# but not all towns were included in the wiki page OR some coal mines did not have towns but another geographic unit as location


# writing helper function for infobox data from wikipedia pages 

helper_function <- function(x, labels, data) {
  if (x %in% labels) {
    text <- data[labels==x]
  }
  return(text)
  
}


towns_url <- "https://en.wikipedia.org/wiki/List_of_towns_in_England"

table1 <- read_html("https://en.wikipedia.org/wiki/List_of_towns_in_England") %>%
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table()

table1 <- table1  %>% 
  mutate(urls = paste0('https://www.wikipedia.org', urls),
         urls = urltools::url_decode(urls))

xpath = '//*[@id="mw-content-text"]/div[1]/table[1]'

xpath2 <- '//*[@id="mw-content-text"]/div[1]/table[1]/tbody/tr[1]/td[1]'

first <-read_html("https://en.wikipedia.org/wiki/List_of_towns_in_England") %>%
  html_element(xpath = '/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[1]/tbody/tr[3]/td[1]/a') %>%
  html_attr('href')


second <-read_html("https://en.wikipedia.org/wiki/List_of_towns_in_England") %>%
  html_element(xpath = '/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[1]/tbody/tr[3]/td[1]/a') %>%
  html_attr('title')




library(rvest)

tables <- 1:24
rows <- 1:130


# Calculate the total number of rows in towns_table1
total_rows <- length(tables) * length(rows)

# Pre-allocate data frame with enough rows
towns_table1 <- data.frame(Towns = character(total_rows), Urls = character(total_rows))

# Counter to track the current row index in towns_table1
current_row <- 1

for (table in tables) {
  for (row in rows) {
    # Calculate the index in towns_table1 based on both table and row indices
    index <- (table - 1) * length(rows) + row
    
    towns_table1$Towns[index] <-  read_html("https://en.wikipedia.org/wiki/List_of_towns_in_England") %>%
      html_element(xpath = paste0('/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[',table,']/tbody/tr[', row, ']/td[1]/a')) %>%
      html_attr("title")
    
    towns_table1$Urls[index] <-  read_html("https://en.wikipedia.org/wiki/List_of_towns_in_England") %>%
      html_element(xpath = paste0('/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[',table,']/tbody/tr[', row, ']/td[1]/a')) %>%
      html_attr("href")
    
    towns_table1$Urls[index] <- paste0("https://en.wikipedia.org", towns_table1$Urls[index])
    
    # Increment the current row index
   current_row <- current_row + 1
  }
}

# Print the result

towns_table <- na.omit(towns_table1)

towns_table$Towns_new <- sapply(str_split(towns_table$Towns, pattern = ","), `[`, 1)
towns_table$Towns_new <- gsub(" ", "", towns_table$Towns_new)
towns_table$Towns_new <- tolower(towns_table$Towns_new)
towns_table$Towns_new <-tools::toTitleCase(towns_table$Towns_new)

Towns_wiki <- towns_table$Towns_new



##################### not used ##################### 

library(stringdist)

# Compute string distances between names1 and names2
distances <- stringdistmatrix(Towns, Towns_wiki, method = "lv")

# Set a threshold for similarity (e.g., allow up to 2 edit distance)
threshold <- 2
# Find matches below the threshold
matches <- which(distances <= threshold, arr.ind = TRUE)

# Print the matches
for (i in 1:nrow(matches)) {
  cat(Towns[matches[i, 1]], "matches with", Towns_wiki[matches[i, 2]], "\n")
}

common_names <- intersect(Towns, Towns_wiki)
non_common_names <- setdiff(Towns, Towns_wiki)



##################################
## dynamic webscraper function to get suitable constituency for each coal mining town
##################################


# for all non-common names, create a dynamic webscraper:

# reading in URL
url <- "https://en.wikipedia.org/wiki/Main_Page"

# setting up the port and connection to server

rD <- rsDriver(browser=c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
driver <- rD$client
driver$navigate(url)

selector_list <- c()
# search button

selector_list$search_box <- '/html/body/div[1]/header/div[2]/div/div/div/form/div/div/input'
selector_list$go_button <-'//*[@id="searchform"]/div/button'


selector_list$first_results <- '/html/body/div[1]/header/div[2]/div/div/div/form/div/div/div[1]/input'
## search for function: 

search_for <- function(term) {
  
  # Find the search field, clear it, and enter the new search term, e.g. "data science"
  search_field <- driver$findElement(using = "xpath", value = selector_list$search_box)
  search_field$clearElement()
  search_field$sendKeysToElement(list(term))
  
  # Wait for one second and then press the enter key
  Sys.sleep(1)
  # Find the "Go" button and click on it
  go_button <- driver$findElement(using = "xpath", value = selector_list$go_button)
  go_button$clickElement()

  
}


######### TEST ########
search_for("Arkwright Town")


# Create the XPath character of the current programme
current_programe_xpath <- sprintf("/html/body/div/div/div/div/div/main/div/section/div[2]/div[%g]/div/h2", programme_int)

# Find the element on the website and transform it to text directly
current_programme_text <- driver$findElement(using = "xpath",
                                             value = current_programe_xpath)$getElementText()[[1]]



infobox_labels <- driver$findElements("css", ".infobox-label")
infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))

infobox_data <- driver$findElements("css", ".infobox-data")
infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))

infobox_all <- data_frame("Lables" = infobox_label_texts, "Data" = infobox_data_texts)



######## TEST END #########



remove(infobox_all)
remove(infobox_data)
remove(infobox_labels)
remove(infobox_data_texts)
remove(infobox_label_texts)
remove(infobox)



####### START SCRAPING DATA ##########


Towns_new <- sapply(str_split(Towns, pattern = ","), `[`, 1)


helper_function <- function(x, labels, data) {
  if (x %in% labels) {
    text <- data[labels==x]
  }
  return(text)
  
}


rD <- rsDriver(browser = c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
driver <- rD$client
driver$navigate(url)

Towns_small <- Towns_new[1:50]
infobox <- data.frame("Civil_parish" = character(),
                      "UK_Parliament" = character())


for (town in Towns_small) {
  tryCatch({
    search_for(town)
    
    infobox_title <- driver$findElements("css", ".infobox-above.fn.org.nowrap")
    infobox_title_text <- unlist(sapply(infobox_title, function(label) label$getElementText()))
    
    if (length(infobox_title_text) == 0) {
      Civil_parish <- town
      UK_Parliament <- NA
    } else {
      
      infobox_labels <- driver$findElements("css", ".infobox-label")
      infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))
      
      infobox_data <- driver$findElements("css", ".infobox-data")
      infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))
      
      Civil_parish <- town
      UK_Parliament <- helper_function("UK Parliament", infobox_label_texts, infobox_data_texts) 
    }
    
    # Create a new data frame with the values
    infobox_all <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    
    # Append the new data frame to the infobox
    infobox <- bind_rows(infobox, infobox_all)
    
  }, error = function(e) {
    cat("Error occurred for search term:", town, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    # You can add additional error handling or logging here
    
    Civil_parish <- town
    UK_Parliament <- NA
    
    infobox_all <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    infobox <- bind_rows(infobox, infobox_all)
    
  })
}



Towns_small <- Towns_new[51:100]
infobox2 <- data_frame("Civil_parish" = NA, "UK_Parliament" = NA)
  
for (town in Towns_small) {
  tryCatch({
    search_for(town)
    
    infobox_title <- driver$findElements("css", ".infobox-above.fn.org.nowrap")
    infobox_title_text <- unlist(sapply(infobox_title, function(label) label$getElementText()))
    
    if (length(infobox_title_text) == 0) {
      Civil_parish <- town
      UK_Parliament <- NA
    } else {
      infobox_labels <- driver$findElements("css", ".infobox-label")
      infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))
      
      infobox_data <- driver$findElements("css", ".infobox-data")
      infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))
      
      Civil_parish <- town
      UK_Parliament <- helper_function("UK Parliament", infobox_label_texts, infobox_data_texts) 
    }
    
    # Create a new data frame with the values
    infobox_all2 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    
    # Append the new data frame to the infobox
    infobox2 <- bind_rows(infobox2, infobox_all2)
  }, error = function(e) {
    cat("Error occurred for search term:", town, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    # You can add additional error handling or logging here
    
    Civil_parish <- town
    UK_Parliament <- NA
    
    infobox_all2 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    infobox2 <- bind_rows(infobox2, infobox_all2)
    
  })
}



Towns_small <- Towns_new[101:151]
infobox3 <- data.frame("Civil_parish" = character(),
                       "UK_Parliament" = character())


for (town in Towns_small) {
  tryCatch({
    search_for(town)
    
    infobox_title <- driver$findElements("css", ".infobox-above.fn.org.nowrap")
    infobox_title_text <- unlist(sapply(infobox_title, function(label) label$getElementText()))
    
    if (length(infobox_title_text) == 0) {
      Civil_parish <- town
      UK_Parliament <- NA
    } else {
      
      infobox_labels <- driver$findElements("css", ".infobox-label")
      infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))
      
      infobox_data <- driver$findElements("css", ".infobox-data")
      infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))
      
      Civil_parish <- town
      UK_Parliament <- helper_function("UK Parliament", infobox_label_texts, infobox_data_texts) 
    }
    
    # Create a new data frame with the values
    infobox_all3 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    
    # Append the new data frame to the infobox
    infobox3 <- bind_rows(infobox3, infobox_all3)
    
  }, error = function(e) {
    cat("Error occurred for search term:", town, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    # You can add additional error handling or logging here
    
    Civil_parish <- town
    UK_Parliament <- NA
    
    infobox_all3 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    infobox3 <- bind_rows(infobox3, infobox_all3)
    
  })
}


rD <- rsDriver(browser = c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
driver <- rD$client
driver$navigate(url)


Towns_small <- Towns_new[152:201]
infobox4 <- data.frame("Civil_parish" = character(),
                       "UK_Parliament" = character())


for (town in Towns_small) {
  tryCatch({
    search_for(town)
    
    infobox_title <- driver$findElements("css", ".infobox-above.fn.org.nowrap")
    infobox_title_text <- unlist(sapply(infobox_title, function(label) label$getElementText()))
    
    if (length(infobox_title_text) == 0) {
      Civil_parish <- town
      UK_Parliament <- NA
    } else {
      
      infobox_labels <- driver$findElements("css", ".infobox-label")
      infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))
      
      infobox_data <- driver$findElements("css", ".infobox-data")
      infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))
      
      Civil_parish <- town
      UK_Parliament <- helper_function("UK Parliament", infobox_label_texts, infobox_data_texts) 
    }
    
    # Create a new data frame with the values
    infobox_all4 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    
    # Append the new data frame to the infobox
    infobox4 <- bind_rows(infobox4, infobox_all4)

  }, error = function(e) {
    cat("Error occurred for search term:", town, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    # You can add additional error handling or logging here
    
    Civil_parish <- town
    UK_Parliament <- NA
    
    infobox_all4 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    infobox4 <- bind_rows(infobox4, infobox_all4)
    
  })
}


rD <- rsDriver(browser = c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
driver <- rD$client
driver$navigate(url)


Towns_small <- Towns_new[202:247]
infobox5 <- data.frame("Civil_parish" = character(),
                       "UK_Parliament" = character())

for (town in Towns_small) {
  tryCatch({
    search_for(town)
    
    infobox_title <- driver$findElements("css", ".infobox-above.fn.org.nowrap")
    infobox_title_text <- unlist(sapply(infobox_title, function(label) label$getElementText()))
    
    if (length(infobox_title_text) == 0) {
      Civil_parish <- town
      UK_Parliament <- NA
    } else {
      
      infobox_labels <- driver$findElements("css", ".infobox-label")
      infobox_label_texts <- unlist(sapply(infobox_labels, function(label) label$getElementText()))
      
      infobox_data <- driver$findElements("css", ".infobox-data")
      infobox_data_texts <- unlist(sapply(infobox_data, function(data) data$getElementText()))
      
      Civil_parish <- town
      UK_Parliament <- helper_function("UK Parliament", infobox_label_texts, infobox_data_texts) 
    }
    
    # Create a new data frame with the values
    infobox_all5 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    
    # Append the new data frame to the infobox
    infobox5 <- bind_rows(infobox5, infobox_all5)
    
  }, error = function(e) {
    cat("Error occurred for search term:", town, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    # You can add additional error handling or logging here
    
    Civil_parish <- town
    UK_Parliament <- NA
    
    infobox_all5 <- data.frame("Civil_parish" = Civil_parish, "UK_Parliament" = UK_Parliament)
    infobox5 <- bind_rows(infobox5, infobox_all5)
    
  })
}


infobox_merged <- rbind(infobox, infobox2, infobox3, infobox4, infobox5)

towns_in_data <- infobox_merged$Civil_parish

for(town in Towns_new){
  if(!(town %in% towns_in_data))
    print(town)
}


new_towns <- data.frame("Civil_parish" = c("Shotton Colliery","Ebchester",
                                          "Betteshanger", "Warrington", "Emley", "Grange Moor", "Altofts"),
                        "UK_Parliament" = c("Easington", "North West Durham", "Dover", 
                                            "Leigh", "Dewsbury", "Dewsbury", "Normanton, Pontefract and Castleford"))

infobox_merged <- rbind(infobox_merged, new_towns)


# getting constituencies manually from wikipedia 
infobox_merged$UK_Parliament[6] <- "Bolsover"
infobox_merged$UK_Parliament[9] <- "Amber Valley"
infobox_merged$UK_Parliament[11] <- "Bolsover"
infobox_merged$UK_Parliament[13] <- "Chesterfield"
infobox_merged$UK_Parliament[15] <- "Chesterfield"
infobox_merged$UK_Parliament[19] <- "Bolsover"
infobox_merged$UK_Parliament[22] <- "North Durham"
infobox_merged$UK_Parliament[24] <- "Gateshead"
infobox_merged$UK_Parliament[25] <- "Hartlepool"
infobox_merged$UK_Parliament[28] <- "North West Durham"
infobox_merged$UK_Parliament[29] <- "City of Durham"
infobox_merged$UK_Parliament[30] <- "Bishop Auckland"
infobox_merged$UK_Parliament[34] <- "North Durham"
infobox_merged$UK_Parliament[37] <- "Easington"
infobox_merged$UK_Parliament[38] <- "Penrith and the Border"
infobox_merged$UK_Parliament[42] <- "North West Durham"
infobox_merged$UK_Parliament[44] <- "Easington"
infobox_merged$UK_Parliament[52] <- "North West Durham"
infobox_merged$UK_Parliament[53] <- "North Durham"
infobox_merged$UK_Parliament[58] <- "Bishop Auckland"
infobox_merged$UK_Parliament[60] <- "Gateshead"
infobox_merged$UK_Parliament[66] <- "Sedgefield"
infobox_merged$UK_Parliament[69] <- "	Washington and Sunderland West"
infobox_merged$UK_Parliament[70] <- "Gateshead"
infobox_merged$UK_Parliament[71] <- "Staffordshire Moorlands"
infobox_merged$UK_Parliament[72] <- "Bishop Auckland"
infobox_merged$UK_Parliament[80] <- "Bolton West"
infobox_merged$UK_Parliament[81] <- "Leigh"
infobox_merged$UK_Parliament[82] <- "St Helens North St Helens South and Whiston"
infobox_merged$UK_Parliament[95] <- "North West Leicestershire"
infobox_merged$UK_Parliament[98] <- "North West Leicestershire"
infobox_merged$UK_Parliament[99] <- "Lagan Valley"
infobox_merged$UK_Parliament[105] <- "Blyth Valley"
infobox_merged$UK_Parliament[110] <- "Rother Valley"
infobox_merged$UK_Parliament[111] <- "Wansbeck"
infobox_merged$UK_Parliament[113] <- "Rother Valley"
infobox_merged$UK_Parliament[114] <- "Wansbeck"
infobox_merged$UK_Parliament[125] <- "Newark"
infobox_merged$UK_Parliament[127] <- "Sherwood"
infobox_merged$UK_Parliament[128] <- "Nottingham South"
infobox_merged$UK_Parliament[135] <- "Ashfield"
infobox_merged$UK_Parliament[137] <- "Sherwood"
infobox_merged$UK_Parliament[139] <- "Ashfield"
infobox_merged$UK_Parliament[142] <- "The Wrekin"
infobox_merged$UK_Parliament[147] <- "Newcastle-under-Lyme"
infobox_merged$UK_Parliament[148] <- "Tamworth"
infobox_merged$UK_Parliament[150] <- "Stoke-on-Trent North"
infobox_merged$UK_Parliament[151] <- "Stoke-on-Trent South"
infobox_merged$UK_Parliament[152] <- "Stoke-on-Trent South"
infobox_merged$UK_Parliament[155] <- "South Staffordshire"
infobox_merged$UK_Parliament[158] <- "Newcastle-under-Lyme"
infobox_merged$UK_Parliament[159] <- "Stoke-on-Trent South"
infobox_merged$UK_Parliament[160] <- "Stoke-on-Trent Central"
infobox_merged$UK_Parliament[162] <- "Coventry"
infobox_merged$UK_Parliament[164] <- "Coventry"
infobox_merged$UK_Parliament[174] <- "Barnsley East"
infobox_merged$UK_Parliament[175] <- "Doncaster Central"
infobox_merged$UK_Parliament[176] <- "Doncaster North"
infobox_merged$UK_Parliament[177] <- "Wakefield" 
infobox_merged$UK_Parliament[180] <- "North West Hampshire"
infobox_merged$UK_Parliament[183] <- "Barnsley East" 
infobox_merged$UK_Parliament[184] <- "Wakefield" 
infobox_merged$UK_Parliament[192] <- "Normanton, Pontefract and Castleford" 
infobox_merged$UK_Parliament[197] <- "Whitehaven" 
infobox_merged$UK_Parliament[199] <- "Doncaster North" 
infobox_merged$UK_Parliament[200] <- "Hemsworth" 
infobox_merged$UK_Parliament[204] <- "Hemsworth" 
infobox_merged$UK_Parliament[205] <- "Elmet and Rothwell" 
infobox_merged$UK_Parliament[206] <- "Morley and Outwood" 
infobox_merged$UK_Parliament[213] <- "Barnsley Central" 
infobox_merged$UK_Parliament[216] <- "Wakefield" 
infobox_merged$UK_Parliament[218] <- "Wakefield" 
infobox_merged$UK_Parliament[224] <- "Barnsley Central" 
infobox_merged$UK_Parliament[225] <- "Barnsley East" 
infobox_merged$UK_Parliament[227] <- "Elmet and Rothwell" 
infobox_merged$UK_Parliament[228] <- "Bishop Auckland" 
infobox_merged$UK_Parliament[231] <- "Wentworth" 
infobox_merged$UK_Parliament[232] <- "Leeds North East" 
infobox_merged$UK_Parliament[233] <- "Normanton, Pontefract and Castleford" 
infobox_merged$UK_Parliament[234] <- "Barnsley Central"
infobox_merged$UK_Parliament[235] <- "Barnsley Central"
infobox_merged$UK_Parliament[236] <- "Barnsley Central"
infobox_merged$UK_Parliament[237] <- "Barnsley Central"
infobox_merged$UK_Parliament[240] <- "Wakefield"



infobox_merged <- rename(infobox_merged, Town = Civil_parish)


##################################################################
### saving uk coal mining towns with corresponding constituencies  
##################################################################

save(infobox_merged, file = "coalmining_towns_constituencies.RData")

coal_mines$Town <- sapply(str_split(coal_mines$Town, pattern = ","), `[`, 1)



#############################################################################
### merging coal mines data and respective constituency data from the scraper
#############################################################################


coal_mines_constituency <- left_join(coal_mines, infobox_merged, by = "Town")

coal_mines_constituency <- unique(coal_mines_constituency)


coal_mines_constituency <- rename(coal_mines_constituency, Constituency = UK_Parliament)



## had to add rows for Barnsley and others due du to constituency changes 

subset <- coal_mines_constituency %>% filter(Constituency == "Barnsley East" | Constituency == "Barnsley Central" | Constituency ==  "Sherwood"
                                             | Constituency == "Doncaster North" | Constituency == "Doncaster Central " | Constituency == "Wentworth" | Constituency == "St Helens North"
                                             | Constituency == "Copeland" | Constituency == "Wansbeck" | Constituency == "Gedling" | Constituency == "Whitehaven")


subset$Constituency[subset$Constituency == "Barnsley East"] <- "Barnsley"
subset$Constituency[subset$Constituency == "Barnsley Central"] <- "Barnsley"
subset$Constituency[subset$Constituency == "Sherwood" & subset$Town == "Blidworth" | subset$Town == "Calverton" |
                    subset$Town == "Linby"  | subset$Town == "Newstead"] <- "Carlton"
subset$Constituency[subset$Constituency == "Gedling"] <- "Carlton"
subset$Constituency[subset$Constituency == " Doncaster North"] <- "Doncaster"
subset$Constituency[subset$Constituency == "Doncaster Central"] <- "Doncaster"
subset$Constituency[subset$Constituency == "Wentworth"] <- "Dearne Valley"
subset$Constituency[subset$Constituency == "St Helens North"] <- "Newton"
subset$Constituency[subset$Constituency == "Copeland"] <- "Whitehaven"
subset$Constituency[subset$Constituency == "Whitehaven" & subset$Town == "Haigh"] <- "Copeland"


coal_mines_constituency <- rbind(coal_mines_constituency, subset)



#### had to add rows for other constituency changes: 

subset2 <- coal_mines_constituency %>% filter(Constituency == "Normanton, Pontefract and Castleford" | Constituency == "North West Hampshire"
                                              | Constituency == "North Tyneside")

subset2$Constituency[subset2$Constituency == "North West Hampshire"] <- "Basingstoke"
subset2$Constituency[subset2$Constituency == "Normanton, Pontefract and Castleford"] <- "Pontefract"
subset2$Constituency[subset2$Constituency == "North Tyneside"] <- "Wallsend"


coal_mines_constituency <- rbind (coal_mines_constituency, subset2)

coal_mines_constituency <- coal_mines_constituency %>% select(-closing_time)


#### had to add one last change to Penistone

subset3 <- coal_mines_constituency %>% 
  filter(Constituency == "Penistone and Stocksbridge" | Constituency == "Normanton, Pontefract and Castleford")

subset3$Constituency[subset3$Constituency == "Penistone and Stocksbridge"] <- "Penistone"
subset3$Constituency[subset3$Constituency == "Normanton, Pontefract and Castleford"] <- "Pontefract & Castleford"


coal_mines_constituency <- rbind (coal_mines_constituency, subset3)

subset4 <- coal_mines_constituency %>% 
  filter(Constituency == "Penistone and Stocksbridge")
subset4$Constituency[subset4$Constituency == "Penistone and Stocksbridge"] <- "Barnsley West & Penistone"


coal_mines_constituency <- rbind (coal_mines_constituency, subset4)



######################################################
##### saving updated coalmine constituencies  ########
######################################################

save(coal_mines_constituency, file = "coal_mines_constituencies.RData")


load("/Users/charlottekuberka/Desktop/LSE Term 2/GY460- Spatial Analysis/GY460-Project/R datasets/coal_mines_constituencies.RData")



# changing three colliery closure dates that were incorrect

coal_mines_constituency <- coal_mines_constituency %>%
  mutate(`Closed(Merged)` = ifelse(Name %in% c("Brenkley", "Wolstanton"), 1986, `Closed(Merged)`))

coal_mines_constituency <- coal_mines_constituency %>%
  mutate(closed = ifelse(Name %in% c( "Brenkley", "Wolstanton"), 1986, closed))

coal_mines_constituency <- coal_mines_constituency %>%
  mutate(`Closed(Merged)` = ifelse(Name == "Pleasley", 1983, `Closed(Merged)`))

coal_mines_constituency <- coal_mines_constituency %>%
  mutate(closed = ifelse(Name == "Pleasley", 1983, closed))


coal_mines_constituency <- coal_mines_constituency %>% 
  mutate(closing_time = case_when(closed < 1984 ~ "before strike",
                                  closed > 1983 & closed < 1986 ~"during strike",
                                  closed > 1985 ~"after strike" ))


coal_mines_constituency$closed <- as.numeric(coal_mines_constituency$`Closed(Merged)`)

table(coal_mines_constituency$From)


####################################################
###### merging with election data  #################
####################################################


election_coalmines_constituencies <- left_join(election_data_merged, coal_mines_constituency, by = "Constituency")
election_coalmines_constituencies <- election_coalmines_constituencies %>% select(-County.y)


election_coalmines_constituencies <- election_coalmines_constituencies %>% 
  mutate(active_coalmines = case_when(
    !is.na(Name) & closed > year ~ 1,
    TRUE ~ 0
  ))

election_coalmines_constituencies <- election_coalmines_constituencies %>% 
  mutate(coalmine = case_when(
    !is.na(Name)  ~ 1,
    TRUE ~ 0
  ))


election_coalmines_constituencies <- election_coalmines_constituencies %>%
  group_by(Constituency, year) %>%
  mutate(number_coalmines = sum(active_coalmines == 1, na.rm = TRUE))


table(election_coalmines_constituencies$`Closed(Merged)`)

election_coalmines_constituencies %>%
  group_by(year) %>%
  summarise(count = sum(active_coalmines == 1, na.rm = TRUE))



election_coalmines_constituencies$year[election_coalmines_constituencies$year == "1974F"] <- "1974"
election_coalmines_constituencies$year[election_coalmines_constituencies$year == "1974O"] <- "1975"



#################################################################
##### FINAL DATASET: ELECTIONS, CONSTITUENCIES, TOWNS ###########
#################################################################

save(election_coalmines_constituencies, file= "election_coalmines_constituencies.RData")


