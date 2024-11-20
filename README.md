# UK_Coal_Data
This repository is used for storing data on coal mining constituencies and the closure of their coal mines. 

The read me file gives an overview of the generated datasets, R scripts and the process of data generation and processing.

"UK Coal Mining Data" is used to generate and scrape the different data sources and merges them together in the end. The final dataset is called  "election_coalmines_constituencies.RData". 

"Descriptives & Analysis" R Script is used for most of the plots and descriptive results using "election_coalmines_constituencies.RData". 

"Census Data" is used to merge census data for the constituencies in 1983 and 2011 and to create spatial weights and lags for the analysis and calculate Moran's I.

Finally "Analysis" includes the event study plot as well as all of the analyses, including the DiD models, Summary Statistics, and the Spatial Models.