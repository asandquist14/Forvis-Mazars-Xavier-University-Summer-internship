#* Title: Data Retrieval
#* Retrieve our data from data sources
#* Version: 0
#*

#* [PACKAGES]
library(tidyverse) # For all things tidy


#* [GLOBAL VARIABLES]
SELECTED_YEAR = c(2023,2024) # the year(s) we will be checking [literally useless right now]
year_dataset = read_csv("data/Hospital_Service_Area_2023.csv")
data_together_final <- read_csv("data/data_together_final_1.csv")

#* [MAIN CODE]


### written by Cade/Abigail





# remove all blank entries "*"
year_dataset_2 <- year_dataset %>% 
  filter(TOTAL_DAYS_OF_CARE != "*")
# 12.7% is actual data

# change the data types
glimpse(year_dataset_2)  # shows you the datatypes

year_dataset_2 <- year_dataset_2 %>%
  mutate(MEDICARE_PROV_NUM = as.factor(MEDICARE_PROV_NUM) ) %>% # set provider number to factor
  mutate(ZIP_CD_OF_RESIDENCE = as.factor(ZIP_CD_OF_RESIDENCE) ) %>% #zip code to factor
  mutate(TOTAL_DAYS_OF_CARE = as.numeric(gsub(",", "", TOTAL_DAYS_OF_CARE))) %>% #days of care to number
  mutate(TOTAL_CHARGES = gsub(",", "", TOTAL_CHARGES)) %>% #format total charge part 1
  mutate(TOTAL_CHARGES = substr(TOTAL_CHARGES, 2, nchar(TOTAL_CHARGES))) %>% #format total charge part 2
  mutate(TOTAL_CHARGES = as.numeric(TOTAL_CHARGES)) %>% # total charges to to number
  mutate(TOTAL_CASES = as.numeric(gsub(",", "", TOTAL_CASES))) #total cases to number
  
         
# finally, write it :D
write.csv(year_dataset_2, "data/Hospital_Service_Area_2023_Cleaned.csv", row.names = FALSE)

# make another dataset, reformatted WITH stars
year_dataset_3 <- year_dataset %>%
  mutate(MEDICARE_PROV_NUM = as.factor(MEDICARE_PROV_NUM) ) %>% # set provider number to factor
  mutate(ZIP_CD_OF_RESIDENCE = as.factor(ZIP_CD_OF_RESIDENCE) ) #zip code to factor
write.csv(year_dataset_3, "data/Hospital_Service_Area_2023_Cleaned_With_Stars.csv", row.names = FALSE)




#* [CREATE COMBINED DATASET]
# NOTE: MAKE SURE YOU'VE RAN "DemographicsGetter.R", THOSE FUNCTIONS ARE REQUIRED.

#' This function turns a dataset of zip codes of a ccn into a single-line dataset with all zip code data
#' @author Cade Campise
#' @param ccn The provider's CCN
#' @return single-line dataframe with lists of information foreach zip code in it's service area
make_new_row <- function(ccn){ 
  # just get the information for our ccn
  ccn_data = year_dataset_2 %>% 
    filter(MEDICARE_PROV_NUM == ccn) %>% 
    mutate(TOTAL_CASES = as.numeric(TOTAL_CASES))

  # alter the raw information to create our new service area
  total_ccn_cases = ccn_data$TOTAL_CASES

  new_row <- data.frame(PROVIDER_CCN = ccn,
                        SERVICE_ZIP_CODES = paste(unlist(ccn_data$ZIP_CD_OF_RESIDENCE), collapse = ", "),
                        SERVICE_ZIP_CHARGES = paste(unlist(ccn_data$TOTAL_CHARGES), collapse = ", "),
                        SERVICE_ZIP_CASES = paste(unlist(ccn_data$TOTAL_CASES), collapse = ", "),
                        
                        TOTAL_CASES = sum(ccn_data$TOTAL_CASES)
                        )
  return(new_row)
}

# begin our dataset
combined_data_a <- data.frame(matrix(ncol = 5, nrow = 0))


#provide column names
colnames(combined_data_a) <- c('PROVIDER_CCN',
                  'SERVICE_ZIP_CODES',
                  'SERVICE_ZIP_CHARGES',
                  'SERVICE_ZIP_CASES',
                  'TOTAL_CASES')

# set datatypes
combined_data_a <- combined_data_a %>%
  mutate(PROVIDER_CCN = as.factor(PROVIDER_CCN) ) %>% # set provider number to factor
  mutate(SERVICE_ZIP_CODES = as.factor(SERVICE_ZIP_CODES) ) %>% #zip code to factor
  mutate(SERVICE_ZIP_CHARGES = as.factor(SERVICE_ZIP_CHARGES) ) %>% #zip code to factor
  mutate(SERVICE_ZIP_CASES = as.factor(SERVICE_ZIP_CASES) ) %>% #zip code to factor
  mutate(TOTAL_CASES = as.numeric(TOTAL_CASES)) #total cases to number


# loop through all unique CCNS, creating our  
for (i in unique(year_dataset_2$MEDICARE_PROV_NUM) ) {
  combined_data_a = rbind(combined_data_a, make_new_row(i))
}

# join our data with stephens/nicks/ethans data
combined_data_a = left_join(combined_data_a,data_together_final,by=c("PROVIDER_CCN"="CCN"))


# start gathering some data from the ACS
# ( MAKE SURE YOU'VE RAN "DemographicsGetter.R" SO YOU HAVE THESE FUNCTIONS )

DEMOGRAPHICS_MASTER_DATA = combined_data_a #first, update the master data

acs_retrieved_data = data.frame(matrix(ncol = 23, nrow = 0))

ccns = c(360003,240010)

#provide column names
colnames(acs_retrieved_data) <- c(
                  'ESTIMATED_POPULATION',
                  'MEDIAN_AGE',
                  'POVERTY_POPULATION',
                  "POVERTY_RATE",
                  "INCOME_PER_CAPITA",
                  "MEDIAN_HOUSEHOLD_INCOME",
                  
                  "POP_MALE_17BELOW",
                  "POP_MALE_18TO24",
                  "POP_MALE_25TO44",
                  "POP_MALE_45TO64",
                  "POP_MALE_65TO84",
                  "POP_MALE_85UP",
                  
                  "POP_FEMALE_17BELOW",
                  "POP_FEMALE_18TO24",
                  "POP_FEMALE_25TO44",
                  "POP_FEMALE_45TO64",
                  "POP_FEMALE_65TO84",
                  "POP_FEMALE_85UP"

                  
                  )

# loop through the combined data and add those ccn's data into a secondary dataset
for (i in unique(combined_data_a$PROVIDER_CCN) ) {
  acs_retrieved_data = retrieve_census_data_averaged_by_ccn(i) %>% 
    mutate(PROVIDER_CCN = i) %>% 
    bind_rows(acs_retrieved_data)
}

# combine the ACS data-set and add some more data relative to other data points
combined_data_a = left_join(combined_data_a,acs_retrieved_data,by="PROVIDER_CCN") %>% 
  #Math Island :D
  mutate(TOT_DISCHARGE = DISCHRG + DISCHRG_HMO) %>% 
  mutate(TOT_COST = HOSP_CHRG_TOT * CST_CHRG) %>% 
  mutate(PERCENT_UNCOMP_COST = TOT_UNCOMP_COST / TOT_COST ) %>% 
  mutate(COST_PER = TOT_COST/TOT_DISCHARGE) %>% 
  mutate(UNCOMP_COST_PER = TOT_UNCOMP_COST/TOT_DISCHARGE) %>% 
  mutate(LINE30vsTOTCOST = COST_UNCOMP/TOT_COST) %>%
 # mutate(LINE30MINUS31DIVIDETOTCOST = (COST_UNCOMP - TOT_UNCOMP_COST)/TOT_COST) %>%
  #Relocate Island :D
  relocate(TOT_DISCHARGE, .after = HOSP_CHRG_TOT) %>%
  relocate(TOT_COST, .before = TOT_UNCOMP_COST) %>% 
  relocate(PERCENT_UNCOMP_COST, .after = TOT_COST) %>% 
  relocate(COST_PER, .after = TOT_UNCOMP_COST) %>% 
  relocate(UNCOMP_COST_PER, .after = COST_PER) %>% 
  # DESTROY CHILDRENS HOSPITALS!!!!
  filter(str_sub(PROVIDER_CCN,3,4) != "33" ) %>% 
  # state related stuff
  # true/false if the state the hospital is in expanded medicare
  # these states in the array did NOT expand medicaid when given the option, thus the ! at the beginning
  mutate(STATE_EXPANDED_MEDICAID = !(str_sub(ADDRESS,-2) %in% c("AL","FL","GA","KA","MS","SC","TN","TX","WI","WY")) ) %>% 
  mutate(MEDICAID_WORK_REQUIREMENT = (str_sub(ADDRESS,-2) %in% c("GA","AR")) ) %>% 
  mutate(STATE = (str_sub(ADDRESS,-2))) %>% 
  # stephen's stuff
  # holy shit this is scary
  mutate(POP_17BELOW_PERCENT = (POP_MALE_17BELOW + POP_FEMALE_17BELOW)/ESTIMATED_POPULATION) %>%
  mutate(POP_18_24_PERCENT = (POP_MALE_18TO24 + POP_FEMALE_18TO24)/ESTIMATED_POPULATION) %>%
  mutate(POP_25TO44_PERCENT = (POP_MALE_25TO44 + POP_FEMALE_25TO44)/ESTIMATED_POPULATION) %>%
  mutate(POP_45TO64_PERCENT = (POP_MALE_45TO64 + POP_FEMALE_45TO64)/ESTIMATED_POPULATION) %>%
  mutate(POP_65TO84_PERCENT = (POP_MALE_65TO84 + POP_FEMALE_65TO84)/ESTIMATED_POPULATION) %>%
  mutate(POP_85UP_PERCENT = (POP_MALE_85UP + POP_FEMALE_85UP)/ESTIMATED_POPULATION) %>%
  mutate(SUM_HOUSEHOLD_INCOME = HOUSEHOLD_INCOME_30BELOW + HOUSEHOLD_INCOME_30TO50 + HOUSEHOLD_INCOME_50TO100 + HOUSEHOLD_INCOME_100TO200 + HOUSEHOLD_INCOME_200UP) %>%
  mutate(HOUSEHOLD_INCOME_30BELOW_PERCENT = HOUSEHOLD_INCOME_30BELOW / SUM_HOUSEHOLD_INCOME) %>%
  mutate(HOUSEHOLD_INCOME_30TO50_PERCENT = HOUSEHOLD_INCOME_30TO50 / SUM_HOUSEHOLD_INCOME) %>%
  mutate(HOUSEHOLD_INCOME_50TO100_PERCENT = HOUSEHOLD_INCOME_50TO100 / SUM_HOUSEHOLD_INCOME) %>%
  mutate(HOUSEHOLD_INCOME_100TO200_PERCENT = HOUSEHOLD_INCOME_100TO200 / SUM_HOUSEHOLD_INCOME) %>%
  mutate(HOUSEHOLD_INCOME_200UP_PERCENT = HOUSEHOLD_INCOME_200UP / SUM_HOUSEHOLD_INCOME) %>% 
  # outlier filtering time!
  filter(LINE30vsTOTCOST <= 1 & LINE30vsTOTCOST >= 0 #keep below 0-1 inclusive
         & CST_CHRG < 5 #give some leeway for above 1 values, destroy obvious values like 3000
         & PERCENT_UNCOMP_COST < 1
         )





# do this at the end so we have a consistent variable for the final product
FINAL_combined_data = combined_data_a 

# write.csv(FINAL_combined_data, "data/COMBINED_DATA_4.csv", row.names = FALSE)




