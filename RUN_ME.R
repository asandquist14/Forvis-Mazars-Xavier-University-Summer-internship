#libraries
library(shiny)
library(leaflet)
library(sf)
library(tmaptools)  
library(shinycssloaders)
library(betareg)
library(tidyverse)
library(tidycensus)
library(rsq)
library(sf)
library(htmltools)
library(leaflet.providers)
library(mapview)
library(tidygeocoder)
library(randomForest)


master_df = read_csv('data/COMBINED_DATA_10.csv')


  ####Map####
DEFAULT_COLOR = "purple"
MINISCULE_COLORS = "#AD3591"

hospital_service_area <- read_csv("data/Hospital_Service_Area_2023_Cleaned.csv")
hospital_service_area_with_stars <- read_csv("data/Hospital_Service_Area_2023_Cleaned_With_Stars.csv")
hospital_super_data <- read_csv("data/COMBINED_DATA_3.csv")

MAP_TITLE_DATA <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    background: rgba(255,255,255,0.8);
    font-weight: bold;
    font-size: 12px;
    padding: 2px 5px;
    border-radius: 2px;
  }
"))

MAP_HOSPITAL_ICON_DATA = makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/f/f9/Plus_sign.svg",
  iconWidth = 10, iconHeight = 10
)


#* [FUNCTIONS]




#' This function creates a world map of the providers service area given their CCN
#' @author Cade Campise
#' @param ccn The provider's CCN
# the Malevolent Maniacal Map Maker
mx4 <- function(ccn=240010){
  # get our specified zip codes
  filtered_service_area = hospital_service_area %>% 
    filter(MEDICARE_PROV_NUM == ccn)   #filter only hospitals with our ccn
  
  filtered_service_area_with_stars = hospital_service_area_with_stars %>% 
    filter(MEDICARE_PROV_NUM == ccn)    #filter only hospitals with our ccn
  
  
  # get the total amount of people who visited
  total_case_amnt = sum(filtered_service_area$TOTAL_CASES)
  highest_case_amnt = max(filtered_service_area$TOTAL_CASES)
  
  # get our service zip codes
  filtered_zips = zcta_data %>% 
    filter(GEOID %in% filtered_service_area$ZIP_CD_OF_RESIDENCE) %>% 
    st_transform(crs=4326)
  
  filtered_zips_with_stars = zcta_data %>% 
    filter(GEOID %in% filtered_service_area_with_stars$ZIP_CD_OF_RESIDENCE) %>% 
    filter( !(GEOID %in% filtered_service_area$ZIP_CD_OF_RESIDENCE)) %>% 
    st_transform(crs=4326)
  
  # order by zip codes so each entry points to eachother correctly
  
  filtered_service_area = arrange(filtered_service_area,filtered_service_area$ZIP_CD_OF_RESIDENCE)
  filtered_zips_with_stars = arrange(filtered_zips_with_stars,filtered_zips_with_stars$ZIP_CD_OF_RESIDENCE)
  filtered_zips = arrange(filtered_zips,filtered_zips$GEOID)
  
  provider_address = hospital_super_data %>% 
    filter(PROVIDER_CCN == ccn) %>% 
    mutate(ADDRESS = gsub("_", ", ", ADDRESS)) %>% 
    mutate(HOSP_NAME = gsub("UNIVER.", "UNIVERSITY ", HOSP_NAME)) %>% 
    mutate(HOSP_NAME = gsub("MED", "MEDICAL", HOSP_NAME)) %>% 
    mutate(HOSP_NAME = gsub("LLC", "", HOSP_NAME))
  
  # default location using the weird address formatting
  provider_location = geocode(provider_address, address = ADDRESS , method = 'osm' )
  
  if (is.na(provider_location)[1,"long"] | is.na(provider_location)[1,"lat"] ) {
    # if the weird address formating doesnt work
    provider_location = geocode(provider_address, address = HOSP_NAME , method = 'osm' )
    if (is.na(provider_location)[1,"long"] | is.na(provider_location)[1,"lat"] ) {
      # if it doesn't register the name, default to ZIP code
      provider_location = geocode(provider_address, address = ZIP , method = 'osm' )
    }  
  }
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    # HELL MODE
    #addTiles(
    #urlTemplate =
    #  "https://tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png?apikey=5f177c9fb4eb4cb0b1b465a05e3d4d02"
    #) %>%     
    addPolygons(
      data = filtered_zips_with_stars,
      color = MINISCULE_COLORS,
      weight = 1,
      label = filtered_zips_with_stars$GEOID,
      popup = ~paste0(
        "<strong>", filtered_zips$GEOID, "</strong></br>",
        "Cases: <11"
      ),
      fillOpacity = .125,
      opacity = .15
    ) %>% 
    addPolygons(
      data = filtered_zips,
      color = DEFAULT_COLOR,
      weight=1,
      label = filtered_zips$GEOID,
      popup = ~paste0(
        "<strong>", filtered_zips$GEOID, "</strong></br>",
        "Cases: ", filtered_service_area$TOTAL_CASES, "</br>",
        "$", filtered_service_area$TOTAL_CHARGES 
      ),
      
      fillOpacity = ((filtered_service_area$TOTAL_CASES)/highest_case_amnt)^.4 * .7 + .15,
      opacity = ((filtered_service_area$TOTAL_CASES)/highest_case_amnt)^.4*.7+.25
      
    ) %>% 
    addControl(tags$div(MAP_TITLE_DATA, HTML(filter(hospital_super_data,PROVIDER_CCN==ccn)$HOSP_NAME)), position = "topleft", className = "map-title") %>%
    addMarkers( #
      lng = provider_location$long,
      lat = provider_location$lat,
      icon = MAP_HOSPITAL_ICON_DATA,
      #popup = paste("Address:", address)
    ) %>% 
    setView(lng = provider_location$long, lat = provider_location$lat, zoom = 7)
  
}

#* [MAIN CODE]

### written by Cade/Abigail


# load our geographal data
zcta_data <- get_acs(
  geography = "zcta",
  variables = "B01003_001",  # Total population as an example
  geometry = TRUE,
  cb=T,
  year=2020,
)
state_data <- get_acs(
  geography = "state",
  variables = "B01003_001",  # Total population as an example
  geometry = TRUE,
  cb=T,
  year=2020,
)

# DESTROY ALASKA AND HAWAII AND PUERTO RICO (remove them from the dataset)
state_data = state_data %>% 
  filter(NAME != "Alaska" & NAME != "Hawaii" & NAME != "Puerto Rico")


##Predictive modles
  #the data being used for each model and regression
  WORKING_DATA <- read_csv("data/COMBINED_DATA_10.CSV")
  
  
  
  # Set seed for Random Forest
  
  set.seed(167)
  
  # The data being used for each model and regression
  WORKING_DATA <- read_csv("data/COMBINED_DATA_10.CSV")
  
  
  #### Linear Regression Model with significant values####
  
  
  # TURN STATE EXPANDED MEDICAID INTO A USEABLE VALUE
  if (!is.factor(WORKING_DATA$STATE_EXPANDED_MEDICAID) ||
      !all(levels(WORKING_DATA$STATE_EXPANDED_MEDICAID) %in% c("NOTEXPANDED", "EXPANDED"))) {
    
    # Ensure numeric version exists or is correctly set
    WORKING_DATA$STATE_EXPANDED_MEDICAID_NUM <- ifelse(
      WORKING_DATA$STATE_EXPANDED_MEDICAID %in% c(TRUE, 1), 1,
      ifelse(WORKING_DATA$STATE_EXPANDED_MEDICAID %in% c(FALSE, 0), 0, NA)
    )
    
    # Convert to factor with labels
    WORKING_DATA$STATE_EXPANDED_MEDICAID <- factor(
      WORKING_DATA$STATE_EXPANDED_MEDICAID_NUM,
      levels = c(0, 1),
      labels = c("NOTEXPANDED", "EXPANDED")
    )
  }
  
  
  if (!is.factor(WORKING_DATA$MEDICAID_WORK_REQUIREMENT) ||
      !all(levels(WORKING_DATA$MEDICAID_WORK_REQUIREMENT) %in% c("NOT-REQUIRED", "REQUIRED"))) {
    
    WORKING_DATA$MEDICAID_WORK_REQUIREMENT <- ifelse(
      WORKING_DATA$MEDICAID_WORK_REQUIREMENT %in% c(TRUE, 1), 1,
      ifelse(WORKING_DATA$MEDICAID_WORK_REQUIREMENT %in% c(FALSE, 0), 0, NA)
    )
    
    WORKING_DATA$MEDICAID_WORK_REQUIREMENT <- factor(
      WORKING_DATA$MEDICAID_WORK_REQUIREMENT,
      levels = c(0, 1),
      labels = c("NOT-REQUIRED", "REQUIRED")
    )
  }
  
  unique(WORKING_DATA$STATE_EXPANDED_MEDICAID)
  str(WORKING_DATA$STATE_EXPANDED_MEDICAID)
  unique(WORKING_DATA$MEDICAID_WORK_REQUIREMENT)
  str(WORKING_DATA$MEDICAID_WORK_REQUIREMENT)
  
  # FORMULA
  significant_values_model_formula <- (COST_UNCOMP/TOT_COST) ~
    HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE +
    HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
    HI_COVERAGE_35TO64_1HITYPE_TRICARE +
    HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID +
    HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY +
    HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY + 
    HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE +
    HI_COVERAGE_35TO64_NOINSURANCE +
    HI_COVERAGE_19TO34_NOINSURANCE +
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
    STATE_EXPANDED_MEDICAID +  
    MEDICAID_WORK_REQUIREMENT
  
  #  + HOUSEHOLD_INCOME_50TO100_PERCENT + HOUSEHOLD_INCOME_100TO200_PERCENT +
  # HOUSEHOLD_INCOME_30BELOW_PERCENT
  
  # MODEL 
  glm_model_for_significan_values <- glm(
    formula = significant_values_model_formula,
    family = gaussian(link = "identity"),
    data = WORKING_DATA
  )
  
  # VIEW RESULTS
  summary(glm_model_for_significan_values)
  
  rsq(glm_model_for_significan_values)
  
  
  
  
  
  
  
  #### Linear Regression Prediction with significant values ####
  
  predict_uncomp_cost_with_significant_values_model <- function(ccn = "360003") 
  {
    
    
    
    target_row <- WORKING_DATA[WORKING_DATA$PROVIDER_CCN == ccn, ]
    
    # Create a prediction data frame
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- data.frame(
      HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      HI_COVERAGE_35TO64_1HITYPE_TRICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_TRICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICARE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
      MEDICAID_WORK_REQUIREMENT = target_row$MEDICAID_WORK_REQUIREMENT
    )
    
    
    # Predict proportion
    predicted_proportion <- predict(glm_model_for_significan_values, newdata = TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION)
    
    # Return estimated cost
    
    return(predicted_proportion)
  }
  
  
  
  
  
  
  
  
  
  
  # dont move throught the wrong programs you dumbass
  
  
  
  
  #### Linear Regression Prediction with significant values and BBB prediction ####
  
  predict_with_significant_values_the_bbb_change <- function(ccn = "360003",
                                                             HI_35TO64_MEDICAID = "30", 
                                                             HI_19TO34_MEDICAID = "20", 
                                                             HI_19TO34_DIRECT_PURCHASE = "30",
                                                             STATE_EXPANDED_MEDICAID_BOOL = FALSE )
  {
    HI_35TO64_MEDICAID <- (as.numeric(HI_35TO64_MEDICAID)/100)
    HI_19TO34_MEDICAID <- (as.numeric(HI_19TO34_MEDICAID)/100)
    HI_19TO34_DIRECT_PURCHASE <- (as.numeric(HI_19TO34_DIRECT_PURCHASE)/100)
    
    HI_MEDICAREANDMEDICAID <- -1
    
    target_row <- WORKING_DATA[WORKING_DATA$PROVIDER_CCN == ccn, ]
    
    target_row$STATE_EXPANDED_MEDICAID <- ifelse(
      STATE_EXPANDED_MEDICAID_BOOL,
      ifelse(target_row$STATE_EXPANDED_MEDICAID == "NOTEXPANDED", "NOTEXPANDED", "EXPANDED"),
      "NOTEXPANDED"
    )
    
    # Create a prediction data frame
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- data.frame(
      HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      HI_COVERAGE_35TO64_1HITYPE_TRICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_TRICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICARE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
      MEDICAID_WORK_REQUIREMENT = target_row$MEDICAID_WORK_REQUIREMENT
    )
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION %>%
      
      mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID + HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID) %>% 
      
      mutate(HI_COVERAGE_35TO64_NOINSURANCE =  HI_COVERAGE_35TO64_NOINSURANCE - (HI_35TO64_MEDICAID *  HI_COVERAGE_35TO64_1HITYPE_MEDICAID) 
             - (HI_MEDICAREANDMEDICAID * HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID)) %>%
      
      mutate(HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID * (1 + HI_MEDICAREANDMEDICAID)) %>%
      
      mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID * (1 + HI_35TO64_MEDICAID))%>%
      
      mutate(HI_COVERAGE_19TO34_NOINSURANCE = HI_COVERAGE_19TO34_NOINSURANCE - (HI_COVERAGE_19TO34_1HITYPE_MEDICAID * HI_19TO34_MEDICAID) 
             - (HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * HI_19TO34_DIRECT_PURCHASE)) %>%
      
      
      mutate(HI_COVERAGE_19TO34_1HITYPE_MEDICAID =  HI_COVERAGE_19TO34_1HITYPE_MEDICAID * (1 + HI_19TO34_MEDICAID)) %>%
      
      
      mutate(HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * (1 + HI_19TO34_DIRECT_PURCHASE))
    
    # Predict proportion
    predicted_proportion <- predict(glm_model_for_significan_values, newdata = TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION)
    
    # Return estimated cost
    
    return(predicted_proportion)
  }
  
  
  
  
  
  
  
  
  #different model watch out 
  
  
  
  
  
  
  # DONT GO ANY FURTHER THAN THIS 
  
  
  
  
  #### Beta Regression Model 
  #### Beta Regression Model all values ####
  
  # Step 1: Prepare the response variable (bounded between 0 and 1)
  epsilon <- 1e-5
  WORKING_DATA$y <- with(WORKING_DATA, COST_UNCOMP / TOT_COST)
  WORKING_DATA$y <- pmin(pmax(WORKING_DATA$y, epsilon), 1 - epsilon)
  
  # Step 2: Define the model formula using your predictors
  model_formula <- y ~ 
    HI_COVERAGE_65UP +
    HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_65UP_1HITYPE_MEDICARE +
    HI_COVERAGE_65UP_1HITYPE_TRICARE +
    HI_COVERAGE_65UP_1HITYPE_VACARE +
    HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE +
    HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE +
    HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY +
    HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY + 
    HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE +
    HI_COVERAGE_65UP_NOINSURANCE +
    HI_COVERAGE_35TO64 +
    HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_35TO64_1HITYPE_MEDICARE +
    HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
    HI_COVERAGE_35TO64_1HITYPE_TRICARE +
    HI_COVERAGE_35TO64_1HITYPE_VACARE +
    HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE +
    HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE +
    HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID +
    HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY +
    HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY + 
    HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE +
    HI_COVERAGE_35TO64_NOINSURANCE +
    HI_COVERAGE_19TO34 +
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_19TO34_1HITYPE_MEDICARE +
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
    HI_COVERAGE_19TO34_1HITYPE_TRICARE +
    HI_COVERAGE_19TO34_1HITYPE_VACARE +
    HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE +
    HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID +
    HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY +
    HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY + 
    HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE +
    HI_COVERAGE_19TO34_NOINSURANCE +
    STATE_EXPANDED_MEDICAID +  
    MEDICAID_WORK_REQUIREMENT
  
  # Step 3: Remove rows with any NA values used in the model
  vars_used <- all.vars(model_formula)
  working_data_clean <- WORKING_DATA[complete.cases(WORKING_DATA[, vars_used]), ]
  
  # Step 4: Fit the beta regression model
  beta_model <- betareg(model_formula, data = working_data_clean)
  
  # Step 5: View model summary
  summary(beta_model)
  #### Beta Regression Prediction ####
  
  predict_uncomp_cost_with_beta_model <- function(ccn = "360003") 
  {
    target_row <- WORKING_DATA[WORKING_DATA$PROVIDER_CCN == ccn, ]
    
    # Create a prediction data frame
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- data.frame(
      HI_COVERAGE_65UP = target_row$HI_COVERAGE_65UP,
      HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_65UP_1HITYPE_MEDICARE = target_row$HI_COVERAGE_65UP_1HITYPE_MEDICARE,
      HI_COVERAGE_65UP_1HITYPE_TRICARE = target_row$HI_COVERAGE_65UP_1HITYPE_TRICARE,
      HI_COVERAGE_65UP_1HITYPE_VACARE = target_row$HI_COVERAGE_65UP_1HITYPE_VACARE,
      HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY, 
      HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_65UP_NOINSURANCE = target_row$HI_COVERAGE_65UP_NOINSURANCE,
      HI_COVERAGE_35TO64 = target_row$HI_COVERAGE_35TO64,
      HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      HI_COVERAGE_35TO64_1HITYPE_TRICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_TRICARE,
      HI_COVERAGE_35TO64_1HITYPE_VACARE = target_row$HI_COVERAGE_35TO64_1HITYPE_VACARE,
      HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19TO34 = target_row$HI_COVERAGE_19TO34,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICARE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_TRICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_TRICARE,
      HI_COVERAGE_19TO34_1HITYPE_VACARE = target_row$HI_COVERAGE_19TO34_1HITYPE_VACARE,
      HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY, 
      HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
      MEDICAID_WORK_REQUIREMENT = target_row$MEDICAID_WORK_REQUIREMENT
    )
    
    # Predict proportion
    predicted_proportion <- predict(beta_model, newdata = TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION)
    
    # Return estimated cost
    
    return(
      predicted_proportion
    )
  }
  
  
  
  
  #### Beta Regression Prediction with BBB prediction ####
  
  predict_uncomp_cost_with_beta_model_and_BBB <- function(ccn = "360003",
                                                          HI_35TO64_MEDICAID = "0", 
                                                          HI_19TO34_MEDICAID = "0", 
                                                          HI_19TO34_DIRECT_PURCHASE = "0") 
  {
    
    
    HI_35TO64_MEDICAID <- (as.numeric(HI_35TO64_MEDICAID)/100)
    HI_19TO34_MEDICAID <- (as.numeric(HI_19TO34_MEDICAID)/100)
    HI_19TO34_DIRECT_PURCHASE <- (as.numeric(HI_19TO34_DIRECT_PURCHASE)/100)
    
    HI_MEDICAREANDMEDICAID <- -1
    
    
    target_row <- WORKING_DATA[WORKING_DATA$PROVIDER_CCN == ccn, ]
    
    # Create a prediction data frame
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- data.frame(
      HI_COVERAGE_65UP = target_row$HI_COVERAGE_65UP,
      HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_65UP_1HITYPE_MEDICARE = target_row$HI_COVERAGE_65UP_1HITYPE_MEDICARE,
      HI_COVERAGE_65UP_1HITYPE_TRICARE = target_row$HI_COVERAGE_65UP_1HITYPE_TRICARE,
      HI_COVERAGE_65UP_1HITYPE_VACARE = target_row$HI_COVERAGE_65UP_1HITYPE_VACARE,
      HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY, 
      HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_65UP_NOINSURANCE = target_row$HI_COVERAGE_65UP_NOINSURANCE,
      HI_COVERAGE_35TO64 = target_row$HI_COVERAGE_35TO64,
      HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICARE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      HI_COVERAGE_35TO64_1HITYPE_TRICARE = target_row$HI_COVERAGE_35TO64_1HITYPE_TRICARE,
      HI_COVERAGE_35TO64_1HITYPE_VACARE = target_row$HI_COVERAGE_35TO64_1HITYPE_VACARE,
      HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE,
      HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY,
      HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19TO34 = target_row$HI_COVERAGE_19TO34,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICARE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_TRICARE = target_row$HI_COVERAGE_19TO34_1HITYPE_TRICARE,
      HI_COVERAGE_19TO34_1HITYPE_VACARE = target_row$HI_COVERAGE_19TO34_1HITYPE_VACARE,
      HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE,
      HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID,
      HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY,
      HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY = target_row$HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY, 
      HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE = target_row$HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
      MEDICAID_WORK_REQUIREMENT = target_row$MEDICAID_WORK_REQUIREMENT
    )
    
    TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION <- TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION %>%
      
      mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID + HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID) %>%
      
      mutate(HI_COVERAGE_35TO64_NOINSURANCE =  HI_COVERAGE_35TO64_NOINSURANCE - (HI_35TO64_MEDICAID *  HI_COVERAGE_35TO64_1HITYPE_MEDICAID) 
             - (HI_MEDICAREANDMEDICAID * HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID)) %>%
      
      mutate(HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID * (1 + HI_MEDICAREANDMEDICAID)) %>%
      
      
      mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID * (1 + HI_35TO64_MEDICAID))%>%
      
      mutate(HI_COVERAGE_19TO34_NOINSURANCE = HI_COVERAGE_19TO34_NOINSURANCE - (HI_COVERAGE_19TO34_1HITYPE_MEDICAID * HI_19TO34_MEDICAID) 
             - (HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * HI_19TO34_DIRECT_PURCHASE)) %>%
      
      
      mutate(HI_COVERAGE_19TO34_1HITYPE_MEDICAID =  HI_COVERAGE_19TO34_1HITYPE_MEDICAID * (1 + HI_19TO34_MEDICAID)) %>%
      
      
      mutate(HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * (1 + HI_19TO34_DIRECT_PURCHASE))
    
    # Predict proportion
    predicted_proportion <- predict(beta_model, newdata = TOTAL_VARIABLES_DATA_FRAME_FOR_FUNCTION)
    
    # Return estimated cost
    
    return(
      predicted_proportion
      
    )
  }
  #### Creating Random Forest Model ####
  
  df = read_csv("data/COMBINED_DATA_10.csv")
  
  df <- df %>%
    mutate(`30/202` = COST_UNCOMP / TOT_COST) %>% 
    mutate(`31/202` = TOT_UNCOMP_COST / TOT_COST)
  
  df <- df %>%
    rename(cool_prop = `30/202`)
  
  data_rf <- df %>% 
    select(cool_prop,HI_COVERAGE_35TO64_NOINSURANCE,
           HI_COVERAGE_19BELOW_NOINSURANCE, HI_COVERAGE_19TO34_NOINSURANCE,
           HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
           HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
           HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,STATE_EXPANDED_MEDICAID,
           HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
           HI_COVERAGE_35TO64_1HITYPE_MEDICAID)
  
  data_rf <- data_rf %>%
    filter(!(HI_COVERAGE_35TO64_NOINSURANCE %in% NA))
  
  rf_model <- randomForest(
    cool_prop ~ 
      HI_COVERAGE_35TO64_NOINSURANCE +
      HI_COVERAGE_19BELOW_NOINSURANCE +
      HI_COVERAGE_19TO34_NOINSURANCE +
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
      HI_COVERAGE_19BELOW_1HITYPE_MEDICAID +
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
      STATE_EXPANDED_MEDICAID,
    data = data_rf,
    ntree = 500,
    mtry = 3,
    importance = TRUE
  )
  
 
  #### Random Forest Prediction ####
  
  predict_uncomp_cost_rf <- function(ccn)
  {
    
    target_row <- df[df$PROVIDER_CCN == ccn, ]
    
    variables_for_rf <- data.frame(
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19BELOW_NOINSURANCE = target_row$HI_COVERAGE_19BELOW_NOINSURANCE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19BELOW_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID)
    
    predicted_proportion <- predict(rf_model, newdata = variables_for_rf)
    
    return(predicted_proportion)
    
  }
  
  #### Random Forest Prediction with BBB ####
  
  predict_uncomp_cost_rf_BBB <- function(ccn,HI_35TO64_MEDICAID, 
                                         HI_19TO34_MEDICAID, 
                                         HI_19TO34_DIRECT_PURCHASE,
                                         STATE_EXPANDED_MEDICAID_BOOL)
  {
    
    HI_35TO64_MEDICAID <- (as.numeric(HI_35TO64_MEDICAID)/100)
    HI_19TO34_MEDICAID <- (as.numeric(HI_19TO34_MEDICAID)/100)
    HI_19TO34_DIRECT_PURCHASE <- (as.numeric(HI_19TO34_DIRECT_PURCHASE)/100)
    
    target_row <- df[df$PROVIDER_CCN == ccn, ]
    
    target_row$STATE_EXPANDED_MEDICAID <- ifelse(
      STATE_EXPANDED_MEDICAID_BOOL,
      ifelse(target_row$STATE_EXPANDED_MEDICAID == FALSE, FALSE, TRUE),
      FALSE
    )
    
    variables_for_rf <- data.frame(
      HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
      HI_COVERAGE_19BELOW_NOINSURANCE = target_row$HI_COVERAGE_19BELOW_NOINSURANCE,
      HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
      HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
      HI_COVERAGE_19BELOW_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,
      HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
      HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
      STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID)
    
    variables_for_rf <- variables_for_rf %>%
      mutate(HI_COVERAGE_35TO64_NOINSURANCE =  HI_COVERAGE_35TO64_NOINSURANCE - (HI_35TO64_MEDICAID *  HI_COVERAGE_35TO64_1HITYPE_MEDICAID)) %>%
      mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID * (1 + HI_35TO64_MEDICAID))%>%
      mutate(HI_COVERAGE_19TO34_NOINSURANCE = HI_COVERAGE_19TO34_NOINSURANCE - (HI_COVERAGE_19TO34_1HITYPE_MEDICAID * HI_19TO34_MEDICAID) 
             - (HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * HI_19TO34_DIRECT_PURCHASE)) %>%
      mutate(HI_COVERAGE_19TO34_1HITYPE_MEDICAID =  HI_COVERAGE_19TO34_1HITYPE_MEDICAID * (1 + HI_19TO34_MEDICAID)) %>%
      mutate(HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * (1 + HI_19TO34_DIRECT_PURCHASE))
    
    predicted_proportion <- predict(rf_model, newdata = variables_for_rf)
    
    return(predicted_proportion)
    
  }

  ####GUI Stuff####


##Pull Functions
get_uncomp_cost_porportion = function(ccn) {
  uncomp = master_df %>% 
    filter(PROVIDER_CCN == ccn) %>% 
    pull(COST_UNCOMP)
  tot = master_df %>% 
    filter(PROVIDER_CCN == ccn) %>% 
    pull(TOT_COST)
  return(uncomp/tot)
}
get_tot_cost = function(ccn) {
  tot = master_df %>% 
    filter(PROVIDER_CCN == ccn) %>% 
    pull(TOT_COST)
  return(tot)
}
get_uncomp_cost = function(ccn){
  tot_uncomp = master_df %>%
    filter(PROVIDER_CCN == ccn) %>% 
    pull(COST_UNCOMP)
  return(tot_uncomp)
}

##rounding functions
round_uncomp_costP = function(ccn) {
  num = (predict_uncomp_cost_with_significant_values_model(ccn)*get_tot_cost(ccn))
  round(num, digits = 2)
  return(num)
}

##display functions

LRxM = function(ccn) {
  paste(round((predict_uncomp_cost_with_significant_values_model(ccn) *100), digits = 2), "%")
}
UCPxM <- function(ccn) {
  amount <- paste0("Uncompensated Cost: <strong>$", format(get_uncomp_cost(ccn), big.mark = ",", scientific = FALSE),"</strong>")
  percent <- paste0("Percent of Uncompensated Cost: <strong>", round(get_uncomp_cost_porportion(ccn) * 100, digits = 2), "%</strong>")
  HTML(paste(amount, percent, sep = "<br><br>"))
}
LRxBBB = function(ccn, med34, med19, dp, st) {
  paste(round((predict_with_significant_values_the_bbb_change(ccn,med34,med19,dp, st) *100), digits = 2), "%")
}
LRxBBB_num = function(ccn, med34, med19, dp, st) {
  n = round((predict_with_significant_values_the_bbb_change(ccn,med34,med19,dp, st) *100), digits = 2)
  return(n)
}
BRxM = function(ccn) {
  paste(round((predict_uncomp_cost_with_beta_model(ccn) *100), digits = 2), "%")
}
BRxBBB = function(ccn, med34, med19, dp) {
  paste(round((predict_uncomp_cost_with_beta_model_and_BBB(ccn,med34,med19,dp) *100), digits = 2), "%")
}
BRxBBB_num = function(ccn, med34, med19, dp) {
  n = round((predict_uncomp_cost_with_beta_model_and_BBB(ccn,med34,med19,dp) *100), digits = 2)
  return(n)
}
RFxM = function(ccn) {
  paste(round((predict_uncomp_cost_rf(ccn) *100),digits = 2), "%")
}
RFxBBB = function(ccn, med34, med19, dp, st) {
  paste(round((predict_uncomp_cost_rf_BBB(ccn,med34,med19,dp, st) *100),digits=2), "%")
}

per_change = function(ccn, med34, med19, dp, st) {
  new_porp = ((
    predict_with_significant_values_the_bbb_change(ccn, med34, med19, dp, st)+ 
    predict_uncomp_cost_with_beta_model_and_BBB(ccn, med34, med19, dp)+ 
    predict_uncomp_cost_rf_BBB(ccn, med34, med19, dp, st)
  )/3)
  
  old_porp = (((predict_uncomp_cost_with_significant_values_model(ccn) + 
                predict_uncomp_cost_with_beta_model(ccn) + predict_uncomp_cost_rf(ccn))/3)
  )
  
  change = 100 * ((new_porp - old_porp) / old_porp)
  
  if (change < 0) {
    paste0("Average changed in predicted uncompensated costs is: ", round(change, digits = 2), "%")
  } else if (change > 0) {
    paste0("Average change in predicted uncompensated cost is: <strong>+", round(change, digits = 2), "%</strong>")
  } else {
    paste("We predict no change")
  }
}
mean_machine = function(ccn) {
  mean(
    predict_uncomp_cost_with_significant_values_model(ccn), 
    predict_uncomp_cost_with_beta_model(ccn), 
    predict_uncomp_cost_rf(ccn)
  )
}
big_ugly_mean_machine = function(ccn, med34, med19, dp, st) {
  new_porp = mean(
    predict_with_significant_values_the_bbb_change(ccn, med34, med19, dp, st), 
    predict_uncomp_cost_with_beta_model_and_BBB(ccn, med34, med19, dp), 
    predict_uncomp_cost_rf_BBB(ccn, med34, med19, dp, st)
  )
}
##GUI and stuff
ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap"),
  
  tags$style(HTML("
    body, label, input, button, select, h1, h2, h3, h4, h5, h6, .shiny-text-output {
      font-family: 'Open Sans', sans-serif;
      
      .shiny-text-output, .shiny-html-output {
      font-size: 20px;
  }
    }
    
    .grid-container {
      display: grid;
      grid-template-columns: 1fr 2fr;
      grid-template-areas:
        'sidebar map'
        'cost table'
        'cost table'
        'cost table';
      gap: 20px;
      padding: 10px;
    }
    input[type='checkbox'] {
    accent-color: #7B4BCC;
  }

  input[type='checkbox'] + label {
    color: #7B4BCC;
  }
    .bottom-container {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      grid-template-areas: 'cost table average';
      gap: 20px;
    }

    .sidebar  { grid-area: sidebar; }
    .map      { grid-area: map; }
    .cost     { grid-area: cost; }
    .table    { grid-area: table; }
    .average  { grid-area: average; }

    .irs-bar {
      background: #7B4BCC !important;
      border-top: 1px solid #7B4BCC !important;
      border-bottom: 1px solid #7B4BCC !important;
    }
    .irs-line {
      background: #e0e0e0;
      border: 1px solid #ccc;
    }
    .irs-slider {
      background: #7B4BCC !important;
      border: 1px solid #5F3C9F !important;
    }
    .irs-single,
    .irs-from,
    .irs-to {
      background-color: #7B4BCC !important;
      color: white !important;
      border: none !important;
    }

    table.model-table {
      width: 100%;
      border-collapse: collapse;
    }
    table.model-table th, table.model-table td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: center;
    }
    table.model-table th {
      background-color: #f2f2f2;
    }
  ")),
  
  titlePanel("Combined Regression Analysis Model with Service Area Map"),
  
  # Top section: Sidebar and Map
  div(class = "grid-container",
      
      div(class = "sidebar",
          numericInput("user_input", "Enter a CCN:", value = 360003),
          sliderInput("BBB_effect_MED19", "Change in Medicaid 19–34:", min = -100, max = 40, value = -15),
          sliderInput("BBB_effect_MED34", "Change in Medicaid 34–64:", min = -100, max = 40, value = -10),
          sliderInput("BBB_effect_DP", "Change in Direct Purchase 19–34:", min = -100, max = 40, value = -5),
          checkboxInput("ST_EX_MED", "Medicaid Expansion", value = TRUE),
          actionButton("run_button", "Run Analysis")
      ),
      
      div(class = "map",
          h3("Service Area Map"),
          withSpinner(leafletOutput("map_result", height = 400), type = 8, color = '#7B4BCC', proxy.height = 196 )
      )
  ),
  
  # Bottom section: Cost + Table + Average
  div(class = "bottom-container",
      div(class = "cost",
          h1("Actual Uncompensated Cost"),
          div(style = "margin-top: 12px;",
          withSpinner(uiOutput("uncomp_displayC"), type = 8, size = 0.3, color = '#7B4BCC', proxy.height = 32))
      ),
      
      div(class = "table",
          h1("Model Predictions"),
          withSpinner(tableOutput("model_table"), type = 8, size = 0.3, color ='#7B4BCC', proxy.height = 32),
      ),
      
      div(class = "average",
          h1("BBB Impact"),
          withSpinner(uiOutput("avg_bbb_impact"), type = 8, size = 0.3, color = '#7B4BCC', proxy.height = 32)
      )
  )
)

# Server
ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap"),
  
  tags$style(HTML("
    body, label, input, button, select, h1, h2, h3, h4, h5, h6, .shiny-text-output {
      font-family: 'Open Sans', sans-serif;
      
      .shiny-text-output, .shiny-html-output {
      font-size: 20px;
  }
    }
    
    .grid-container {
      display: grid;
      grid-template-columns: 1fr 2fr;
      grid-template-areas:
        'sidebar map'
        'cost table'
        'cost table'
        'cost table';
      gap: 20px;
      padding: 10px;
    }
    input[type='checkbox'] {
    accent-color: #7B4BCC;
  }

  input[type='checkbox'] + label {
    color: #7B4BCC;
  }
    .bottom-container {
      display: grid;
      grid-template-columns: 1fr 0.75fr 1fr;
      grid-template-areas: 'cost table average';
      gap: 20px;
    }

    .sidebar  { grid-area: sidebar; }
    .map      { grid-area: map; }
    .cost     { grid-area: cost; }
    .table    { grid-area: table; }
    .average  { grid-area: average; }

    .irs-bar {
      background: #7B4BCC !important;
      border-top: 1px solid #7B4BCC !important;
      border-bottom: 1px solid #7B4BCC !important;
    }
    .irs-line {
      background: #e0e0e0;
      border: 1px solid #ccc;
    }
    .irs-slider {
      background: #7B4BCC !important;
      border: 1px solid #5F3C9F !important;
    }
    .irs-single,
    .irs-from,
    .irs-to {
      background-color: #7B4BCC !important;
      color: white !important;
      border: none !important;
    }

    table.model-table {
      width: 100%;
      border-collapse: collapse;
    }
    table.model-table th, table.model-table td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: center;
    }
    table.model-table th {
      background-color: #f2f2f2;
    }
  ")),
  
  titlePanel("Combined Regression Analysis Model with Service Area Map"),
  
  # Top section: Sidebar and Map
  div(class = "grid-container",
      
      div(class = "sidebar",
          textInput("user_input", "Enter a CCN:", value = 360003),
          sliderInput("BBB_effect_MED19", "Change in Medicaid 19–34:", min = -100, max = 40, value = -15),
          sliderInput("BBB_effect_MED34", "Change in Medicaid 34–64:", min = -100, max = 40, value = -10),
          sliderInput("BBB_effect_DP", "Change in Direct Purchase 19–34:", min = -100, max = 40, value = -5),
          checkboxInput("ST_EX_MED", "Medicaid Expansion", value = TRUE),
          actionButton("run_button", "Run Analysis")
      ),
      
      div(class = "map",
          h3("Service Area Map"),
          withSpinner(leafletOutput("map_result", height = 400), type = 8, color = '#7B4BCC', proxy.height = 196 )
      )
  ),
  
  # Bottom section: Cost + Table + Average
  div(class = "bottom-container",
      div(class = "cost",
          h1("Actual Uncompensated Cost"),
          div(style = "margin-top: 12px;",
          withSpinner(uiOutput("uncomp_displayC"), type = 8, size = 0.3, color = '#7B4BCC', proxy.height = 32))
      ),
      
      div(class = "table",
          h1("Model Predictions"),
          withSpinner(tableOutput("model_table"), type = 8, size = 0.3, color ='#7B4BCC', proxy.height = 32),
      ),
      
      div(class = "average",
          h1("BBB Impact"),
          withSpinner(uiOutput("avg_bbb_impact"), type = 8, size = 0.3, color = '#7B4BCC', proxy.height = 32)
      )
  )
)

# Server
server <- function(input, output, session) {
  # Triggered when user clicks the button
  observeEvent(input$run_button, {
    ccn_val <- input$user_input
    output_mode = input$output_mode_choice
    BBBDP = input$BBB_effect_DP
    BBBMED34 = input$BBB_effect_MED34
    BBBMED19 = input$BBB_effect_MED19
    ST_MED = input$ST_EX_MED
    
    output$uncomp_displayC = renderUI({ UCPxM(ccn_val) })
    output$uncomp_displayP = renderText({ UCPxP(ccn_val) })
    output$model_resultLR <- renderText({ LRxM(ccn_val) })
    output$model_resultLR_BBB <- renderText({ LRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED) })
    output$model_resultBR <- renderText({ BRxM(ccn_val) })
    output$model_resultBR_BBB <- renderText({ BRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP) })
    output$model_resultRF <- renderText({ RFxM(ccn_val) })
    output$model_resultRF_BBB <- renderText({ RFxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED) })
    
    output$map_result <- renderLeaflet({ mx4(ccn_val) })
    
    output$model_table <- renderTable({
      data.frame(
        Model = c("Linear Regression", "Beta Regression", "Random Forest", "Average"),
        `2023 Fit` = c(
          LRxM(ccn_val),
          BRxM(ccn_val),
          RFxM(ccn_val),
          paste(round(((
            (predict_uncomp_cost_with_significant_values_model(ccn_val) + 
               predict_uncomp_cost_with_beta_model(ccn_val) + predict_uncomp_cost_rf(ccn_val))
          /3)*100), digits = 2),"%")
          
        ),
        `BBB` = c(
          LRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED),
          BRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP),
          RFxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED),
          paste(round((((
            predict_with_significant_values_the_bbb_change(ccn_val, BBBMED34, BBBMED34, BBBDP, ST_MED)+ 
            predict_uncomp_cost_with_beta_model_and_BBB(ccn_val, BBBMED34, BBBMED34, BBBDP)+ 
            predict_uncomp_cost_rf_BBB(ccn_val, BBBMED34, BBBMED34, BBBDP, ST_MED))/3)
            * 100)
            , digits = 2 ),"%")
        ),
        check.names = FALSE
      )
    })
    
    output$avg_bbb_impact <- renderText({
      per_change(ccn_val, BBBMED19, BBBMED34, BBBDP, ST_MED)
    })
  })
  
  # Run the same analysis once at startup
  observe({
    isolate({
      ccn_val <- input$user_input
      output_mode = input$output_mode_choice
      BBBDP = input$BBB_effect_DP
      BBBMED34 = input$BBB_effect_MED34
      BBBMED19 = input$BBB_effect_MED19
      ST_MED = input$ST_EX_MED
      
      output$uncomp_displayC = renderUI({ UCPxM(ccn_val) })
      output$uncomp_displayP = renderText({ UCPxP(ccn_val) })
      output$model_resultLR <- renderText({ LRxM(ccn_val) })
      output$model_resultLR_BBB <- renderText({ LRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED) })
      output$model_resultBR <- renderText({ BRxM(ccn_val) })
      output$model_resultBR_BBB <- renderText({ BRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP) })
      output$model_resultRF <- renderText({ RFxM(ccn_val) })
      output$model_resultRF_BBB <- renderText({ RFxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED) })
      
      output$map_result <- renderLeaflet({ mx4(ccn_val) })
      
      output$model_table <- renderTable({
        data.frame(
          Model = c("Linear Regression", "Beta Regression", "Random Forest", "Average"),
          `2023 Fit` = c(
            LRxM(ccn_val),
            BRxM(ccn_val),
            RFxM(ccn_val),
            paste(round(((
              (predict_uncomp_cost_with_significant_values_model(ccn_val) + 
              predict_uncomp_cost_with_beta_model(ccn_val) +
              predict_uncomp_cost_rf(ccn_val))/3)
              * 100)
              , digits = 2 ),"%")
            
          ),
          `BBB` = c(
            LRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED),
            BRxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP),
            RFxBBB(ccn_val, BBBMED34, BBBMED19, BBBDP, ST_MED),
            paste(round((((
              predict_with_significant_values_the_bbb_change(ccn_val, BBBMED34, BBBMED34, BBBDP, ST_MED)+ 
              predict_uncomp_cost_with_beta_model_and_BBB(ccn_val, BBBMED34, BBBMED34, BBBDP)+
              predict_uncomp_cost_rf_BBB(ccn_val, BBBMED34, BBBMED34, BBBDP, ST_MED))/3)
              * 100)
            , digits = 2 ),"%")
          ),
          check.names = FALSE
        )
      })
      
      output$avg_bbb_impact <- renderText({
        per_change(ccn_val, BBBMED19, BBBMED34, BBBDP, ST_MED)
      })
    })
  })
}
# Run the app
shinyApp(ui, server)