# Load Packages

library(httr) # Useful for web authentication
library(jsonlite) # Convert JSON files into data frames
library(tidyverse) # For all things tidy

#### Using httr and jsonlite ####

# Create all of the different parts of the url seperatly
# This is useful for creating a function later

url_endpoint <- "https://api.census.gov/data/"

api_key <- "key=0c9acdaa232bfda30bb478676afc09cc6419a57d"
# This is Ethan's api key, if you want access the api please request your own
# key and replace mine with it.

dataset <- "2020/dec/pl?"

variables <- "get=P1_001N&"

state <- "for=state:*&"

# Create the url to access the api

url <- paste(url_endpoint, dataset, variables, state, api_key, sep="")




url # Print out url that we can test in our browsers


# Create a data frame using this format

data <- 
  url %>% 
  GET() %>% 
  content(as = "text",
          encoding = "UTF-8") %>% 
  fromJSON() %>% 
  data.frame()

view(data) # Look at the data

data <- data[-1, ] # Remove the first row

# Renaming columns

colnames(data)[1] <- "pop"

colnames(data)[2] <- "state"

# Fixing data types

data$pop <- as.integer(data$pop)

# Summary statistics

total_population <- sum(data$pop)

total_population

# Basically useless with the tidycensus package

#### Working with tidycensus ####

library(tidycensus)

census_api_key("0c9acdaa232bfda30bb478676afc09cc6419a57d")

#### Average Age ####

age20 <- get_decennial(geography = 'state',
                       variables = 'P13_001N',
                       year = 2020,
                       sumfile = 'dhc')

age20 %>% # Chatgpt makes these graphs look a lot better
  ggplot(aes(x = value, y = reorder(NAME, value))) +
  geom_point(color = "#2c7fb8", size = 3) + 
  labs(
    title = "Median Age by State (2020)",
    x = "Median Age",
    y = "State"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

#### Population ####

dhc.vars <- load_variables(2020, "dhc", cache = TRUE)
# View all of the variables and their corresponding codes

total_pop <-get_decennial(geography = 'state',
                          variables = 'P1_001N',
                          year = 2020,
                          sumfile = 'dhc')

total_pop %>% 
  ggplot(aes(x = value, y = reorder(NAME, value))) +
  geom_point(color = "#2c7fb8", size = 3) + 
  labs(
    title = "Median Age by State (2020)",
    x = "Median Age",
    y = "State"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

#### Other Census' ####

acs5.vars <- load_variables(2021, "acs5")
subject <- load_variables(2021, "acs5/subject")

#### Maps ####

library(sf)










