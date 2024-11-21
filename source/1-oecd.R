
# Retrieve data ------

# Function to make API request and retrieve data
get_oecd_data <- function(api_url) {
  response <- GET(api_url)
  stop_for_status(response)
  content(response, "text", encoding = "UTF-8")
}

# Set API url
url <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.0/Q.Y.CAN+FRA+DEU+ITA+JPN+USA.S1..B1GQ+P6+P7+P72+P71+P62+P61.._Z...V+L.N.T0102?startPeriod=",data_start,"-Q1&dimensionAtObservation=AllDimensions")

# Get API response
test_data <- get_oecd_data(url)

# Pull API response into R object
parsed_test <- fromJSON(test_data)



# API to data frame ------

# Pull dataset from API response
qna_raw <- parsed_test$dataSets[[2]]

# Transpose this data
qna_transposed <- data.frame(series = colnames(qna_raw),
                   values = as.data.frame(t(qna_raw))) %>%
  # Pull value out of list
  extract(X1, into = "value", regex = "\\((.*?)\\,", convert = TRUE)

# Rename rows
row.names(qna_transposed) <- seq(1:length(row.names(qna_transposed)))



# We need to add identifiers and references to this dataset to make it mean something

# Pull names of identifiers from API response
names <- tolower(parsed_test$structure$dimensions$observation$name) %>%
  str_replace_all(" ", "_")

# Each of these has a corresponding table in the API response, giving possible values 
# for each of the identifiers. Pull out each of these tables and store them in a list:

# To do this in one go with a loop, set loop parameters:
n <- 0 # loop count variable
references <- list() # blank list to store data frames in

# Loop over each identifier:
for (i in names) {
  
  # Set loop count to identify element in list
  n <- n+1
  
  # Pull out parameter from list structure
  df <- parsed_test$structure$dimensions$observation$values[[n]] %>%
    # Rename variables for joining back onto raw data
    select(!!paste0(i, "_id") := id, !!paste0(i, "_name") := name) %>%
    # Add row to facilitate the join to the raw data
    mutate(!!paste0(i, "_join") := as.character(row_number() - 1))
  
  # Give this dataframe the name of the parameter
  #assign(i, df)
  references[[n]] <- df
  
}

# Split the ID variable in qna_transposed to join references onto each
# of the individual idenifiers
qna_split_id <- qna_transposed %>%
  # Separate series column to join on parameter dataframes
  separate(series, paste0(names, "_join"), sep = ":", remove = FALSE)

# The most efficient way to join each of the 14 id datasets to the qna data is to
# join each of the reference datasets to the raw data while still in the list 
# and then reduce the list into one dataset

# Create function to join each of the datasets and pass to the list
# This will also then remove all join columns to make reducing the data in the next step easier
join_func <- function(df){left_join(qna_split_id, df) %>% select(-contains("_join"))}

# Pass this function to each of the reference datasets
qna_join_to_refs <- lapply(references, join_func)

# Join all datasets in the list together to get the qna data with all identifiers attached
qna_full <- Reduce(full_join, qna_join_to_refs)

#rm(list=setdiff(ls(), "parsed_test"))



# Clean data ------

# Clean data to be consistent with ONS data
qna_clean <- qna_full %>%
  rename(country_name = reference_area_name,
         period_id = time_period_id) %>%
  mutate(
    year = as.numeric(substr(period_id,1,4)),
    # Consistent column for how the data is measured
    measure = case_when(
      price_base_id == "L" ~ "CVM",
      price_base_id == "V" ~ "CP"
    ),
    # Consistent column for what is being measured
    subject = case_when(
      transaction_name == "Gross domestic product" ~ "GDP",
      TRUE ~ transaction_name
    ),
    # Add column for area imports/exports sent to/from
    area = case_when(
      subject == "GDP" ~ country_name,
      TRUE ~ "WW"
    )
  )

