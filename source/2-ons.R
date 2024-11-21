
# Retrieve data ------

# Function for pulling data from online file
pull_from_ons <- function(dataset_url) {
  
  data_name <- deparse(substitute(dataset_url))
  save_name <- substr(data_name, 1, str_locate(data_name,"_")[1]-1)
  save_path <- paste0("data/raw/", save_name,".csv")
  
  download.file(dataset_url, save_path, mode = "wb")
  
}

# File urls for ONS data
mret_location <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet/current/mret.csv"
pn2_location  <- "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/secondestimateofgdp/current/pn2.csv"
pnbp_location <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/balanceofpayments/datasets/balanceofpayments/current/pnbp.csv"
ukea_location <- "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/unitedkingdomeconomicaccounts/current/ukea.csv"

# Pull ONS data from ONS website
pull_from_ons(pn2_location)
pull_from_ons(mret_location)
pull_from_ons(pnbp_location)
pull_from_ons(ukea_location)

# Read ONS data into R
# List files to read in
files <- list.files("data/raw", pattern="*.csv")
# Read files into R giving them the name 'dataset_raw'
for (i in 1:length(files)) {
  assign(paste0(str_remove(files[i], ".csv"),"_raw"), 
         read_csv(paste0("data/raw/",files[i]), col_names = FALSE))
}

# Read in ref data
cdids <- read_csv("data/ref/series_ids.csv")


# Clean data ------

# Set function to clean all time series in one go
clean_ons_time_series <- function(df) {
  
  # Transpose data
  df_tp <- as.data.frame(t(df[,-1]))
  colnames(df_tp) <- df$X1
  row.names(df_tp) <- seq(1:length(row.names(df_tp)))
  
  # Get names of columns for pivoting data to long form
  cols_to_pivot <- colnames(df_tp)[8:length(colnames(df_tp))]
  
  # Pivot data to long form
  df_final <- df_tp %>%
    # Get rid of columns we are not interested in
    select(-c("PreUnit": "Important Notes")) %>%
    # Pivot to long form
    pivot_longer(names_to = "period", cols = all_of(cols_to_pivot)) %>%
    # Define period types
    mutate(period_type = case_when(
      nchar(period) == 4 ~ "Y",
      nchar(period) == 7 ~ "Q", 
      nchar(period) == 8 ~ "M"
    ),
    # Add year variable
    year = as.numeric(substr(period,1,4))) %>%
    # Filter for quarters and the years of data we are interested in
    filter(period_type == "Q",
           year >= data_start) %>%
    # Change column types
    mutate(Title = as.character(Title), 
           CDID = as.character(CDID),
           value = as.numeric(as.character(value))) %>%
    # Join on info and filter for the years we are interested in
    inner_join(cdids) 
  
  return(df_final)
  
}

# Clean data frames - apply finction over all datasets
clean_ts <- lapply(list(mret_raw, pn2_raw, pnbp_raw, ukea_raw), clean_ons_time_series)

# Set names for dfs
ons_names <- c("mret", "pn2", "pnbp", "ukea")

# Pull dfs from list into environment
list2env(setNames(clean_ts, ons_names), .GlobalEnv)

# Combine all dfs into one
ons_combined <- mret %>% 
  mutate(source = "mret") %>%
  rbind(pn2 %>% mutate(source = "pn2")) %>%
  rbind(pnbp %>% mutate(source = "pnbp")) %>%
  rbind(ukea %>% mutate(source = "ukea"))


# Pick series depending on revision cycle ---

pick_series <- ons_combined %>% 
  # Find availability of each series across the sources
  distinct(CDID, title, source) %>% 
  # Add flag so availability is clear when data is pivoted
  mutate(flag = "Y") %>% 
  # Pivot data
  pivot_wider(names_from = source, values_from = flag, values_fill = "N") %>%
  # Add flags showing which series are available for the different points
  # in the ONS revisions cycle.
  mutate(# Available first - but revised after BoP/UKEA
         avail_1 = 
           case_when(
             (mret == "Y" | pn2 == "Y") ~ "Y",
             TRUE ~ "N"),
         # Available second - but first to be revised
         avail_2 = 
           case_when(
             (pnbp == "Y" | ukea == "Y") ~ "Y",
             TRUE ~ "N"),
         # Set choice to flag which source is the first choice based on the 
         # revision cycle
         choice = 
          case_when(
             (revision_cycle == FALSE & avail_1 == "Y") ~ 1,
             revision_cycle == FALSE ~ 2,
             (revision_cycle == TRUE & avail_2 == "Y") ~ 2,
             revision_cycle == TRUE ~ 1),
         # Based on choice, pick which source to pull from for each series
         source = 
           case_when(
             (choice == 1 & mret == "Y") ~ "mret",
             (choice == 1 & pn2 == "Y") ~ "pn2",
             (choice == 2 & pnbp == "Y") ~ "pnbp",
             TRUE ~ "ukea"
           )) %>%
  select(CDID, source)

# Filter data for unique series, keeping the source selected by the above 
ons_final <- ons_combined %>%
  # Use inner_join of pick_series to filter datasets
  inner_join(pick_series) %>%
  # Re-format period var
  mutate(period_id = str_replace(period, " ", "-"))


# PULL IN EU/NONEU FROM ALT LOCATION

# Set file location
file_location <- "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/breakdownoftrade/current/additionaltradetablesforgdppublication.xlsx"

# Download file from ONS website
download.file(file_location, "data/raw/uktradebyregion.xls", mode = "wb")

# Pull in the whole excel sheet with annual and quarterly tables
first_col <- readxl::read_xlsx("data/raw/uktradebyregion.xls", sheet = "4. Trade in Services CVM", skip = 7) %>%
  # Pull the first column into a vector to identify where the quarterly data starts
  select(`Dataset identifier`) %>%
  pull()

# Find where the quarterly data starts - it starts after a blank line, i.e. where there is a blank value
# add 10 to this to skip past descriptive rows
skip_point <- which(is.na(first_col)) + 10

# Pull in quarterly data
services_area_cvm <- readxl::read_xlsx("data/raw/uktradebyregion.xls", sheet = "4. Trade in Services CVM", skip = skip_point) %>%
  # Rename column for easier manipulation
  rename("period" = "Dataset identifier") %>%
  # Get rid of balance columns 
  select(-c("JLV5", "JLUE")) %>%
  # Filter for the time period of interest
  filter(as.numeric(substr(period,1,4)) >= data_start) %>%
  # Put data in long form
  pivot_longer(cols = c("JLV3", "JLV4", "JLTT", "JLTZ"), names_to = "CDID", values_to = "value")

# We need to add extra information columns ont this - we can pull this reference information out of the spreadsheet.

# Pull out reference information
refs_info <- readxl::read_xlsx("data/raw/uktradebyregion.xls", sheet = "4. Trade in Services CVM", range = "A7:G8") %>%
  # Transpose this
  t()

# Pull the reference info into a nicer format to manipulate
refs <-tibble(desc = row.names(refs_info)[2:7],
               CDID = refs_info[2:7,1])

# Add reference info to services CVMs by area

services_area_clean <- services_area_cvm %>%
  # Join on reference information
  left_join(refs, by = "CDID") %>%
  mutate(
    # Define area column by detecting it from the description
    area = case_when(
      str_detect(desc, "Non-EU") ~ "Non-EU",
      TRUE ~ "EU"),
    # Define subject by detecting it from the description column
    subject = case_when(
      str_detect(desc, "Exports") ~ "Exports of services",
      TRUE ~ "Imports of services"),
    # Add extra info cols
    measure = "CVM",
    country_name = "United Kingdom",
    period_id = str_replace(period, " ", "-"),
    year = as.numeric(substr(period,1,4)))
