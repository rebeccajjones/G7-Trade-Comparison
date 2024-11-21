
# Join data ------

# Set out columns we want to keep from both data sets
cols_to_keep <- c("country_name", "subject", "measure", 
                  "period_id", "year", "area", "value")

# Combine ons and oecd data
combined <- ons_final %>%
  # Select columns from ONS
  select(all_of(cols_to_keep)) %>%
  # Add on services cvms
  rbind(services_area_clean %>%
          select(all_of(cols_to_keep))) %>%
  # Add on the same columns from OECD
  rbind(qna_clean %>%
          select(all_of(cols_to_keep)))

# Calculate total imports and exports for services CVMs to EU/Non-EU
area_cvm_total <- combined %>%
  filter(area %in% c("EU", "Non-EU")) %>%
  pivot_wider(names_from = country_name, values_from = value) %>%
  # Where UK (less PM) is null, take the value for UK instead
  mutate(`United Kingdom (less PM)` = case_when(
    is.na(`United Kingdom (less PM)`) ~ `United Kingdom`,
    TRUE ~ `United Kingdom (less PM)`)) %>%
  # Pivot data back to long form
  pivot_longer(cols = contains("United Kingdom"), 
               names_to = "country_name", values_to =  "value") %>%
  pivot_wider(names_from = subject, values_from = value) %>%
  mutate(
    `Exports of goods and services` = `Exports of goods` + `Exports of services`,
    `Imports of goods and services` = `Imports of goods` + `Imports of services`) %>%
  pivot_longer(cols = c(6:11), names_to = "subject", values_to = "value") %>%
  filter(subject %in% c("Exports of goods and services",
                        "Imports of goods and services"),
         !(measure == "CP" & country_name == "United Kingdom")
         )
  

# Calculate Trade Intensity ------

trade_intensity <- combined %>%
  # Filter for the three components we need to calculate trade intensity
  filter((subject == "GDP") | (subject %in% c("Exports of goods and services",
                              "Imports of goods and services") & area == "WW")) %>%
  # We do not have a GDP value for UK (less PM) - we will set
  # this to the same value as UK as the (less PM) refers to
  # the trade pert of the calculation.
  # Remove area var to pivot
  select(-area) %>%
  # Pivot wider by country name
  pivot_wider(names_from = country_name, values_from = value) %>%
  # Where UK (less PM) is null, take the value for UK instead
  mutate(`United Kingdom (less PM)` = case_when(
                      is.na(`United Kingdom (less PM)`) ~ `United Kingdom`,
                      TRUE ~ `United Kingdom (less PM)`)) %>%
  # Pivot data back to long form
  pivot_longer(cols = c(5:12), names_to = "country_name", values_to = "value") %>%
  # We will pivot the data again by subject to calculate trade intensity
  # Simplify names in columns to make pivoting easier
  mutate(subject = case_when(
                      subject == "GDP" ~ "GDP",
                      TRUE ~ tolower(substr(subject,1,7))
                      )) %>% 
  # Pivot wider by subject
  pivot_wider(names_from = subject, values_from = value) %>%
  # Calculate trade intensity
  mutate(intensity = (imports+exports)/GDP,
         subject = "Trade intensity",
         area = "WW") %>%
  rename(value = intensity) %>%
  # Keep only the columns that we need
  select(cols_to_keep)

# Add trade intensity and cvm totals to combined data
combined_final <- combined %>%
  rbind(trade_intensity) %>%
  rbind(area_cvm_total)
  

# Index Data ------

# Index the data to pre-specified yearly average
combined_indexed <- combined_final %>%
  # Join on yearly average for the groupings
  full_join(combined_final %>%
              # index_year is specified in main.R
              filter(year == index_year) %>%
              group_by(country_name, subject, measure, area) %>%
              # Summarise over the groupigns to get the yearly average
              summarise(index_avg = mean(value)) %>%
              # Select columns to join back on
              select(country_name, subject, measure, area, index_avg)) %>%
  # Calculate index
  mutate(value_indexed = (value/index_avg)*100) %>%
  select(-index_avg)


# Split data for exporting into spreadsheet
int_comps_data <- combined_indexed %>%
  filter(!(area %in% c("EU", "Non-EU")))

eu_neu_data <- combined_indexed %>%
  filter(area %in% c("EU", "Non-EU")) %>%
  mutate(area = case_when(
    str_detect(country_name, "PM") ~ paste0(area, " (less PM)"),
    TRUE ~ area
  )) 
  


# Export Data ------

# Write filepath for this machine
filepath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Department for International Trade/Europe Trade Analysis Unit - Documents/Data Analysis and Insights/Main R Folder/automated_narrative_charts/")

# Write data to sharepoint
write_csv(int_comps_data, paste0(filepath,"international_comparisons_data.csv"))  
write_csv(eu_neu_data, paste0(filepath,"eu_non-eu_data.csv"))  
