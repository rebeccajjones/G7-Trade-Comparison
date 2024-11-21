
# Clean data ---

# Re-format MRET
mret_servs <- as.data.frame(t(mret_raw[,-1]))
colnames(mret_servs) <- mret_raw$X1
row.names(mret_servs) <- seq(1:length(row.names(mret_servs)))

# Get names of columns for pivoting data to long form
cols_to_pivot_s <- colnames(mret_servs)[8:length(colnames(mret_servs))]

# Define names of services series
servs_series <- c("IKBE","IKBF","S23X","S23Y","S26B","S26C","S28N","S28O","S2AZ","S2B2","S2DD","S2DE",
                  "S2FP","S2FQ","S2I3","S2I4","S2KF","S2KG","S2MR","S2MS","S2P5","S2P6","S2PK","S2PL",
                  "IKBB","IKBC","MTN7","MTN6","FKOA","FHME","FAPO","APQL","FDSG","FIOU","FDTF","FIPT",
                  "FDYI","FITY","FEBA","FIVX","FDYQ","FIUG","FEHH","FIWF","FGXJ","FLQJ","FGZA","FLSA")

# Clean
servs <- mret_servs %>%
  # Get rid of columns we are not interested in
  select(-c("PreUnit": "Important Notes")) %>%
  # Pivot to long form
  pivot_longer(names_to = "period", cols = all_of(cols_to_pivot_s)) %>%
  # Define period types
  mutate(period_type = case_when(
    nchar(period) == 4 ~ "Y",
    nchar(period) == 7 ~ "Q", 
    nchar(period) == 8 ~ "M"
  ),
  # Add year variable
  year = as.numeric(substr(period,1,4)),
  value = as.numeric(value)/1000) %>%
  # Filter for quarters and the years of data we are interested in
  filter(period_type == "Q",
         year >= data_start,
         CDID %in% servs_series) 

# Define services series with 
tot_servs_series <- c("IKBB", "IKBC", "IKBE", "IKBF")

# Filter for sectors to clean title column - different structure to total
servs_sect <- servs %>%
  filter(!(CDID %in% tot_servs_series)) %>%
  separate(Title, into = c("subject", "sector", "area", "direction",
                           "source", "measure", "adj", "unit"), sep = ": ")

# Filter for total services to clean title column - different structure to sectors
total_servs <- servs %>%
  filter(CDID %in% tot_servs_series) %>%
  separate(Title, into = c("subject", "area", "direction", "source",
                           "measure", "adj"), sep = ": ") %>%
  mutate(sector = "Total Services", 
         unit = "£m")

# Combine sectors and total back together
servs_combined <- rbind(servs_sect, total_servs) %>%
  select(sector, direction, measure, period, value) %>%
  # Add year and quarter column
  mutate(year = substr(period,1,4),
         quarter = substr(period,6,7))

# Calculate change from 2019
servs_final <- servs_combined %>%
  # Add on cols with 2019 values to calculate difference
  left_join(servs_combined %>%
              filter(year == 2019) %>% 
              select(sector, direction, measure, quarter, val_old = value)) %>%
  # Calculate change from 2019
  mutate(prev_q_change = value - val_old,
         preq_q_percent = prev_q_change/val_old) %>%
  # Filter for the data we want
  filter(as.numeric(year) >= servs_from)

# Write data ---

write_csv(servs_final, paste0(filepath,"services.csv"))  
