# Imports the Bloomberg price indices and tidies them up
## JULY 2023 (now imports both crude and refined indices)

# BASIC JONES ACT SETUP --------------------------------------------------

library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))

# CRUDE INDICES --------------------------------------------------
## IMPORT --------------------------------------------------

filepath <- file.path(dropbox, 'rawdata', 'orig', 'Bloomberg', 'Crude',
                      'bbg_indices2023.xlsx')

indices <- read_xlsx(filepath, sheet = 'pasted_indices',
                     skip = 1,
                     col_names = c('index', 'name', 'currency', 'units', 'des')) %>% 
  select(-des) %>% 
  mutate(index = str_extract(index, '([^\\s]+)')) %>% 
  # Noticed there is a duplicate in the raw data. This excludes duplicate along
  # all columns
  distinct()

prices <- read_xlsx(filepath, sheet = 'pasted_prices', skip = 3,
                    col_types = 'text')

prices_clean <- prices %>%
  pivot_longer(-Dates, names_to = 'index', values_to = 'price') %>% 
  filter(price != '#N/A N/A') %>% 
  # Importing Excel date as character. Need to use Excel origin for converting
  # to R date. See https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r
  mutate(date = as_date(as.numeric(Dates), origin = "1899-12-30"),
         price = as.numeric(price),
         index = str_extract(index, '([^\\s]+)')
         ) %>% 
  relocate(date) %>% 
  select(-Dates)
  
## COMBINE --------------------------------------------------

combined_bbg_crude <- prices_clean %>% 
  inner_join(indices, by = 'index')

## SAVE --------------------------------------------------
# saveRDS(combined_bbg_crude, file.path(dropbox, 'intdata', 'Bloomberg', 
#                                  'Crude', 'bbg_indices_crude.RDS'))


bbg_indices_crude_monthly <- combined_bbg_crude %>%
  mutate(Month = month(date), Year = year(date) ) %>%
  group_by(index,name,currency, units, Year, Month) %>%
    summarise(price = mean(price,na.rm = TRUE)) %>% ungroup()

saveRDS(bbg_indices_crude_monthly, 
        file.path(dropbox, 'intdata', 'Bloomberg', 
                  'Crude', 'bbg_indices_crude_monthly.RDS'))


