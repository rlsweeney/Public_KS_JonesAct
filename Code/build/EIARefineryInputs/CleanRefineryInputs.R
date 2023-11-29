# CleanRefineryInputs.R

# - read in the gross inputs variable from the EIA refinery utilization page
# https://www.eia.gov/dnav/pet/PET_PNP_UNC_A_EPXXX2_YIY_MBBLPD_M.htm
# - this series wasn't in the API pull Jonathan did 
# - we are going to use it to construct total Q demanded in padd 1 for crude 

# BASIC JONES ACT SETUP --------------------------------------------------

library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))



indata <- read_excel(file.path(dropbox,"rawdata/orig/EIARefineryInputs",
                    'PET_PNP_UNC_A_EPXXX2_YIY_MBBLPD_M_20230613.xls'),
                    sheet = "Data 1", skip = 1)


snames <- indata %>% 
    slice(1:2) %>%
    pivot_longer(!Sourcekey, names_to = "rlab") %>%
    select(-Sourcekey) %>%
    rename(Sourcekey = rlab, 
           Description = value)


indata %>% 
    filter(!row_number() == 1)


tabdata <- read_excel(file.path(dropbox,"rawdata/orig/EIARefineryInputs",
                    'PET_PNP_UNC_A_EPXXX2_YIY_MBBLPD_M_20230613.xls'),
                    sheet = "Data 1", skip = 2)

tabdata %>%
    pivot_longer(!Date, names_to = "Description")


EIA_RefineryInputs <- tabdata %>%
    pivot_longer(!Date, names_to = "Description", values_to = "Volume_kbpd") %>%
    mutate(PADD = case_when(
            Description == "U.S. Gross Inputs to Refineries (Thousand Barrels Per Day)" ~ 0,
            Description == "East Coast (PADD 1) Gross Inputs to Atmospheric Crude Oil Distillation Units (Thousand Barrels per Day)" ~ 1,
            Description == "Midwest (PADD 2) Gross Inputs to Refineries (Thousand Barrels Per Day)" ~ 2,
            Description == "Gulf Coast (PADD 3) Gross Inputs to Refineries (Thousand Barrels Per Day)" ~ 3,
            Description == "Rocky Mountains (PADD 4) Gross Inputs to Refineries (Thousand Barrels Per Day)" ~ 4,
            Description == "West Coast (PADD 5) Gross Inputs to Refineries (Thousand Barrels Per Day)" ~ 5,
            TRUE ~ as.numeric(NA)
    )) %>%
    mutate(Year = year(Date), Month = month(Date))



saveRDS(EIA_RefineryInputs, file.path(dropbox,"intdata",
                        "EIARefineryInputs", 'EIA_RefineryInputs.RDS'))
