# clean_main_data.R
## standardize the data prep for the main data
## main eia data comes from `run_eia_api_v2.R`

# BASIC JONES ACT SETUP --------------------------------------------------
rm(list=ls())
library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))

eia_api_dir <- file.path(dropbox, 'rawdata', 'data', 
            'EIA_API_Output',"July2023")


# EIA SPOT PRICES ---------------------------------------------------------

eia_prices <- readRDS(file.path(eia_api_dir, 'EIASpotPrices.RDS'))

# with(eia_prices, table(`product-name`,`area-name`))

# eia_prices %>% 
#   select(series,`series-description`) %>%
#   unique() %>% print(n=Inf)

spot_prices <- eia_prices %>%
  mutate(ymdate = ymd(paste0(period, "-01")),
         Year = year(ymdate),
         Month = month(ymdate)) %>%
  rename(productName = `product-name`) %>% 
  mutate(prod_category = case_when(
           productName == "Conventional Regular Gasoline" ~ "Conventional",
           productName == "UK Brent Crude Oil" ~ "Crue", 
           productName == "Reformulated Motor Gasoline" ~ "RFG",
           productName == "WTI Crude Oil" ~ "Crude",
           productName == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
           productName == "No 2 Fuel Oil / Heating Oil" ~ "No2 Heating Oil",
           productName == "No 2 Diesel Low Sulfur (0-15 ppm)" ~ "ULSD",
           TRUE ~ as.character(NA))) %>%
  mutate(location = case_when(
            series %in% c("EER_EPMRU_PF4_RGC_DPG","EER_EPJK_PF4_RGC_DPG",
                "RWTC","EER_EPD2DXL0_PF4_RGC_DPG") ~ "GC",
            series %in% c("EER_EPD2F_PF4_Y35NY_DPG", "RBRTE",
                          "EER_EPD2DXL0_PF4_Y35NY_DPG",
                          "EER_EPMRU_PF4_Y35NY_DPG") ~ "NY",
            TRUE ~ as.character(NA))) %>%
     mutate(price = as.numeric(value),
            price_bbl = case_when(
                  units == "$/GAL" ~ price * 42, 
                  TRUE ~ price)) %>%
  select(Year,Month,ymdate,location,price_bbl_eia = price_bbl,
         productName)

saveRDS(spot_prices, file.path(dropbox,"intdata",
                 'clean_eia_spot_prices.RDS'))

# Bloomberg ---------------------------------------------

## using avg monthly price here since quantity is monthly
bbg_prices <- readRDS(file.path(dropbox,'intdata', 'Bloomberg', 'Crude', 
            'bbg_indices_crude_monthly.RDS'))

clean_prices_bbg <-   bbg_prices %>%
  mutate(prod_category = case_when(
               index %in% c('MOINY87P', 'MOIGC87P') ~ "Conventional",
               index %in% c('RBOB87PM', 'RBOBG87P') ~ "RFG",
               index %in% c('JETINYPR', 'JETIGCPR') ~ "Jet Fuel",
               index %in% c('DIEINULP', 'DIEIGULP') ~ "ULSD",
               index %in% c('FVCOFM', 'USCRLLSS') ~ "Crude",
               TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  mutate(location = if_else(
            index %in% c('MOINY87P','RBOB87PM','JETINYPR','DIEINULP','FVCOFM'),
                  "NY","GC")) %>%
  # these prices are actually in cents per gallon
  mutate(price_bbl = case_when(
                  units == "USd/gallon" ~ price * 42 / 100, 
                  TRUE ~ price)) %>%
  select(Year,Month,prod_category,location,price_bbl,bbg_index = index, bbg_name = name) %>%
  mutate(ymdate = ymd(paste(Year, Month, '01', sep = '-')))

saveRDS(clean_prices_bbg , file.path(dropbox,"intdata",
                 'clean_prices_bbg.RDS'))

## using avg monthly price here since quantity is monthly
bbg_prices <- readRDS(file.path(dropbox,'intdata', 'Bloomberg', 'Crude', 
                                'bbg_indices_crude_monthly.RDS'))


## version of clean price data for USGC to Los Angeles
clean_prices_bbg_la <-   bbg_prices %>%
  mutate(prod_category = case_when(
    index %in% c('MOILPR87', 'MOIGC87P') ~ "Conventional",
    index %in% c('JETFLAPL', 'JETIGCPR') ~ "Jet Fuel",
    index %in% c('DIEILAAM', 'DIEIGULP') ~ "ULSD",
    TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  mutate(location = if_else(
    index %in% c('MOILPR87','JETFLAPL','DIEILAAM'),
    "LA","GC")) %>%
  # these prices are actually in cents per gallon
  mutate(price_bbl = case_when(
    units == "USd/gallon" ~ price * 42 / 100, 
    TRUE ~ price)) %>%
  # LA jet fuel is actually $/gallon
  mutate(price_bbl = case_when(
    index == "JETFLAPL" ~ price_bbl * 100,
    TRUE ~ price_bbl)) %>%
  select(Year,Month,prod_category,location,price_bbl,bbg_index = index, bbg_name = name) %>%
  mutate(ymdate = ymd(paste(Year, Month, '01', sep = '-')))

saveRDS(clean_prices_bbg_la , file.path(dropbox,"intdata",
                                     'clean_prices_bbg_la.RDS'))


# EIA Prime Supplier ---------------------------------------------

## EIA Consumption Volume data ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# from CleanPrimeSupplier.R ------------------------------------------------
eia_prime <- readRDS(file.path(eia_api_dir,'EIAPrimeSupplierSalesVol.RDS')) 

  ## Prime supplier notes: 
  # - https://www.eia.gov/dnav/pet/pet_cons_prim_dcu_R10_a.htm
  # - this contains subcategories and super categories that totally subsume them 
  # so we shouldn't need to sum anything here 
  # - for gas, the main sub categores are Reformulated Gasoline and Conventional Gasoline
  #   - Oxygenated Gasoline (not in there for Padd 1 after 1995) 

prime_volume <- eia_prime %>%
  mutate(ymdate = ymd(paste0(period, "-01")),
         Year = year(ymdate),
         Month = month(ymdate)) %>%
  rename(productName = `product-name`) %>% 
  mutate(PADD = gsub("PADD ","",`area-name`),
         prod_category = case_when(
           productName == "Conventional Gasoline (No Oxy)" ~ "Conventional", 
           productName == "Reformulated Motor Gasoline" ~ "RFG",
           productName == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
           productName == "No 2 Fuel Oil / Heating Oil" ~ "No2 Fuel Oil",
           productName == "No 2 Distillate" ~ "No2 Distillate",
           TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  # Convert thousand gallons per day to millions of  barrels per month
  mutate(QUANTITY = as.numeric(value),
        barrels = 
           (QUANTITY * days_in_month(ymdate)) /(1000 * 42)) %>%
  select(prod_category, Year, Month, ymdate,PADD,
         PrimeSupply = barrels)

## Diesel is censored some times, but can construct as the difference 
## between total distillate and fuel oil 

td <- prime_volume %>%
    filter(prod_category %in% c("No2 Fuel Oil","No2 Distillate")) %>%
    pivot_wider(names_from = prod_category, values_from = PrimeSupply) %>%
    mutate(PrimeSupply = `No2 Distillate` - `No2 Fuel Oil`) %>%
    select(-`No2 Distillate`, -`No2 Fuel Oil`) %>%
    mutate(prod_category = "ULSD") # note assuming this is all low sulfur 

prime_volume <- bind_rows(prime_volume,td)    


# Refinery inputs of crude ------------------------------------------------
  # from CleanRefineryINputs.R

intdata <- readRDS(file.path(dropbox,"intdata",
                        "EIARefineryInputs", 'EIA_RefineryInputs.RDS'))

refinery_inputs <- intdata %>%
    mutate(RefineryCrudeInputs = (Volume_kbpd  * days_in_month(Date)) /1000 ) 


# Company level imports data -------------------------------------
# - note this cleaned in CleanCompanyImports_rename.do

# filtering out CA imports which are possible closer than Padd 3
eia_company_imp <- read_dta(file.path(dropbox, 'intdata', 
                    'EIACompanyLevelImports','CompanyImports_renamed.dta')) %>%
  filter(CNTRY_NAME != "CANADA") 

  
co_import_light <- eia_company_imp %>% 
  filter(PROD_NAME  == "CRUDE OIL" | PROD_NAME == "CRUDE OIL,FOREIGN",
         APIGRAVITY > 37.5) %>% # setting this to Brent API 
  mutate(prod_category = "Light Crude") 
         
subpadd_co_imports <- eia_company_imp %>% 
  mutate(prod_category = 
           case_when(
              PROD_NAME == "ALL OTHER MOTOR GAS BLENDING COMPONENTS" ~ "Conventional", 
              PROD_NAME == "MOTOR GAS, CONVENTIONAL, OTHER" ~ "Conventional", 
              PROD_NAME == "MGBC, GASOLINE TREATED AS BLENDSTOCK (GTAB)" ~ "Conventional", 
              PROD_NAME == "MGBC, CONVENTIONAL BLENDSTOCK FOR OXYGENATE BLENDING (CBOB)" ~ "Conventional", 
              PROD_NAME == "MGBC, REFORMULATED BLENDSTOCK FOR OXYGENATE BLENDING (RBOB)" ~ "RFG",
              PROD_NAME == "JET FUEL, KEROSENE-TYPE" ~ "Jet Fuel",
              PROD_NAME == "DISTILLATE, 15 PPM SULFUR AND UNDER" ~ "ULSD",
              PROD_NAME == "CRUDE OIL" ~ "Crude",
              PROD_NAME == "CRUDE OIL,FOREIGN" ~ "Crude",
              TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  bind_rows(co_import_light) %>%
  mutate(PADD = case_when(
              PORT_STATE %in% c("FL","GA","SC","NC","VA","WV") ~ "1C",
              PORT_STATE %in% c("MD","DE","NJ","PA","NY") ~ "1B",
              PORT_STATE %in% c("CT","RI","MA","NH","VT","ME") ~ "1A",
              TRUE ~ as.character(PORT_PADD)) ) %>% 
  mutate(date = ymd(paste(YEAR, MONTH, '01', sep = '-')),
         barrels = QUANTITY/ 1000) %>%
  ## collapse blending components 
  group_by(prod_category,YEAR,MONTH, date, PADD) %>% 
    summarise(barrels = sum(barrels, na.rm = TRUE)) %>% ungroup() %>%
  select(prod_category,PADD, Year = YEAR, Month = MONTH, ymdate = date,
         Imports = barrels)


# EXPORTS ---------------------------------------------------------
  ## using PADD exports by destination from EIA
  # - https://www.eia.gov/dnav/pet/PET_MOVE_EXPCP_A2_R10_EP00_EEX_MBBL_M.htm

eia_exports_dest <- readRDS(file.path(eia_api_dir,'EIAPaddExportsDestination.RDS')) 

# keep track of exports to central america and caribbean 

central_list <- c("Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", 
    "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador", 
    "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", 
    "Nicaragua", "Panama", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", 
    "Saint Vincent and the Grenadines", "Trinidad and Tobago")

# Create a named list of country codes
central_codes <- list(
  "Antigua and Barbuda" = "ATG",
  "The Bahamas" = "BHS",
  "Barbados" = "BRB",
  "Belize" = "BLZ",
  "Costa Rica" = "CRI",
  "Cuba" = "CUB",
  "Dominica" = "DMA",
  "Dominican Republic" = "DOM",
  "El Salvador" = "SLV",
  "Grenada" = "GRD",
  "Guatemala" = "GTM",
  "Haiti" = "HTI",
  "Honduras" = "HND",
  "Jamaica" = "JAM",
  "Mexico" = "MEX",
  "Nicaragua" = "NIC",
  "Panama" = "PAN",
  "Puerto Rico" = "PRI",
  "Saint Kitts and Nevis" = "KNA",
  "Saint Lucia" = "LCA",
  "Saint Vincent and the Grenadines" = "VCT",
  "Trinidad and Tobago" = "TTO"
)

  ## note, it looks like Cuba is missing here. never has any exports. i think exports are banned to Cuba 

exports_dest <- eia_exports_dest %>%
  filter(units == "MBBL") %>% # this file contains both bbl and bbl/day
  mutate(ymdate = ymd(paste0(period, "-01")),
         Year = year(ymdate),
         Month = month(ymdate)) %>%
  filter(!grepl("PADD",`area-name`)) %>% # filter `area-name` contains the string "PADD"
  separate(duoarea, into = c("origin","dest"), sep = "-") %>% # split duoarea into two columns separated by - 
  mutate(PADD = case_when(
              origin == "R10" ~ 1,
              origin == "R20" ~ 2,
              origin == "R30" ~ 3,
              origin == "R40" ~ 4,
              origin == "R50" ~ 5,
              TRUE ~ as.numeric(NA)  )) %>% 
  filter(!is.na(PADD)) %>% # this filters out aggregate exports
  mutate(prod_category = 
           case_when(`product-name` == "Finished Motor Gasoline" ~ "Conventional", 
                     `product-name` == "Gasoline Blending Components" ~ "Conventional", 
                     `product-name` == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
                     `product-name` == "Distillate Fuel Oil, 0 to 15 ppm Sulfur" ~ "ULSD",
                     `product-name` == "Distillate Fuel Oil" ~ "Distillate - All",
                     `product-name` == "Crude Oil" ~ "Crude",
                     TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  mutate(barrels = value / 1000,
         barrels_central = 
            if_else(`area-name` %in% central_codes, value/1000,0 ))  %>%
  ## collapse blending components 
  group_by(PADD, prod_category,Year,Month, ymdate) %>% 
    summarise(barrels = sum(barrels, na.rm = TRUE),
              barrels_central = sum(barrels_central, na.rm = TRUE)) %>% ungroup() %>%
  rename(Exports_All = barrels,
         Exports_CentAm = barrels_central) %>%
  mutate(Exports_NonCentAm = Exports_All - Exports_CentAm)

  
# PADD MOVEMENTS ---------------------------------------------------------
eia_movements <- readRDS(file.path(eia_api_dir, 'EIAMovementsTankerBarge.RDS'))

movements <- eia_movements %>%
  mutate(PADD = case_when(
                duoarea == 'R1X-R30' ~ "1A",
                duoarea == 'R1Y-R30' ~ "1B",
                duoarea == 'R1Z-R30' ~ "1C",
                duoarea == 'R10-R30' ~ "1",
                TRUE ~ as.character(NA))) %>%
  rename(productName = `product-name`) %>% 
  mutate(prod_category = case_when(
           productName == "Conventional Motor Gasoline" ~ "Conventional",
           productName == "Conventional Gasoline Blending Components" ~ "Conventional", 
           productName == "Reformulated Motor Gasoline" ~ "RFG",
           productName == "Reformulated Gasoline Blending Components" ~ "RFG",
           productName == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
           productName == "Distillate Fuel Oil" ~ "No2 Distillate",
           productName == "Distillate Fuel Oil, 0 to 15 ppm Sulfur" ~ "ULSD",
           productName == "Crude Oil" ~ "Crude",           
           TRUE ~ as.character(NA))) %>%
  filter(!is.na(prod_category)) %>%
  mutate(ymdate = ymd(paste0(period, "-01")),
         Year = year(ymdate),
         Month = month(ymdate)) %>%
  # Convert thousand gallons per month to millions of  barrels per month
  mutate(QUANTITY = as.numeric(value),
         barrels = QUANTITY / 1000) %>%
  group_by(Year,Month,prod_category,PADD) %>%
    summarize(PADD3to1movements = sum(barrels, na.rm = TRUE)) %>%
    ungroup()


# CONSTRUCT A SINGLE SUBPADD LEVEL DF FOR PADD 1------------------------------

imp_heavy_crude <- subpadd_co_imports %>%
  filter(prod_category == "Crude") %>%
  inner_join(subpadd_co_imports %>%
              filter(prod_category == "Light Crude") %>%
              mutate(prod_category = "Crude") %>%
               rename(Imports_light = Imports)) %>%
  mutate(Imports_heavy = Imports - Imports_light)

# for consumption, just use "light" consumption (to match import restriction)
# computing this as observed refinery inputs - heavy imports 
df_crude_cons <- refinery_inputs %>%
  filter(PADD == 1) %>%
  select(Year,Month,RefineryCrudeInputs) %>%
  mutate(prod_category = "Crude", PADD = "1B", 
          ymdate = ymd(paste(Year, Month, '01', sep = '-'))) %>%
  left_join(imp_heavy_crude) %>%
  mutate(Consumption = RefineryCrudeInputs - Imports_heavy) %>%
  select(-Imports,-Imports_light,-Imports_heavy,-RefineryCrudeInputs)


# use light crude instead of all crude for imports
imp_df <- subpadd_co_imports %>%
  filter(prod_category != "Crude") %>%
  mutate(prod_category = 
          if_else(prod_category == "Light Crude","Crude",prod_category))

combined_subpadd_volumes<- prime_volume %>%
  filter(PADD %in% c("1A","1B","1C")) %>%
  filter(prod_category %in% c("Conventional","ULSD","Jet Fuel")) %>%
  rename(Consumption = PrimeSupply) %>%
  bind_rows(df_crude_cons) %>%
  left_join(movements) %>%
    replace_na(list(PADD3to1movements = 0)) %>%
    rename(Movements = PADD3to1movements) %>%
  left_join(imp_df) %>%
    replace_na(list(Imports = 0))


saveRDS(combined_subpadd_volumes, file.path(dropbox,"intdata",
                 'combined_subpadd_volumes.RDS'))

# PADD 5 imports
padd5_imp <- imp_df %>%
  filter(PADD=="5")
saveRDS(padd5_imp, file.path(dropbox,"intdata",'padd5_imp.RDS'))



# Create a PADD 3 df 
df_crude_cons <- refinery_inputs %>%
  filter(PADD == 3) %>%
  select(Year,Month,Consumption = RefineryCrudeInputs) %>%
  mutate(prod_category = "Crude", PADD = "3", 
          ymdate = ymd(paste(Year, Month, '01', sep = '-')))


export_df <- exports_dest %>%
                    filter(PADD == 3) %>%
    select(Year,Month,prod_category,Exports_All, Exports_NonCentAm)

price_df <- clean_prices_bbg %>%
  select(-bbg_index,-bbg_name) %>%
  pivot_wider(names_from = location, 
            values_from = price_bbl, 
            names_prefix = "price_") %>%
  mutate(price_diff = price_NY - price_GC) 


main_padd3_df <- 
  prime_volume %>%
  filter(PADD == "3") %>%
  filter(prod_category %in% c("Conventional","ULSD","Jet Fuel")) %>%
  rename(Consumption = PrimeSupply) %>%
  bind_rows(df_crude_cons) %>%
  left_join(export_df) %>%
    replace_na(list(Exports_All = 0,Exports_NonCentAm = 0)) %>%
  left_join(price_df)

saveRDS(main_padd3_df , file.path(dropbox,"intdata",
                 'main_padd3_df.RDS'))

