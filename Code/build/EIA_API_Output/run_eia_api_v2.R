# run_eia_api_v2.R

# BASIC JONES ACT SETUP --------------------------------------------------
library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))

# Load the packages we need 
#packages <- c("devtools")
# Using this package: https://github.com/jameelalsalam/eia2
# devtools::install_github("jameelalsalam/eia2")

library(eia2)

# API Setup --------------------------------------------------

API_KEY <- '1482d2cfad0f3ea05f4918d6cda04173'
eia_set_key(API_KEY)


# Important: Update this every time you run the code! W
# - we are downloading "raw" data and this code gets the most recent data. 
# - update this outdir path to generate a versioned directory of the data 
outdir <- file.path(dropbox, 'rawdata', 'data', 'EIA_API_Output',
              "July2023")

# GET PRIME SUPPLIER DATA --------------------------------------------------
# - https://www.eia.gov/dnav/pet/pet_cons_prim_dcu_R1Y_m.htm

base <- "petroleum/cons/prim/"
eia2(base)

## get a list of valid facets
duolist <- eia2_facet(base,"duoarea")
prodlist <- eia2_facet(base,"product")
## can then print these to pick out the ones we want 

# just downloading this for the main product categories we actually use, for padds 
my_prods <- c("EPD2", # no 2 distillate (all), 
              "EPD2F", # no 2 fuel oil 
              "EPD2DXL0", # ULSD
              "EPM0", # Total gasoline
              "EPM0R", # RFG
              "EPM0U", # conventional gas (no oxy)
              "EPJK") # Jet fuel 

my_regions <- c('R1X', #   PADD 1A
                'R1Y', # PADD 1B 
                'R1Z', # PADD 1C
                'R20', #   PADD 2
                'R30', #   PADD 3
                'R40', #   PADD 5
                'R50') #   PADD 5

# EIA api has a 5000 row limit. make sure a single query doesn't exceed that
df_sub <- eia2(base,
               frequency = "monthly",
               facets = list(
                 duoarea = my_regions[1],
                 product = my_prods),
               data_cols = c("value"),
               start = "2000-01")

nrow(df_sub)


# LOOP THROUGH REGIONS AND BIND TOGETHER 
combined <- NA

for (i in my_regions) {
  td <- df_sub <- eia2(base,
                       frequency = "monthly",
                       facets = list(
                         duoarea = i, product = my_prods),
                       data_cols = c("value"),
                       start = "2000-01")
  if (length(combined) == 1) {
      combined <- td
    } else {
      combined <- bind_rows(combined, td)
    }
}

# saveRDS(combined, file.path(outdir, 'EIAPrimeSupplierSalesVol.RDS'))


# GET SPOT PRICES --------------------------------------------------
# - https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm

base <- "petroleum/pri/spt/"
eia2(base)

df_sub <- eia2(base,
               frequency = "monthly",
              #  facets = list(
              #    duoarea = my_regions[1],
              #    product = my_prods),
               data_cols = c("value"),
               start = "2000-01")

nrow(df_sub)

# whole thing is less than 5000 rows, so can just save this 

# saveRDS(df_sub, file.path(outdir, 'EIASpotPrices.RDS'))


# GET EXPORTS --------------------------------------------------
## series we want is padd exports by destination 
## https://www.eia.gov/dnav/pet/pet_move_expcp_a2_r10_ep00_eex_mbbl_m.htm
## Note: This data series is only available from 2010 onwards. Before that, exports just reported at the PADD level. 
  ## In this call, I started things in 2000, but it only returns aggregate prior to 2010.

base <- "petroleum/move/expcp/"
eia2(base)

## get a list of valid facets
duolist <- eia2_facet(base,"duoarea")
prodlist <- eia2_facet(base,"product")

# Here note that h
## can then print these to pick out the ones we want 

# just downloading this for the main product categories we actually use, for padds 
my_prods <- c("EPD0", # distillate fuel oil (all), 
              "EPDXL0", # Distillate Fuel Oil, 0 to 15 ppm Sulfur
              "EPM0F", # Total finished gasoline (realistically all conventional)
              "EPOBG", # Total mgbc (realistically all conventional)
              "EPJK", # Jet fuel 
              "EPC0") #    Crude Oil) 

my_regions <- c('R1X', #   PADD 1A
                'R1Y', # PADD 1B 
                'R1Z', # PADD 1C
                'R20', #   PADD 2
                'R30', #   PADD 3
                'R40', #   PADD 5
                'R50') #   PADD 5

# EIA api has a 5000 row limit. make sure a single query doesn't exceed that
df_sub <- eia2(base,
               frequency = "monthly",
                facets = list(
              #    duoarea = my_regions[1],
                  product = my_prods),
               data_cols = c("value"),
               start = "2019-01",
               end = "2019-02")


nrow(df_sub)               

# create a list of dates of the format Year-Month by month from 2000 to 2020. use 
dates <- seq(as.Date("2000-01-01"), as.Date("2023-06-01"), by = "month")
dates <- format(dates, "%Y-%m")
 
# loop through the list dates by and bind together  
combined <- NA

# for (i in 1:5) {  
for (i in 1:(length(dates)-1)) {
  print(i)
  td <- df_sub <- eia2(base,
                       frequency = "monthly",
                       facets = list(product = my_prods),
                       data_cols = c("value"),
                       start = dates[i],
                       end = dates[i+1])
  if (nrow(td) == 5000) {
    print("HIT API LIMIT!")
  }

  if (length(combined) == 1) {
      combined <- td
    } else {
      combined <- bind_rows(combined, td)
    }
}

# saveRDS(combined, file.path(outdir, 'EIAPaddExportsDestination.RDS'))

# Movements by mode --------------------------------------------------

## Tanker + barge
base <- "petroleum/move/tb/"
eia2(base)

## get a list of valid facets
duolist <- eia2_facet(base,"duoarea")
prodlist <- eia2_facet(base,"product")

prodlist

## just getting the shipments from padd 3 to 1 and subpadds here. could also just loop through the entire duolist 
my_regions <- c('R10-R30','R1X-R30','R1Y-R30','R1Z-R30') 

# EIA api has a 5000 row limit. make sure a single query doesn't exceed that
df_sub <- eia2(base,
               frequency = "monthly",
                facets = list(
                  duoarea = "R10-R30",
                  product = my_prods),
               data_cols = c("value"),
               start = "2010-01",
               end = "2023-02")

nrow(df_sub)  


# loop through the list dates by and bind together  
combined <- NA

for (i in 1:(length(my_regions))) {
  print(i)
  td <- df_sub <- eia2(base,
                       frequency = "monthly", 
                       facets = list(
                        # product = my_prods, 
                       duoarea = my_regions[i]),
                       data_cols = c("value"),
                    start = "2010-01",
                    end = "2023-06")
  if (nrow(td) == 5000) {
    print("HIT API LIMIT!")
  }

  if (length(combined) == 1) {
      combined <- td
    } else {
      combined <- bind_rows(combined, td)
    }
}

# saveRDS(combined, file.path(outdir, 'EIAMovementsTankerBarge.RDS'))
