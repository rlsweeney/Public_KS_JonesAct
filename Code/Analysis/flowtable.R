# flowtable.R
# Creates table of average annual imports, exports, and movements, by subpadd

# BASIC JONES ACT SETUP ------------------------------------------------
rm(list=ls())
library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))

# PREP DATA ---------------------------------------------------------------

combined_subpadd_volumes <- readRDS(file.path(dropbox,"intdata",
                                              'combined_subpadd_volumes.RDS'))

main_padd3_df <- readRDS(file.path(dropbox,"intdata",
                 'main_padd3_df.RDS'))

padd5_imp <- readRDS(file.path(dropbox,"intdata",
                               'padd5_imp.RDS'))


# UTILITY FUNCTION TO ANNUALIZE DATA --------------------------------------
conv_to_annual <- function(x) (x/2)


# SUMMARIZE USEC DATA -----------------------------------------------------
# First summarize and get PADDs and flow type as rows, products as columns
sum_usec <- combined_subpadd_volumes %>%
  filter(Year >= 2018, Year <= 2019) %>%
  select(PADD,prod_category,Consumption,Movements,Imports) %>%
  group_by(PADD,prod_category) %>%
  summarize_all(list(sum)) %>%
  ungroup() %>%
  mutate_at(c("Consumption","Movements","Imports"), conv_to_annual) %>%
  pivot_longer(cols = Consumption:Imports, names_to = "flowtype", 
               values_to = "Q") %>%
  mutate(Q = round(Q)) %>%
  mutate(prod_category = if_else(prod_category=="Conventional", "Gasoline", 
              prod_category )) %>%
  pivot_wider(names_from = prod_category, values_from = Q) %>%
  mutate(Crude = if_else(is.na(Crude), 0, Crude)) 

# Make three sub-tables, one for each sub-padd
sum_1a <- sum_usec %>%
  filter(PADD=="1A") %>%
  select("flowtype","Gasoline","Jet Fuel","ULSD","Crude") %>%
  filter(flowtype!="Movements")
sum_1b <- sum_usec %>%
  filter(PADD=="1B") %>%
  select("flowtype","Gasoline","Jet Fuel","ULSD","Crude") %>%
  filter(flowtype!="Movements")
sum_1c <- sum_usec %>%
  filter(PADD=="1C") %>%
  select("flowtype","Gasoline","Jet Fuel","ULSD","Crude") %>%
  mutate(flowtype = if_else(flowtype=="Movements", "Movements from PADD 3", 
                            flowtype )) %>%
  arrange(flowtype)


# SUMMARIZE USGC DATA -----------------------------------------------------
sum_3 <- main_padd3_df %>%
  filter(Year >= 2018, Year <= 2019) %>%
  select(prod_category,Consumption,Exports_NonCentAm) %>%
  mutate(Consumption = if_else(is.na(Consumption), 0, Consumption)) %>%
  group_by(prod_category) %>%
  summarize_all(list(sum)) %>%  
  ungroup() %>%
  mutate_at(c("Consumption","Exports_NonCentAm"), conv_to_annual) %>%
  pivot_longer(cols = Consumption:Exports_NonCentAm, names_to = "flowtype", 
               values_to = "Q") %>%
  mutate(Q = round(Q)) %>%
  mutate(prod_category = if_else(prod_category=="Conventional", "Gasoline", 
                                 prod_category )) %>%
  pivot_wider(names_from = prod_category, values_from = Q) %>%
  mutate(flowtype = if_else(flowtype=="Exports_NonCentAm", "Exports", flowtype)) %>%
  select("flowtype","Gasoline","Jet Fuel","ULSD","Crude")


# ASSEMBLE TABLE -------------------------------------------------------------
sum_combined <- rbind(sum_1a,sum_1b,sum_1c,sum_3)

# Create the LaTeX table
tt <- xtable(sum_combined, digits = c(0,0,0,0,0,0))

tt

# Add the panel labels
latex_table <- print(tt, add.to.row = list(pos = list(c(0),c(2),c(4),c(7)), 
            command = c("\\midrule & Conventional & &  & \\\\\n
                         & Gasoline & Jet Fuel & ULSD & Crude \\\\
                        \\midrule \\multicolumn{5}{l}{\\textbf{New England (PADD 1a)}} \\\\\n",
                        "\\midrule \\multicolumn{5}{l}{\\textbf{Central Atlantic (PADD 1b)}} \\\\\n",
                        "\\midrule \\multicolumn{5}{l}{\\textbf{Lower Atlantic (PADD 1c)}} \\\\\n",
                        "\\midrule \\multicolumn{5}{l}{\\textbf{Gulf Coast (PADD 3)}} \\\\\n")),
            include.rownames = FALSE, include.colnames = FALSE,
            hline.after = c(9), caption.placement = "top", floating = FALSE)
            
latex_table

# Print the LaTeX table to a file
file_path <- file.path(root, "output", "tables", "flows_summary.tex")
cat(latex_table, file = file_path)


# SUMMARIZE PADD 5 DATA -------------------------
padd5_imp_sum <- padd5_imp %>%
  filter(Year==2018 | Year==2019) %>%
  select(prod_category,Imports,Year) %>%
  group_by(prod_category,Year) %>%
  summarize_all(list(sum)) %>% ungroup() %>%
  select(prod_category,Imports) %>%
  group_by(prod_category) %>%
  summarize_all(list(mean))



# EXPORT SOME SINGLE NUMBER TEX FILES WITH FLOW INFO --------------------------
texfolder <- file.path(root,"output","tex_numbers")
# Padd 1 imports by fuel (for map figure in intro). Sum across sub-padds
sum_1_imp <- rbind(sum_1a,sum_1b,sum_1c) %>%
  filter(flowtype=="Imports") %>%
  select(!flowtype) %>%
  summarize_all(list(sum)) %>%
  mutate(tot = Gasoline + get("Jet Fuel") + ULSD + Crude)
# Total padd 1 imports summed across products
tex_totalECimports <- file.path(texfolder,"totalECimports.tex")
cat(sum_1_imp %>% select(tot) %>% pull(), file=tex_totalECimports)
# Total padd 3 exports
sum_3_exp <- sum_3 %>%
  filter(flowtype=="Exports") %>%
  mutate(tot = Gasoline + get("Jet Fuel") + ULSD + Crude)
tex_totalGCexports <- file.path(texfolder,"totalGCexports.tex")
cat(sum_3_exp %>% select(tot) %>% pull(), file=tex_totalGCexports)

# Padd 5 imports by fuel
tex_padd5_imp_conv <- file.path(texfolder,"padd5_imp_conv.tex")
  cat(padd5_imp_sum %>% filter(prod_category=="Conventional") %>%
  select(Imports) %>% pull() %>% 
  formatC(digits=0,format="f"), file=tex_padd5_imp_conv)
tex_padd5_imp_jet <- file.path(texfolder,"padd5_imp_jet.tex")
  cat(padd5_imp_sum %>% filter(prod_category=="Jet Fuel") %>%
        select(Imports) %>% pull() %>% 
        formatC(digits=0,format="f"), file=tex_padd5_imp_jet)
tex_padd5_imp_ULSD <- file.path(texfolder,"padd5_imp_ULSD.tex")
  cat(padd5_imp_sum %>% filter(prod_category=="ULSD") %>%
        select(Imports) %>% pull() %>% 
        formatC(digits=0,format="f"), file=tex_padd5_imp_ULSD)
tex_padd5_imp_crude <- file.path(texfolder,"padd5_imp_crude.tex")
  cat(padd5_imp_sum %>% filter(prod_category=="Crude") %>%
        select(Imports) %>% pull() %>% 
        formatC(digits=0,format="f"), file=tex_padd5_imp_crude)
