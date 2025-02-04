# price_figures.R

# BASIC JONES ACT SETUP ------------------------------------------------
rm(list=ls())
library(here)
root <- here()
source(file.path(root, "code", "basic_setup.R"))

# PREP DATA ---------------------------------------------------------------

clean_prices_bbg <- readRDS(file.path(dropbox,"intdata",
                 'clean_prices_bbg.RDS')) 
clean_prices_bbg_la <- readRDS(file.path(dropbox,"intdata",
                                      'clean_prices_bbg_la.RDS')) 

## ARGUS predictions ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
indata <- read_csv(file.path(dropbox, 
                     'intdata','Argus','PredictedFreightRates.csv'),
                  col_types = cols() )

argus <- indata %>%
  mutate(date = my(Date)) %>%
  mutate(Year = year(date), Month = month(date) ) %>%
  group_by(DC, Destination,Year,Month) %>% 
    summarise(shipcost_argus = mean(PredRate, na.rm = TRUE)) %>% ungroup() 

# Make analysis dataset
diffs_df_loc <- clean_prices_bbg %>%
  select(-bbg_index,-bbg_name) %>%
  pivot_wider(names_from = location, 
            values_from = price_bbl, 
            names_prefix = "price_") %>%
  mutate(DC = if_else(prod_category == "Crude", "D", "C"),
         price_diff = price_NY - price_GC) %>%
  left_join(argus %>% filter(Destination == "NYC")) %>%
  mutate(prod_category = if_else(prod_category == "Conventional",
                                 "Conv. Gasoline", prod_category),
         days = days_in_month(ymdate))

# Analysis dataset for LA
diffs_df_loc_la <- clean_prices_bbg_la %>%
  select(-bbg_index,-bbg_name) %>%
  pivot_wider(names_from = location, 
              values_from = price_bbl, 
              names_prefix = "price_") %>%
  mutate(DC = if_else(prod_category == "Crude", "D", "C"),
         price_diff = price_LA - price_GC) %>%
  left_join(argus %>% filter(Destination == "LA")) %>%
  mutate(prod_category = if_else(prod_category == "Conventional",
                                 "Conv. Gasoline", prod_category),
         days = days_in_month(ymdate))


## MAKE FIGURES -------------------------------------------------------------

## PRICE DIFFS AND ARGUS RATES TOGETHER
(p_pricediffs_arugsrates <- diffs_df_loc %>%
    filter(Year>=2018 & Year <=2019) %>%
    filter(prod_category != "RFG") %>%
    ggplot(.) +
    geom_line( aes(x = ymdate, y = price_diff), linewidth = 1.5 )+ 
    geom_line( aes(x = ymdate, y = shipcost_argus), color = "red", 
               linetype = "dashed", linewidth = 1.25) + 
    facet_wrap(~factor(prod_category, levels=c("Conv. Gasoline",
                                               "Jet Fuel", "ULSD", "Crude"))) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = 'black'),
          axis.line = element_line(color = 'black'),
          axis.title.x = element_blank(),
#          plot.caption = element_text(size = 8),  ,
          text = element_text(size=18),
          panel.spacing.x = unit(4, "mm") ) +
# need to specify the axis because the right most tick lables overlap with facet 
          scale_x_date(breaks = seq(as.Date("01/01/2018", '%m/%d/%Y'),
                                    as.Date("12/10/2019", '%m/%d/%Y'),
                                    by = "6 months"),
                        date_labels =  "%b %Y") +
          ylab('USGC to NYC Price difference ($/bbl)')  )

ggsave(file.path(root,"output","figures", "pricediffs_arugsrates.png"))


# Save each panel separately for paper, with TNR font
diffs_df_loc_long <- diffs_df_loc %>%
  select(Year,Month,ymdate,prod_category,price_diff,shipcost_argus) %>%
  filter(Year>=2018 & Year <=2019) %>%
  pivot_longer(cols=c(price_diff,shipcost_argus),
               names_to = "pricearg",
               values_to = "USDperbbl") %>%
  mutate(pricearg = if_else(pricearg == "price_diff",
               "Price diff", "Shipping cost"))
get_plot_spread <- function(
    fcat = "Crude",
    flab = fcat){
  p <- diffs_df_loc_long %>%
    filter(prod_category == fcat) %>%
    ggplot(.) +
    geom_line( aes(x = ymdate, y = USDperbbl, color = factor(pricearg),
                   size = factor(pricearg), linetype = factor(pricearg))) + 
    geom_line( aes(x = ymdate, y = USDperbbl, color = factor(pricearg),
                   size = factor(pricearg), linetype = factor(pricearg))) + 
    ylim(-1,6) +
    scale_color_manual(values = c("black","red")) +
    scale_size_manual(values = c(2,1.5)) +
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = 'black'),
          axis.line = element_line(color = 'black'),
          axis.title.x = element_blank(),
          #          plot.caption = element_text(size = 8),  ,
          text = element_text(size=27,family = 'Times New Roman'),
          panel.spacing.x = unit(4, "mm"),
          legend.position = c(0.2,0.82), legend.title = element_blank(),
          legend.key = element_blank()) +
    labs( y = '$/bbl') 
  
  return(p)
}
p_crude <- get_plot_spread(fcat = "Crude")
ggsave(file.path(root,"output","figures", "pricespread_argus_crude.png"))
p_jet <- get_plot_spread(fcat = "Jet Fuel")
ggsave(file.path(root,"output","figures", "pricespread_argus_jet.png"))
p_gas <- get_plot_spread(fcat = "Conv. Gasoline")
ggsave(file.path(root,"output","figures", "pricespread_argus_conv.png"))
p_ulsd <- get_plot_spread(fcat = "ULSD")
ggsave(file.path(root,"output","figures", "pricespread_argus_ULSD.png"))




# LONG PANEL OF PRICE DIFFS FOR APPX ------------------------------------------
# Use separate scales because the diffs get huge for jet fuel in 2022
get_plot_spread_long <- function(
    fcat = "Crude",
    flab = fcat,
    minyear = 2013,
    maxyear = 2022){
  p <- diffs_df_loc %>%
    filter(Year>=2013 & Year <=2022) %>%
    filter(prod_category == fcat) %>%
    ggplot(.) +
    geom_line( aes(x = ymdate, y = price_diff), na.rm = TRUE, linewidth = 1.5 ) + 
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = 'black'),
          axis.line = element_line(color = 'black'),
          axis.title.x = element_blank(),
          #          plot.caption = element_text(size = 8),  ,
          text = element_text(size=27,family = 'Times New Roman'),
          panel.spacing.x = unit(4, "mm") ) +
    labs( y = 'Price Spread ($/bbl)') 
  
  return(p)
}
p_crude <- get_plot_spread_long(fcat = "Crude")
ggsave(file.path(root,"output","figures", "pricespread_long_crude.png"))
p_jet <- get_plot_spread_long(fcat = "Jet Fuel")
ggsave(file.path(root,"output","figures", "pricespread_long_jet.png"))
p_gas <- get_plot_spread_long(fcat = "Conv. Gasoline")
ggsave(file.path(root,"output","figures", "pricespread_long_conv.png"))
p_ulsd <- get_plot_spread_long(fcat = "ULSD")
ggsave(file.path(root,"output","figures", "pricespread_long_ULSD.png"))

# Faceted version with same scale for each panel. Not saved.
(p <- diffs_df_loc %>%
    filter(Year>=2013 & Year <=2022) %>%
    # group_by(prod_category,Year) %>%
    #   summarize(price_diff = mean(price_diff)) %>% ungroup() %>%
    filter(prod_category != "RFG") %>%
    ggplot(.) +
    geom_line( aes(x = ymdate, y = price_diff), linewidth = 1.5 )+ 
    facet_wrap(~factor(prod_category, levels=c("Conv. Gasoline",
                                               "Jet Fuel", "ULSD", "Crude"))) )
                                             


# COMPARE EIA AND BBG FOR APPENDIX ------------------------------------------

spot_prices_eia <- readRDS(file.path(dropbox,"intdata",
                 'clean_eia_spot_prices.RDS'))

combined <- 
    spot_prices_eia %>% 
    mutate(prod_category = case_when(
        productName == "UK Brent Crude Oil" ~ "Crude",
        productName == "WTI Crude Oil" ~ "Crude",
        productName == "Conventional Regular Gasoline" ~ "Conventional",
        productName == "No 2 Diesel Low Sulfur (0-15 ppm)" ~ "ULSD",
        productName == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
        TRUE ~ "Other")) %>%
    filter(prod_category != "Other") %>%
    rename(price_bbl = price_bbl_eia) %>%
    mutate(source = "EIA") %>%
    select(!productName) %>%
    bind_rows(clean_prices_bbg %>% 
                  mutate(source = "Bloomberg") %>% 
                  select(-bbg_index, -bbg_name)) %>%
    filter(prod_category != "RFG") %>% 
    mutate(prod_category = if_else(prod_category == "Conventional",
                                 "Conv. Gasoline", prod_category))

( p_level <- combined %>%
    filter(Year >= 2013 & Year <=2022) %>%
    mutate(plot_group = paste(prod_category,location, sep = " - ")) %>% ggplot() + geom_line(aes(x = ymdate, y = price_bbl, color = source)) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          text = element_text(size = 16)) + 
          labs(y = "Levels ($/bbl)", x = "") + 
    facet_wrap(~plot_group, ncol = 2) )

##^ ok the above is pretty useless. they are right on top of eachother 


df_wide_source <- combined %>% 
    pivot_wider(names_from = source, values_from = price_bbl) %>%
    mutate(price_diff = EIA - Bloomberg, 
           price_diff_pct = (EIA - Bloomberg)/Bloomberg) 

( pricediffs_eia_long <- df_wide_source %>%
    filter(Year >= 2013 & Year <=2022) %>%
    filter(!is.na(price_diff)) %>%
#    filter(prod_category != "Jet Fuel") %>%
    mutate(plot_group = paste(prod_category,location, sep = " - ")) %>% 
    ggplot() + geom_line(aes(x = ymdate, y = price_diff)) +
    theme_bw() + 
    theme(legend.position = c(.5,0.85),
    legend.title = element_blank(), 
    text = element_text(size = 16)) + 
    labs(y = "EIA Price - BBG Price ($/bbl)", x = "") + 
    facet_wrap(~factor(plot_group, 
                 levels=c("Conv. Gasoline - NY", "Conv. Gasoline - GC",
                          "ULSD - NY", "ULSD - GC",
                          "Crude - NY", "Crude - GC",
                          "Jet Fuel - GC") ), ncol = 2)  +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = 'black'),
          axis.line = element_line(color = 'black'),
          axis.title.x = element_blank(),
#          plot.caption = element_text(size = 8),  ,
          text = element_text(size=18),
          panel.spacing.x = unit(4, "mm") ) ) 

ggsave(file.path(root,"output","figures", "pricediffs_eia_long.png"))


df_wide_source_loc <- combined %>% 
    mutate(source_loc = paste(source, location, sep = "_")) %>%
    select(-source, -location) %>%
    pivot_wider(names_from = source_loc, values_from = price_bbl) %>%
    mutate(price_diff_bbg = Bloomberg_NY - Bloomberg_GC,
           price_diff_eia = EIA_NY - EIA_GC,
           diffs_source = price_diff_eia - price_diff_bbg) 

pricediffs_spread_eia <- df_wide_source_loc %>%
    filter(Year >= 2013 & Year <=2022) %>%
    filter(prod_category != "Jet Fuel") %>%
    filter(prod_category != "Crude") %>%
    ggplot() + geom_line(aes(x = ymdate, y = diffs_source)) +
    labs(y = "EIA Spread - BBG Spread ($/bbl)", x = "") + 
    facet_wrap(~factor(prod_category, 
                 levels=c("Conv. Gasoline", "ULSD","Crude") ), ncol = 1)  +
    theme(text = element_text(size=20,family = 'Times New Roman'),
          legend.position = c(.5,0.85),
    	  legend.title = element_blank(), 
    	  plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = 'black'),
          axis.line = element_line(color = 'black'),
          axis.title.x = element_blank(),
          panel.spacing.x = unit(4, "mm") ) 

ggsave(file.path(root,"output","figures", "pricediffs_spread_eia.png"))



## PRICE DIFFS VS ARGUS RATES TO LA--------------------------------------------
(p_pricediffs_arugsrates_la <- diffs_df_loc_la %>%
   filter(Year>=2018 & Year <=2019) %>%
   ggplot(.) +
   geom_line( aes(x = ymdate, y = price_diff), linewidth = 1.5 )+ 
   geom_line( aes(x = ymdate, y = shipcost_argus), color = "red", 
              linetype = "dashed", linewidth = 1.25) + 
   facet_wrap(~factor(prod_category, levels=c("Conv. Gasoline",
                                              "Jet Fuel", "ULSD"))) +
   theme(plot.title = element_text(hjust = 0.5),
         panel.background = element_rect(fill = 'white'),
         panel.border = element_blank(),
         panel.grid.major.y = element_line(size = .05, color = 'black'),
         axis.line = element_line(color = 'black'),
         axis.title.x = element_blank(),
         #          plot.caption = element_text(size = 8),  ,
         text = element_text(size=18),
         panel.spacing.x = unit(4, "mm") ) +
   # need to specify the axis because the right most tick lables overlap with facet 
   scale_x_date(breaks = seq(as.Date("01/01/2018", '%m/%d/%Y'),
                             as.Date("12/10/2019", '%m/%d/%Y'),
                             by = "6 months"),
                date_labels =  "%b %Y") +
   ylab('USGC to LA price difference ($/bbl)')  )



## SINGLE NUMBER TEX FILES FOR THE PAPER -------------------------------
texfolder <- file.path(root,"output","tex_numbers")
# Average price diff minus Argus rate for each product
diffs_df_loc_sum <- diffs_df_loc %>%
  filter(Year>=2018 & Year <=2019) %>%
  filter(prod_category != "RFG") %>%
  select(prod_category, price_diff, shipcost_argus) %>%
  mutate(price_argus_diff = price_diff - shipcost_argus) %>%
  group_by(prod_category) %>% 
  summarize_all(list(mean))
tex_pricediffminusArgus_conv <- file.path(texfolder,"pricediffminusArgus_conv.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Conv. Gasoline") %>% 
      select(price_argus_diff) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_pricediffminusArgus_conv)
tex_pricediffminusArgus_jet <- file.path(texfolder,"pricediffminusArgus_jet.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Jet Fuel") %>% 
      select(price_argus_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediffminusArgus_jet)
tex_pricediffminusArgus_ULSD <- file.path(texfolder,"pricediffminusArgus_ULSD.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="ULSD") %>% 
      select(price_argus_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediffminusArgus_ULSD)
tex_pricediffminusArgus_crude <- file.path(texfolder,"pricediffminusArgus_crude.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Crude") %>% 
      select(price_argus_diff) %>% mutate(price_argus_diff = price_argus_diff * -1) %>%
      pull() %>% formatC(digits=2,format="f"), file=tex_pricediffminusArgus_crude)

# Average price diff for each product
tex_pricediff_conv <- file.path(texfolder,"pricediff_conv.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Conv. Gasoline") %>% 
      select(price_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediff_conv)
tex_pricediff_jet <- file.path(texfolder,"pricediff_jet.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Jet Fuel") %>% 
      select(price_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediff_jet)
tex_pricediff_ULSD <- file.path(texfolder,"pricediff_ULSD.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="ULSD") %>% 
      select(price_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediff_ULSD)
tex_pricediff_crude <- file.path(texfolder,"pricediff_crude.tex")
cat(diffs_df_loc_sum %>% filter(prod_category=="Crude") %>% 
      select(price_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediff_crude)

# Average clean counterfactual shipping costs to Pt Canaveral and NYC
argus_sum <- argus %>%
  filter(DC=="C") %>% filter(Destination=="CNV" | Destination=="NYC") %>%
  select(Destination,shipcost_argus) %>%
  group_by(Destination) %>%
  summarize_all(list(mean))
tex_argus_avg_CNV <- file.path(texfolder,"argus_avg_CNV.tex")
cat(argus_sum %>% filter(Destination=="CNV") %>% 
      select(shipcost_argus) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_argus_avg_CNV)
tex_argus_avg_NYC <- file.path(texfolder,"argus_avg_NYC.tex")
cat(argus_sum %>% filter(Destination=="NYC") %>% 
      select(shipcost_argus) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_argus_avg_NYC)

# Average price diff minus Argus rate for jet fuel to USWC
diffs_df_loc_sum_la <- diffs_df_loc_la %>%
  filter(Year>=2018 & Year <=2019) %>%
  filter(prod_category=="Jet Fuel") %>%
  select(prod_category, price_diff, shipcost_argus) %>%
  mutate(price_argus_diff = price_diff - shipcost_argus) %>%
  group_by(prod_category) %>% 
  summarize_all(list(mean))
tex_pricediff_jet_la <- file.path(texfolder,"pricediff_jet_la.tex")
cat(diffs_df_loc_sum_la %>%
      select(price_diff) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_pricediff_jet_la)
tex_shipcost_la <- file.path(texfolder,"shipcost_la.tex")
cat(diffs_df_loc_sum_la %>%
      select(shipcost_argus) %>% pull() %>%
      formatC(digits=2,format="f"), file=tex_shipcost_la)