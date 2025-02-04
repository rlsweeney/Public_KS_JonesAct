# counterfactuals.R

# BASIC JONES ACT SETUP ------------------------------------------------
rm(list=ls())
library(here)
root <- here()

source(file.path(root, "code", "basic_setup.R"))

# PREP DATA ---------------------------------------------------------------
clean_prices_bbg <- readRDS(file.path(dropbox,"intdata",
                 'clean_prices_bbg.RDS'))  

price_df <- clean_prices_bbg %>%
  select(-bbg_index,-bbg_name) %>%
  pivot_wider(names_from = location, 
            values_from = price_bbl, 
            names_prefix = "price_") %>%
  mutate(price_diff = price_NY - price_GC) 

# ARGUS predictions 
indata <- read_csv(file.path(dropbox, 
                     'intdata','Argus','PredictedFreightRates.csv'),
                  col_types = cols() )

argus <- indata %>%
  mutate(date = my(Date)) %>%
  mutate(Year = year(date), Month = month(date) ) %>%
  group_by(DC, Destination,Year,Month) %>% 
  summarise(shipcost_argus = mean(PredRate, na.rm = TRUE),
            shipcost_argus_LD = mean(PredRate_LD, na.rm = TRUE)) %>% 
  ungroup()

combined_subpadd_volumes <- readRDS(file.path(dropbox,"intdata",
                 'combined_subpadd_volumes.RDS'))

## padd 3 volumes computed in clean_main_data.R ----------------------------

main_padd3_df <- readRDS(file.path(dropbox,"intdata",
                 'main_padd3_df.RDS'))  

# PERFORM MAIN COUNTERFACTUAL CALCULATIONS ----------------------------------

get_main_cf_dfs <- function(PADD1a_port = "BOS",
                            PADD1b_port = "NYC",
                            PADD1c_port = "CNV",
                            distspec = "L") {

    # Zero small movements of crude oil, and of products to 1a and 1b
    main_subpadd_df <- combined_subpadd_volumes %>%
        filter(Year >= 2018, Year <= 2019) %>%
        left_join(price_df) %>%
        mutate(DC = if_else(prod_category == "Crude", "D", "C"),
                Destination = case_when( 
                                PADD == "1A" ~ PADD1a_port,
                                PADD == "1B" ~ PADD1b_port,
                                PADD == "1C" ~ PADD1c_port)) %>%
        mutate(Movements = if_else(prod_category=="Crude",0,Movements)) %>%
        mutate(Movements = if_else(PADD=="1C",Movements,0)) %>%
        left_join(argus) %>% mutate(ds = distspec) %>%
        mutate(shipcost_argus = if_else(ds == "L", shipcost_argus, 
                                        shipcost_argus_LD)) %>%
        select(-shipcost_argus_LD, -ds)
          

    # ## Algorithm --------------------------------------------------------------- 
    # - assume shipments go to closest subpadd first (sort 1c, 1b, 1a)
    # - compute the cumulative imports by product-date 
    #     - for 1c its just 1c, 1b its 1c + 1b, etc.
    # - compute cumulative imports of OTHER subpadds 
    #     - for 1c its 0, 1b  its 1c, 1a its 1c + 1b
    #     - called this `used_imports`
    # - algorithim for computing the gc exports rerouted to a subpadd (`cf_gc_reroute`)
    #     - if price ny - price gc < shipcost, then no shipments 
    #     - else if cumulative imports would not exhaust exports, then all imports get converted to gc shipments 
    #     - else if used imports < exports < cum imports, imports are reduced, but not zeroed out 
    #     - else 0 

    reroute_df <- main_subpadd_df %>%
        mutate(jones_sort = case_when(PADD == "1C" ~ 1,
                PADD == "1B" ~ 2, PADD == "1A" ~ 3)) %>%
        left_join(main_padd3_df %>% 
                select(Year,Month,prod_category,Exports_NonCentAm, Exports_All)) %>%
        arrange(prod_category,Year,Month,jones_sort)     %>%
        group_by(Year,Month,prod_category) %>%
            mutate(cum_imports = cumsum(Imports)) %>% ungroup() %>%
        mutate(used_imports = cum_imports - Imports, 
            cf_gc_reroute = case_when(
                    price_diff  < shipcost_argus ~ 0,  
                    cum_imports < Exports_NonCentAm ~ Imports,
                    used_imports < Exports_NonCentAm ~ 
                        Exports_NonCentAm - used_imports,
                    TRUE ~ 0),
            cf_imports = Imports - cf_gc_reroute, 
            cf_movements = Movements + cf_gc_reroute)
           
    # Compute counterfactual volumes aggregated to padd 1 level for graphs 
    padd1_cf_volumes <- reroute_df  %>%
        select(-PADD) %>%
        select(Year,Month,ymdate,prod_category,Imports,cf_gc_reroute,
            Movements, cf_imports, cf_movements) %>%
        group_by(ymdate,prod_category,Year,Month) %>%
        summarize_all(sum) %>% ungroup() %>%
        left_join(main_padd3_df %>% 
                    select(Year,Month,prod_category,Exports = Exports_NonCentAm)) %>%
        mutate(cf_exports = Exports - cf_gc_reroute)  

    # Compute counterfactual prices, then efficiency and cs changes -----------
    # - first need to figure out if gc exports are exhausted 
    # - if yes, then need to figure out how far up the east cost they rerouted exports go
    #     - counterfactual price is NY price - shipcost to furthest point gc shipments go to
    # - if gc movements are on the margin (no imports), price = gc price + shipcost 
    # - else price = ny price

    ## GC price *increase* 
    # - find farthest poing gc shipments go to 
    # - find if exports are zero 
    # - if yes, price is nyh - shipcost to furthest point gc shipments go to 

    ec_ship_point <- reroute_df  %>%
        filter(cf_movements > 0) %>%
        arrange(prod_category,ymdate,-shipcost_argus) %>%
        group_by(prod_category,ymdate) %>%
            slice(1) %>% ungroup() %>%
        select(prod_category,ship_padd = PADD,ymdate,shipcost_argus) 

    padd3_cf <- padd1_cf_volumes %>%
        select(ymdate,prod_category,cf_exports) %>%
        left_join(ec_ship_point) %>%
        left_join(main_padd3_df) %>%
        mutate(cf_price = case_when(
                        is.na(ship_padd) ~ price_GC, 
                        cf_exports == 0 ~ price_NY - shipcost_argus, 
                        TRUE ~ price_GC),
            cf_price_change = cf_price - price_GC, 
            cf_cs_change = -cf_price_change*Consumption) 

    subpadd_cf_df <- reroute_df %>%
        left_join(padd3_cf %>% 
                    select(Year,Month,prod_category,price_GC,price_GC_cf = cf_price)) %>%
        mutate(cf_price = if_else(cf_imports == 0 & cf_movements > 0,
                                price_GC_cf + shipcost_argus, price_NY), 
            cf_price_diff = cf_price - price_GC_cf,
            cf_price_change = cf_price - price_NY,
            efficiency_gain = (-cf_price_change+price_GC_cf-price_GC)*cf_movements, 
            cs_change = -cf_price_change* Consumption) 

    return(list(main_subpadd_df=main_subpadd_df,
                subpadd_cf_df=subpadd_cf_df,
                padd1_cf_volumes=padd1_cf_volumes,
                padd3_cf=padd3_cf,
                reroute_df=reroute_df))
}

main_results <- get_main_cf_dfs()

# COMPARE PADD 1 IMPORTS, SHIPMENTS AND EXPORTS -----------------------------------

plot_df <- main_results$padd1_cf_volumes  %>%
    select(ymdate,prod_category,Imports,cf_imports,Movements,cf_movements, 
           Exports,cf_exports) %>%
    pivot_longer(cols = c(Imports,cf_imports,Movements,cf_movements, 
           Exports,cf_exports), names_to = "type", values_to = "value") %>%
    mutate(series = case_when(type %in% c("Imports", "cf_imports") ~ "Imports",
                              type %in% c("Movements", "cf_movements") ~ "Movements",
                              type %in% c("Exports", "cf_exports") ~ "Exports"),
           scenario = if_else(str_starts(type, "cf_"), "counterfactual", "baseline"))          

prod_levels <- c("Conventional","Jet Fuel", "ULSD", "Crude")
prod_labels <- c('Conv. Gasoline','Jet Fuel', 'ULSD', 'Crude')

( padd1_volumes_baseline <- plot_df %>%
    filter(scenario == "baseline") %>%
    ggplot(aes(x = ymdate, y = value, color = series)) + 
    geom_line() +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          text = element_text(size = 16)) + 
    labs(y = "Million Barrels", x = "", title = "Baseline") + 
    facet_wrap(~factor(prod_category,     
        levels = prod_levels, labels = prod_labels)) + 
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
                        date_labels =  "%b %Y") )


( padd1_volumes_counterfactual <- plot_df %>%
    filter(scenario == "counterfactual") %>%
    ggplot(aes(x = ymdate, y = value, color = series)) + 
    geom_line() +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          text = element_text(size = 16)) + 
    labs(y = "Million Barrels", x = "", title = "Counterfactual") + 
    facet_wrap(~factor(prod_category,     
        levels = prod_levels, labels = prod_labels)) + 
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
                        date_labels =  "%b %Y") )

# Make a four row X 3 column plot, faceted by product category -----------------

get_series_p <- function(pcat, plab = pcat){

    p <- plot_df %>%
        filter(prod_category == pcat) %>%
        ggplot(aes(x = ymdate, y = value, color = series, linetype=scenario)) + 
        geom_line(size = 1.25) +
#        theme_bw() + 
        facet_wrap(~series) + 
        theme(legend.position = "none",
            text = element_text(size = 14)) + 
        labs(y = substitute("mmbbl/month"), x = "", title = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = 'white'),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(size = .05, color = 'black'),
            axis.line = element_line(color = 'black'),
            axis.title.x = element_blank(),
            strip.background = element_blank(),
            text = element_text(size=14, family = "Times New Roman"),
            panel.spacing.x = unit(4, "mm") ) +
    # need to specify the axis because the right most tick labels overlap with facet 
            scale_x_date(breaks = seq(as.Date("01/01/2018", '%m/%d/%Y'),
                                        as.Date("12/10/2019", '%m/%d/%Y'),
                                        by = "12 months"),
                        date_labels =  "%b %Y") 

    return(p)
}

p_cru <- get_series_p("Crude")
p_gas <- get_series_p("Conventional", "Conventional Gasoline")
p_ulsd <- get_series_p("ULSD")
p_jet <- get_series_p("Jet Fuel")

ggsave(plot=p_cru, dpi = 150, height = 1.5, width = 6.5, unit = 'in',
       filename =file.path(root,"output","figures", "padd1_counterfactual_crude.png"))
ggsave(plot=p_gas, dpi = 150, height = 1.5, width = 6.5, unit = 'in',
       filename =file.path(root,"output","figures", "padd1_counterfactual_gas.png"))
ggsave(plot=p_ulsd, dpi = 150, height = 1.5, width = 6.5, unit = 'in',
       filename =file.path(root,"output","figures", "padd1_counterfactual_ulsd.png"))
ggsave(plot=p_jet, dpi = 150, height = 1.5, width = 6.5, unit = 'in',
       filename =file.path(root,"output","figures", "padd1_counterfactual_jet.png"))

# g <- grid.arrange(p_gas, p_jet, p_ulsd,p_cru, ncol = 1)

# ggsave(plot=g, dpi = 150, height = 7.5, width = 6.5, unit = 'in',
#       filename =file.path(root,"output","figures", "padd1_counterfactual_12panel.png"))

# MAKE SOME PLOTS BY SUBPADD --------------------------------------------------

plot_df_subpadd <- main_results$subpadd_cf_df %>%
    select(ymdate,PADD,prod_category,Imports,cf_imports,
                Price = price_NY,cf_price) %>%
    pivot_longer(cols = c(Imports,cf_imports,Price,cf_price), 
                 names_to = "type", values_to = "value") %>%
    mutate(series = case_when(type %in% c("Imports", "cf_imports") ~ "Imports",
                              type %in% c("Price", "cf_price") ~ "Price"),
           scenario = if_else(str_starts(type, "cf_"), "counterfactual", "baseline"))          

pcat = "Conventional"
plap = "Conv. Gasoline"
ycat = "Imports"
ylab = ycat 
pylab = "Million Barrels"

get_p_sub_cf <- function(pcat,plab = pcat,
                     ycat, ylab = ycat, pylab = "Million Barrels"){
 
    p <- plot_df_subpadd %>%
        filter(prod_category == pcat) %>%
        filter(series == ycat) %>%
        ggplot(aes(x = ymdate, y = value, color = PADD, linetype=scenario)) + 
        geom_line(size = 1.25) +
        theme_bw() + 
        facet_wrap(~PADD) + 
        theme(legend.position = "none",text = element_text(size = 16))  + 
        labs(y = pylab, x = "", title = ylab) +
        theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = 'white'),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(size = .05, color = 'black'),
            axis.line = element_line(color = 'black'),
            axis.title.x = element_blank(),
            text = element_text(size=16),
            panel.spacing.x = unit(4, "mm") ) +
    # need to specify the axis because the right most tick lables overlap with facet 
            scale_x_date(breaks = seq(as.Date("01/01/2018", '%m/%d/%Y'),
                                        as.Date("12/10/2019", '%m/%d/%Y'),
                                        by = "6 months"),
                        date_labels =  "%b %Y") 
    return(p)
}

get_p_sub_diff <- function(pcat,plab=pcat,
                     ycat, ylab, pylab = "$/Barrel"){

    p <- main_results$subpadd_cf_df %>%
        mutate(yvar = {{ycat}}) %>%
        filter(prod_category == pcat) %>%
        ggplot(aes(x = ymdate, y = yvar, color = PADD)) + 
        geom_line(size = 1.25) +
        theme_bw() + 
        facet_wrap(~PADD) + 
        theme(legend.position = "none",text = element_text(size = 16))  + 
        labs(y = pylab, x = "", title = ylab) +
        theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = 'white'),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(size = .05, color = 'black'),
            axis.line = element_line(color = 'black'),
            axis.title.x = element_blank(),
            text = element_text(size=16),
            panel.spacing.x = unit(4, "mm") ) +
    # need to specify the axis because the right most tick lables overlap with facet 
            scale_x_date(breaks = seq(as.Date("01/01/2018", '%m/%d/%Y'),
                                        as.Date("12/10/2019", '%m/%d/%Y'),
                                        by = "6 months"),
                        date_labels =  "%b %Y") 
    return(p)
}

get_p_sub_combined <- function(fpcat,fplab = fpcat){

    p_imp <- get_p_sub_cf(pcat = fpcat, plab = fplab,
                     ycat = "Imports",
                     ylab = "Imports (observed vs counterfactual)",
                     pylab = "Million Barrels")


    p_price_diff <- get_p_sub_diff(pcat = fpcat, plab = fplab,
                     ycat = cf_price_change,
                     ylab = "Price Change",
                     pylab = "$/Barrel")

    
    p_cs_diff <- get_p_sub_diff(pcat = fpcat, plab = fplab,
                     ycat = cs_change,
                     ylab = "Consumer Surplus Change",
                     pylab = "Million $")

    # Combine the plots into a grid
    p_comb <- cowplot::plot_grid(p_imp, p_price_diff, p_cs_diff, ncol = 1)

    # Add a title to the grid
    p <- cowplot::ggdraw() +
        cowplot::draw_label(fpcat, fontface = "bold", x = 0.5, hjust = 0.5, 
                    vjust = 1, size = 20) + 
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines"))

    # Combine the title and the grid
    p <- cowplot::plot_grid(p, p_comb, ncol = 1, rel_heights = c(0.1, 0.90))

    return(p)
}

p_conv <- get_p_sub_combined("Conventional", "Conv. Gasoline")
p_jet <- get_p_sub_combined("Jet Fuel")
p_ulsd <- get_p_sub_combined("ULSD")
p_cru <- get_p_sub_combined("Crude")

p_conv
# ggsave(file.path(root,"output","figures", "subpadd_counterfactual_gas.png"))
p_jet 
# ggsave(file.path(root,"output","figures", "subpadd_counterfactual_jet.png"))
p_ulsd
# ggsave(file.path(root,"output","figures", "subpadd_counterfactual_ulsd.png"))

# MAKE COUNTERFACTUAL TABLE -------------------------------------------------

get_cf_tables<- function(results_df = main_results){

    # EFFICIENCY ---------------------------------------------------------
    panel_efficiency <- results_df$subpadd_cf_df  %>%
        group_by(prod_category,PADD) %>%
            summarize(efficiency_gain = sum(efficiency_gain)/2) %>%
            ungroup() %>%
        pivot_wider(names_from = prod_category, values_from = efficiency_gain) %>%
        select(PADD,Conventional,ULSD, `Jet Fuel`, Crude) %>%
        mutate(Region = case_when(
                PADD == "1A" ~ "New England (1A)",
                PADD == "1B" ~ "Central Atlantic (1B)",
                PADD == "1C" ~ "Lower Atlantic (1C)",
                PADD == "FL" ~ "Florida",
                PADD == "3" ~ "Gulf Coast (3)") ) %>%
        select(Region, everything()) %>% select(-PADD)
   
    # Price savings ------------------------------------------------------
    df_p_ec <- results_df$subpadd_cf_df  %>%
        group_by(prod_category,PADD) %>%
        summarize(price_change = mean(cf_price_change)) %>%
        pivot_wider(names_from = prod_category, values_from = price_change) %>%
        select(PADD,Conventional,ULSD, `Jet Fuel`, Crude)

    panel_price <- results_df$padd3_cf %>%
        group_by(prod_category) %>%
        summarize(price_change = mean(cf_price_change)) %>%
        pivot_wider(names_from = prod_category, values_from = price_change) %>%
        mutate(PADD = "3") %>%
        select(PADD,Conventional,ULSD, `Jet Fuel`, Crude) %>%
        bind_rows(df_p_ec) %>%
        arrange(PADD) %>%
        mutate(Region = case_when(
                PADD == "1A" ~ "New England (1A)",
                PADD == "1B" ~ "Central Atlantic (1B)",
                PADD == "1C" ~ "Lower Atlantic (1C)",
                PADD == "FL" ~ "Florida",
                PADD == "3" ~ "Gulf Coast (3)") ) %>%
        select(Region, everything()) %>% select(-PADD)

    # Consumer surplus ---------------------------------------------------------

    df_cs_ec <- results_df$subpadd_cf_df  %>%
        group_by(prod_category,PADD) %>%
        summarize(cs_change = sum(cs_change)/2) %>%
        pivot_wider(names_from = prod_category, values_from = cs_change) %>%
        select(PADD,Conventional,ULSD, `Jet Fuel`, Crude)

    panel_cs <- results_df$padd3_cf %>%
        group_by(prod_category) %>%
        summarize(cs_change = sum(cf_cs_change)/2) %>%
        pivot_wider(names_from = prod_category, values_from = cs_change) %>%
        mutate(PADD = "3") %>%
        select(PADD,Conventional,ULSD, `Jet Fuel`, Crude) %>%
        bind_rows(df_cs_ec) %>%
        arrange(PADD) %>%
        mutate(Region = case_when(
                PADD == "1A" ~ "New England (1A)",
                PADD == "1B" ~ "Central Atlantic (1B)",
                PADD == "1C" ~ "Lower Atlantic (1C)",
                PADD == "FL" ~ "Florida",
                PADD == "3" ~ "Gulf Coast (3)") ) %>%
        select(Region, everything()) %>% select(-PADD)

    # Summarize flows and flow changes --------------------------------------------
    conv_to_annual <- function(x) (x/2)
    
    sum_reroute <- results_df$reroute_df %>%
        select(prod_category,PADD,Imports,cf_gc_reroute,cf_imports) %>%
        group_by(prod_category,PADD) %>%
        summarize_all(list(sum)) %>% ungroup() %>%
        mutate_at(c("Imports","cf_gc_reroute","cf_imports"), conv_to_annual)
        
    sum_reroute1a <- sum_reroute %>%
        filter(PADD=="1A") %>%
        pivot_longer(cols = Imports:cf_imports, names_to = "flowtype", 
                    values_to = "Q") %>%
        pivot_wider(names_from = prod_category, values_from = Q) %>%
        mutate(Crude = 0)
    
    sum_reroute1b <- sum_reroute %>%
        filter(PADD=="1B") %>%
        pivot_longer(cols = Imports:cf_imports, names_to = "flowtype", 
                    values_to = "Q") %>%
        pivot_wider(names_from = prod_category, values_from = Q)
    
    sum_reroute1c <- sum_reroute %>%
        filter(PADD=="1C") %>%
        pivot_longer(cols = Imports:cf_imports, names_to = "flowtype", 
                    values_to = "Q") %>%
        pivot_wider(names_from = prod_category, values_from = Q) %>%
        mutate(Crude = 0)
    
    sum_reroute3 <- results_df$reroute_df %>%
        select(PADD,prod_category,Movements,Exports_NonCentAm,cf_gc_reroute) %>%
        group_by(prod_category,PADD) %>%
        summarize(Move = sum(Movements),Exp = sum(Exports_NonCentAm),  # sum over months
                    cf_Reroute = sum(cf_gc_reroute)) %>% ungroup() %>%
        mutate_at(c("Move","Exp","cf_Reroute"), conv_to_annual) %>% 
        mutate(Move = if_else(PADD=="1C",Move,0)) %>%
        group_by(prod_category) %>%
        summarize(Movements = sum(Move),Exports = mean(Exp),           # summarize over destination padds
                    cf_gc_Reroute = sum(cf_Reroute)) %>% ungroup() %>%
        mutate(cf_move = Movements + cf_gc_Reroute, cf_exp = Exports - cf_gc_Reroute) %>%
        select(prod_category,Movements,Exports,cf_move,cf_exp) %>%
        pivot_longer(cols = Movements:cf_exp, names_to = "flowtype", 
                    values_to = "Q") %>%
        pivot_wider(names_from = prod_category, values_from = Q) %>%
        mutate(PADD = "3")
    
    sum_flows <- rbind(sum_reroute1a,sum_reroute1b,sum_reroute1c,sum_reroute3) %>%
        mutate(flowtype = case_when(flowtype=="Imports" ~ "Imports",
                                    flowtype=="cf_gc_reroute" ~ "New movements from USGC",
                                    flowtype=="cf_imports" ~ "Counterfactual imports",
                                    flowtype=="Movements" ~ "Movements to PADD 1",
                                    flowtype=="Exports" ~ "Exports",
                                    flowtype=="cf_move" ~ "Counterfactual movements to PADD 1",
                                    flowtype=="cf_exp" ~ "Counterfactual exports")) %>%
        select(!PADD)

    # Producer surplus ------------------------------------------------------------
    # Delta PS in padd 1 is -delta P * flows in - delta CS
    # Delta PS in padd 3 is delta P * flows out - delta CS
    panel_cs_long <- panel_cs %>%
        pivot_longer(cols = Conventional:Crude, names_to = "prod_category", 
                values_to = "deltaCS") %>%
        mutate(deltaCS = if_else(is.na(deltaCS),0,deltaCS))
    
    panel_price_long <- panel_price %>%
        pivot_longer(cols = Conventional:Crude, names_to = "prod_category", 
                    values_to = "deltaP") %>%
        mutate(deltaP = if_else(is.na(deltaP),0,deltaP))
    
    panel_moves1_long <- results_df$reroute_df %>%
        select(prod_category,PADD,Imports,Movements) %>%
        group_by(prod_category,PADD) %>%
        summarize_all(list(sum)) %>% ungroup() %>%
        mutate_at(c("Imports","Movements"), conv_to_annual) %>%
        mutate(movestot = Imports + Movements) %>%
        mutate(Region = case_when(
            PADD == "1A" ~ "New England (1A)",
            PADD == "1B" ~ "Central Atlantic (1B)",
            PADD == "1C" ~ "Lower Atlantic (1C)")) %>%
        select(Region,prod_category,movestot) %>%
        mutate(movestot = -movestot)
    
    panel_moves3_long <- sum_reroute3 %>%
        select(flowtype,Conventional,"Jet Fuel",ULSD,Crude) %>%
        filter(flowtype=="Movements" | flowtype=="Exports") %>%
        pivot_longer(cols = Conventional:Crude, names_to = "prod_category", 
                    values_to = "movestot") %>%
        pivot_wider(names_from = flowtype, values_from = movestot) %>%
        mutate(movestot = Movements + Exports) %>%
        mutate(Region = "Gulf Coast (3)") %>%
        select(Region,prod_category,movestot)
    
    panel_moves_long <- rbind(panel_moves1_long,panel_moves3_long)
    
    panel_ps <- panel_cs_long %>%
        left_join(panel_price_long) %>% left_join(panel_moves_long) %>%
        mutate(movestot = if_else(is.na(movestot),0,movestot)) %>%
        mutate(PS = deltaP * movestot - deltaCS) %>%
        select(Region,prod_category,PS) %>%
        pivot_wider(names_from = prod_category, values_from = PS)
    
    return(list(sum_flows=sum_flows,
                sum_reroute= sum_reroute,
                sum_reroute1a = sum_reroute1a,
                sum_reroute1b = sum_reroute1b,
                sum_reroute1c = sum_reroute1c,
                sum_reroute3 = sum_reroute3,
                panel_efficiency=panel_efficiency,
                panel_price=panel_price,
                panel_cs=panel_cs,
                panel_ps=panel_ps,
                df_p_ec=df_p_ec,
                df_cs_ec=df_cs_ec))
}

main_tables <- get_cf_tables()

# -----------------------------------------------------------------------------
# EXPORT TO LATEX   --------------------------------------------
# -----------------------------------------------------------------------------
options(xtable.comment = FALSE)

export_tables <- function(results_tables = main_tables, 
                          stub = ""){

    ## COUNTERFACTUAL FLOW SUMMARY TABLE

    # Create the LaTeX table
    ttf <- xtable(results_tables$sum_flows, digits = c(0,0,0,0,0,0))

    ttf

    # Add the panel labels
    latex_tablef <- print(ttf, add.to.row = list(pos = list(c(0),c(3),c(6),c(9)), 
                                                command = c("\\midrule & Conventional & &  & \\\\\n
                            & Gasoline & Jet Fuel & ULSD & Crude \\\\ 
                            \\midrule \\multicolumn{5}{l}{\\textbf{New England (PADD 1a)}} \\\\\n",
                                                            "\\midrule \\multicolumn{5}{l}{\\textbf{Central Atlantic (PADD 1b)}} \\\\\n",
                                                            "\\midrule \\multicolumn{5}{l}{\\textbf{Lower Atlantic (PADD 1c)}} \\\\\n",
                                                            "\\midrule \\multicolumn{5}{l}{\\textbf{Gulf Coast (PADD 3)}} \\\\\n")),
                        include.rownames = FALSE, include.colnames = FALSE,
                        hline.after = c(13), caption.placement = "top", floating = FALSE)

    latex_tablef

    # Print the LaTeX table to a file
    file_path <- file.path(root, "output", "tables", 
                    paste0("counterfactual_flows_summary", stub, ".tex"))

    cat(latex_tablef, file = file_path)

    ## EXPORT SINGLE TABLES OF CF RESULTS

    export_panel <- function(fpanel, fname){
            tt <- print(xtable(fpanel), include.rownames = FALSE)
            tt <- gsub("\\centering\n", "", tt)
            tt <- gsub("\\\\begin\\{table\\}\\[ht\\]\n\\\\", "", tt)
            tt <- gsub("\\\\end\\{table\\}\n", "", tt)
            
            file_path <- file.path(root, "output", "tables", paste0(fname,stub,".tex"))
            cat(tt, file = file_path)
    }

    export_panel(results_tables$panel_efficiency, "counterfactual_efficiency")
    export_panel(results_tables$panel_price, "counterfactual_price")
    export_panel(results_tables$panel_cs, "counterfactual_cs")


    ## COMBINE COUNTERFACTUAL SUMMARY TABLE IN R THEN EXPORT 
    df_combined <- rbind(results_tables$panel_price,
                         results_tables$panel_efficiency,
                         results_tables$panel_cs,
                         results_tables$panel_ps) %>%
    
    mutate(Conventional = if_else(is.na(Conventional),0,Conventional),
            "Jet Fuel" = if_else(is.na(get("Jet Fuel")),0,get("Jet Fuel")),
            ULSD = if_else(is.na(ULSD),0,ULSD),
            Crude = if_else(is.na(Crude),0,round(Crude,2))) %>%
    select(Region,Conventional,"Jet Fuel",ULSD,Crude)

    # Create the LaTeX table
    mdigit <- matrix(c(rep(2,4*6), rep(0,11*6)), nrow=15, ncol=6, byrow=TRUE)

    tt <- xtable(df_combined, digits=mdigit)

    print(tt, include.rownames = FALSE)

    # Add the panel labels
    latex_table <- print(tt, add.to.row = list(pos = list(c(0),c(4),c(7),c(11)), 
        command = c(" & Conventional & &  & \\\\\n
                    Region & Gasoline & Jet Fuel & ULSD & Crude \\\\ 
                    \\midrule \\multicolumn{5}{l}{\\textbf{Price changes (\\$/bbl)}} \\\\\n",
                    "\\midrule \\multicolumn{5}{l}{\\textbf{Efficiency changes (\\$million/year)}} \\\\\n",
                    "\\midrule \\multicolumn{5}{l}{\\textbf{Consumer surplus changes (\\$million/year)}} \\\\\n",
                    "\\midrule \\multicolumn{5}{l}{\\textbf{Producer surplus changes (\\$million/year)}} \\\\\n")),
        include.rownames = FALSE, include.colnames = FALSE,
        hline.after = c(-1,15), floating = FALSE)

    latex_table

    # Print the LaTeX table to a file
    file_path <- file.path(root, "output", "tables", 
                    paste0("counterfactual_summary",stub,".tex"))
    cat(latex_table, file = file_path)
}


export_tables()


# RUN TABLES ASSUMING PORT EV INSTEAD OF CNV 
port_ev_results <- get_main_cf_dfs(PADD1c_port = "EV")
port_ev_tables <- get_cf_tables(results_df = port_ev_results)
export_tables(port_ev_tables,stub="_port_ev")


# RUN TABLES ASSUMING LOG SPEC FOR COUNTERFACTUAL SHIPPING COSTS
logship_results <- get_main_cf_dfs(distspec = "LD")
logship_tables <- get_cf_tables(results_df = logship_results)
export_tables(logship_tables,stub="_logship")




# -----------------------------------------------------------------------------
# EXPORT SOME SINGLE NUMBER TEX FILES
# -----------------------------------------------------------------------------
texfolder <- file.path(root,"output","tex_numbers")

# Flows
# PADD 1c cf gasoline imports
tex_cf_imports_1c_gas <- file.path(texfolder,"cf","imports_1c_gas.tex")
cat(main_tables$sum_reroute1c %>% filter(flowtype=="cf_imports") %>% 
      select(Conventional) %>% pull() %>% round(), file=tex_cf_imports_1c_gas)

# PADD 1 new movements
padd1new <- main_tables$sum_reroute %>%
  select(prod_category,cf_gc_reroute,Imports) %>% group_by(prod_category) %>%
  summarize_all(list(sum)) %>% ungroup() %>%
  mutate(PctReplaced = cf_gc_reroute / Imports * 100)

tex_cf_newmove_1_gas <- file.path(texfolder,"cf","newmove_1_gas.tex")
cat(padd1new %>% filter(prod_category=="Conventional") %>% 
      select(cf_gc_reroute) %>% pull() %>% round(), file=tex_cf_newmove_1_gas)
tex_cf_newmove_1_jet <- file.path(texfolder,"cf","newmove_1_jet.tex")
cat(padd1new %>% filter(prod_category=="Jet Fuel") %>% 
      select(cf_gc_reroute) %>% pull() %>% round(), file=tex_cf_newmove_1_jet)
tex_cf_newmove_1_ULSD <- file.path(texfolder,"cf","newmove_1_ULSD.tex")
cat(padd1new %>% filter(prod_category=="ULSD") %>% 
      select(cf_gc_reroute) %>% pull() %>% round(), file=tex_cf_newmove_1_ULSD)
tex_cf_newmove_1_crude <- file.path(texfolder,"cf","newmove_1_crude.tex")
cat(padd1new %>% filter(prod_category=="Crude") %>% 
      select(cf_gc_reroute) %>% pull() %>% round(), file=tex_cf_newmove_1_crude)
tex_cf_pctreplaced_1_gas <- file.path(texfolder,"cf","pctreplaced_1_gas.tex")
cat(padd1new %>% filter(prod_category=="Conventional") %>% 
      select(PctReplaced) %>% pull() %>% round(), file=tex_cf_pctreplaced_1_gas)
tex_cf_pctreplaced_1_jet <- file.path(texfolder,"cf","pctreplaced_1_jet.tex")
cat(padd1new %>% filter(prod_category=="Jet Fuel") %>% 
      select(PctReplaced) %>% pull() %>% round(), file=tex_cf_pctreplaced_1_jet)
tex_cf_pctreplaced_1_ULSD <- file.path(texfolder,"cf","pctreplaced_1_ULSD.tex")
cat(padd1new %>% filter(prod_category=="ULSD") %>% 
      select(PctReplaced) %>% pull() %>% round(), file=tex_cf_pctreplaced_1_ULSD)
tex_cf_pctreplaced_1_crude <- file.path(texfolder,"cf","pctreplaced_1_crude.tex")
cat(padd1new %>% filter(prod_category=="Crude") %>% 
      select(PctReplaced) %>% pull() %>% round(), file=tex_cf_pctreplaced_1_crude)

# Total existing and cf movements
padd3move <- main_tables$sum_reroute3 %>%
  mutate(Tot = Conventional + Crude + get("Jet Fuel") + ULSD)
tex_cf_existmoves_all_all <- file.path(texfolder,"cf","existmoves_all_all.tex")
cat(padd3move %>% filter(flowtype=="Movements") %>%
      select(Tot) %>% pull() %>% round(), file=tex_cf_existmoves_all_all)
tex_cf_cfmoves_all_all <- file.path(texfolder,"cf","cfmoves_all_all.tex")
cat(padd3move %>% filter(flowtype=="cf_move") %>%
      select(Tot) %>% pull() %>% round(), file=tex_cf_cfmoves_all_all)

# PADD 3 new movements of gasoline as share of exports
padd3new_gas <- main_tables$sum_reroute3 %>%
  select(flowtype,Conventional) %>%
  pivot_wider(names_from = flowtype, values_from = Conventional) %>%
  mutate(PctExpGas = (Exports-cf_exp)/Exports*100)
tex_cf_pctdiverted_3_gas <- file.path(texfolder,"cf","pctdiverted_3_gas.tex")
cat(padd3new_gas %>%
      select(PctExpGas) %>% pull() %>% round(), file=tex_cf_pctdiverted_3_gas)

# Number of months in which the price diff exceeds shipping costs to padd 1a
df_nmonths_1a <- main_results$main_subpadd_df %>%
  filter(Year>=2018 & Year <=2019) %>%
  filter(PADD=="1A") %>%
  select(prod_category, price_diff, shipcost_argus) %>%
  mutate(shipflag = if_else(price_diff>=shipcost_argus,1,0)) %>%
  select(prod_category,shipflag) %>%
  group_by(prod_category) %>% 
  summarize_all(list(sum))
tex_nmonths_posmargin_gas_1a <- file.path(texfolder,"cf","nmonths_posmargin_gas_1a.tex")
cat(df_nmonths_1a %>% filter(prod_category=="Conventional") %>% 
      select(shipflag) %>% pull() %>% round(), file=tex_nmonths_posmargin_gas_1a)
tex_nmonths_posmargin_jet_1a <- file.path(texfolder,"cf","nmonths_posmargin_jet_1a.tex")
cat(df_nmonths_1a %>% filter(prod_category=="Jet Fuel") %>% 
      select(shipflag) %>% pull() %>% round(), file=tex_nmonths_posmargin_jet_1a)
tex_nmonths_posmargin_ulsd_1a <- file.path(texfolder,"cf","nmonths_posmargin_ulsd_1a.tex")
cat(df_nmonths_1a %>% filter(prod_category=="ULSD") %>% 
      select(shipflag) %>% pull() %>% round(), file=tex_nmonths_posmargin_ulsd_1a)

# Number of months in which the price diff exceeds shipping costs to padd 1b
df_nmonths_1b <- main_results$main_subpadd_df %>%
  filter(Year>=2018 & Year <=2019) %>%
  filter(PADD=="1B") %>%
  select(prod_category, price_diff, shipcost_argus) %>%
  mutate(shipflag = if_else(price_diff>=shipcost_argus,1,0)) %>%
  select(prod_category,shipflag) %>%
  group_by(prod_category) %>% 
  summarize_all(list(sum))
tex_nmonths_posmargin_gas_1b <- file.path(texfolder,"cf","nmonths_posmargin_gas_1b.tex")
cat(df_nmonths_1b %>% filter(prod_category=="Conventional") %>% 
      select(shipflag) %>% pull() %>% round(), file=tex_nmonths_posmargin_gas_1b)

# Price changes
tex_deltap_gas_1c <- file.path(texfolder,"cf","tex_deltap_gas_1c.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1C") %>% select(Conventional) %>%
    mutate(Conventional = Conventional * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_gas_1c)

tex_deltap_jet_1c <- file.path(texfolder,"cf","tex_deltap_jet_1c.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1C") %>% select("Jet Fuel") %>%
      mutate("Jet Fuel" = get("Jet Fuel") * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_jet_1c)

tex_deltap_ulsd_1c <- file.path(texfolder,"cf","tex_deltap_ulsd_1c.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1C") %>% select(ULSD) %>%
      mutate(ULSD = ULSD * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_ulsd_1c)

tex_deltap_gas_1b <- file.path(texfolder,"cf","tex_deltap_gas_1b.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1B") %>% select(Conventional) %>%
      mutate(Conventional = Conventional * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_gas_1b)

tex_deltap_gas_1a <- file.path(texfolder,"cf","tex_deltap_gas_1a.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1A") %>% select(Conventional) %>%
      mutate(Conventional = Conventional * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_gas_1a)
      
tex_deltap_gas_3 <- file.path(texfolder,"cf","tex_deltap_gas_3.tex")
cat(main_tables$panel_price %>% filter(Region=="Gulf Coast (3)") %>% select(Conventional) %>%
      pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_gas_3)

tex_deltap_crude_1b <- file.path(texfolder,"cf","tex_deltap_crude_1b.tex")
cat(main_tables$df_p_ec %>% filter(PADD=="1B") %>% select(Crude) %>%
      mutate(Crude = Crude * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_deltap_crude_1b)

# Consumption-weighted (across subpadds) product price changes
df_conssum <- main_results$main_subpadd_df %>%
  filter(prod_category!="Crude") %>%
  select(prod_category,PADD,Consumption) %>%
  group_by(prod_category,PADD) %>%
  summarize_all(list(sum)) %>% ungroup() %>%
  group_by(prod_category) %>%
  mutate(ConsTot = sum(Consumption), ConsShare = Consumption / ConsTot) %>% ungroup() %>%
  select(prod_category,PADD,ConsShare)
df_weighteddeltap <- main_tables$df_p_ec %>%
  select(!Crude) %>%
  pivot_longer(cols = Conventional:"Jet Fuel", names_to = "prod_category", 
               values_to = "DeltaP") %>%
  left_join(df_conssum) %>%
  mutate(WDeltaP = DeltaP * ConsShare) %>% select(!PADD) %>%
  group_by(prod_category) %>%
  summarize_all(list(sum)) %>% ungroup()
tex_wdeltap_gas <- file.path(texfolder,"cf","tex_wdeltap_gas.tex")
cat(df_weighteddeltap %>% filter(prod_category=="Conventional") %>%
      mutate(WDeltaP = WDeltaP * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_wdeltap_gas)
tex_wdeltap_jet <- file.path(texfolder,"cf","tex_wdeltap_jet.tex")
cat(df_weighteddeltap %>% filter(prod_category=="Jet Fuel") %>%
      mutate(WDeltaP= WDeltaP * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_wdeltap_jet)
tex_wdeltap_ulsd <- file.path(texfolder,"cf","tex_wdeltap_ulsd.tex")
cat(df_weighteddeltap %>% filter(prod_category=="ULSD") %>%
      mutate(WDeltaP = WDeltaP * (-1)) %>% pull() %>% 
      formatC(digits=2,format="f"), file=tex_wdeltap_ulsd)


# Efficiency gains
df_sum_eff <- main_tables$panel_efficiency %>%
  mutate(Crude = if_else(is.na(Crude),0,Crude)) %>%
  mutate(toteff = Conventional + ULSD + get("Jet Fuel") + Crude)
df_sumsum_eff <- df_sum_eff %>%
  select(Conventional,ULSD,"Jet Fuel",Crude,toteff) %>%
  summarize_all(list(sum))
tex_eff_all_1c <- file.path(texfolder,"cf","eff_all_1c.tex")
cat(df_sum_eff %>% filter(Region=="Lower Atlantic (1C)") %>%
      select(toteff) %>% pull() %>% round(), file=tex_eff_all_1c)
tex_eff_all_all <- file.path(texfolder,"cf","eff_all_all.tex")
cat(df_sumsum_eff %>%
      select(toteff) %>% pull() %>% round(), file=tex_eff_all_all)

# CS changes
df_sum_cs <- main_tables$panel_cs %>%
  mutate(Crude = if_else(is.na(Crude),0,Crude)) %>%
  mutate(ULSD = if_else(is.na(ULSD),0,ULSD)) %>%
  mutate(totcsprod = Conventional + ULSD + get("Jet Fuel")) %>%
  mutate(totcs = totcsprod + Crude)
df_sum1_cs <- df_sum_cs %>%
  filter(Region!="Gulf Coast (3)") %>%
  select(Conventional,ULSD,"Jet Fuel",Crude,totcsprod,totcs) %>%
  summarize_all(list(sum))
df_sumsum_cs <- df_sum_cs %>%
  select(Conventional,ULSD,"Jet Fuel",Crude,totcsprod,totcs) %>%
  summarize_all(list(sum))
tex_cs_prod_1 <- file.path(texfolder,"cf","cs_prod_1.tex")
cat(df_sum1_cs %>%
      select(totcsprod) %>% pull() %>% round(), file=tex_cs_prod_1)
tex_cs_prod_1c <- file.path(texfolder,"cf","cs_prod_1c.tex")
cat(df_sum_cs %>% filter(Region=="Lower Atlantic (1C)") %>%
      select(totcsprod) %>% pull() %>% round(), file=tex_cs_prod_1c)
tex_cs_prod_3 <- file.path(texfolder,"cf","cs_prod_3.tex")
cat(df_sum_cs %>% filter(Region=="Gulf Coast (3)") %>%
      select(totcsprod) %>%  mutate(totcsprod = totcsprod * (-1)) %>%
      pull() %>% round(), file=tex_cs_prod_3)
tex_cs_crude_1b <- file.path(texfolder,"cf","cs_crude_1b.tex")
cat(df_sum_cs %>% filter(Region=="Central Atlantic (1B)") %>%
      select(Crude) %>% pull() %>% round(), file=tex_cs_crude_1b)
tex_cs_all_1 <- file.path(texfolder,"cf","cs_all_1.tex")
cat(df_sum1_cs %>%
      select(totcs) %>% pull() %>% round(), file=tex_cs_all_1)
tex_cs_all_all <- file.path(texfolder,"cf","cs_all_all.tex")
cat(df_sumsum_cs %>%
      select(totcs) %>% pull() %>% round(), file=tex_cs_all_all)

# PS changes
df_sum_ps <- main_tables$panel_ps %>%
  mutate(totpsprod = Conventional + ULSD + get("Jet Fuel")) %>%
  mutate(totps = totpsprod + Crude)
df_sum1_ps <- df_sum_ps %>%
  filter(Region!="Gulf Coast (3)") %>%
  select(Conventional,ULSD,"Jet Fuel",Crude,totpsprod,totps) %>%
  summarize_all(list(sum))
df_sumsum_ps <- df_sum_ps %>%
  select(Conventional,ULSD,"Jet Fuel",Crude,totpsprod,totps) %>%
  summarize_all(list(sum))
tex_ps_all_all <- file.path(texfolder,"cf","ps_all_all.tex")
cat(df_sumsum_ps %>% select(totps) %>% mutate(totps = totps * (-1)) %>%
      pull() %>% round(), file=tex_ps_all_all)
tex_ps_all_3 <- file.path(texfolder,"cf","ps_all_3.tex")
cat(sum3_ps <- df_sum_ps %>% filter(Region=="Gulf Coast (3)") %>%
      select(totps) %>% pull() %>% round(), file=tex_ps_all_3)
tex_ps_all_1 <- file.path(texfolder,"cf","ps_all_1.tex")
cat(sum3_ps <- df_sum1_ps %>% select(totps) %>% 
      mutate(totps = totps * (-1)) %>% pull() %>% round(), file=tex_ps_all_1)