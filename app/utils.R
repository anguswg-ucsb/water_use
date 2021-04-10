
get_pop_data <- function(df){
  county_pop <- df %>%
    group_by(county_nm) %>%
    mutate(population = total_population_total_population_of_area_in_thousands*1000,
           county_cd = as.numeric(county_cd)) %>%
    group_by(county_nm) %>%
    arrange(population) %>%
    mutate(growth_rate = (population - lag(population))/lag(population)) %>%
    mutate(growth_rate = growth_rate*100) %>%
    select(1:5, population, growth_rate)
}

# g <- get_pop_data(df)

# ggplot2::ggplot(tst, aes(x = year, y = growth_rate)) +
#   ggplot2::geom_line(aes(col = county_nm))
# df <- dataRetrieval::readNWISuse("CA", "ALL") %>%
#   janitor::clean_names()
# df2 <- tidy_sectors(df)

# cols <- viridisLite::viridis(12)
# cols <- substr(cols, 0, 7)

# num = length(unique(tst$county_nm))
# cols = colorRampPalette(brewer.pal(12, "Spectral"))(num)
#
# highchart() %>%
#   hc_add_series(tst, type = "line", hcaes(x = year, y = growth_rate, group = "county_nm")) %>%
#   hc_add_theme(hc_thm = hc_theme_darkunica())
#   hc_colors(cols)

tidy_sectors <- function(df){
  sector <- df %>%
    janitor::clean_names() %>%
    rename(population = total_population_total_population_of_area_in_thousands)
  sector <- df %>%
    mutate(across(7:last_col(), as.numeric)) %>%
    replace(is.na(.), 0) %>%
    mutate(
      public_supply_total_self_supplied_withdrawals_fresh_in_mgal_d =
            public_supply_total_self_supplied_withdrawals_fresh_in_mgal_d + domestic_total_self_supplied_withdrawals_fresh_in_mgal_d,
      agriculture_total_self_supplied_withdrawals_fresh_in_mgal_d =
            irrigation_total_total_self_supplied_withdrawals_fresh_in_mgal_d,
      public_supply_self_supplied_surface_water_withdrawals_fresh_in_mgal_d = public_supply_self_supplied_surface_water_withdrawals_fresh_in_mgal_d + domestic_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
      agriculture_self_supplied_surface_water_withdrawals_fresh_in_mgal_d = irrigation_total_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
      public_supply_self_supplied_groundwater_withdrawals_fresh_in_mgal_d = public_supply_self_supplied_groundwater_withdrawals_fresh_in_mgal_d + domestic_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
      agriculture_self_supplied_groundwater_withdrawals_fresh_in_mgal_d = irrigation_total_self_supplied_groundwater_withdrawals_fresh_in_mgal_d ) %>% # population = population*1000
    select(-domestic_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -domestic_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -domestic_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
              -aquaculture_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -aquaculture_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -aquaculture_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
              -livestock_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -livestock_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
           -livestock_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -livestock_animal_specialties_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -livestock_stock_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -livestock_animal_specialties_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
           -livestock_stock_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
           -livestock_animal_specialties_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -livestock_stock_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -irrigation_total_total_self_supplied_withdrawals_fresh_in_mgal_d,
           -irrigation_total_self_supplied_surface_water_withdrawals_fresh_in_mgal_d,
           -irrigation_total_self_supplied_groundwater_withdrawals_fresh_in_mgal_d,
           -total_population_total_population_of_area_in_thousands) %>%
    pivot_longer(7:last_col(), names_to = "sector", values_to = "withdrawals")
  #   colnames(sector)[7] <-  sub("_.*", "", colnames(sector)[7])
}

make_graph <- function(df) {
  cols <- viridisLite::viridis(15)
  cols <- substr(cols, 0, 7)
  highchart() %>%
    hc_add_series(df2, type = "column", hcaes(x = year, y = withdrawals, group = "sector")) %>%
    hc_colors(cols)
    # hc_colors(c("red", "green", "grey", "blue", "orange")) %>%
    hc_yAxis(title = list(text ="Mgal/day")) %>%
    # hc_xAxis(categories = df$year) %>%
    hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL)
}

# USGS Water use data
water_use_data <- function(state, county) {
  water_use <- dataRetrieval::readNWISuse(stateCd = state, countyCd = county) %>%
    janitor::clean_names()
  water_use <- water_use %>%
    select(1:6, contains("total_self_supplied_withdrawals_surface_water"), contains("surface_water_withdrawals_for_golf"))
  water_use <- water_use %>%
    mutate(across(5:22, as.numeric)) %>%
    replace(is.na(.), 0)
  water_use <- water_use %>%
    rename(statefips = state_cd,
           countyfips = county_cd,
           pop = total_population_total_population_of_area_in_thousands,
           livestock1 = livestock_stock_total_self_supplied_withdrawals_surface_water_in_mgal_d,
           livestock2 = livestock_animal_specialties_total_self_supplied_withdrawals_surface_water_in_mgal_d,
           therm1 = thermoelectric_power_closed_loop_cooling_total_self_supplied_withdrawals_surface_water_in_mgal_d,
           therm2 = thermoelectric_power_once_through_cooling_total_self_supplied_withdrawals_surface_water_in_mgal,
           aquacultere1 = aquaculture_total_self_supplied_withdrawals_surface_water_in_mgal_d,
           irrigation1 = irrigation_total_total_self_supplied_withdrawals_surface_water_in_mgal_d,
           irrigation2 = irrigation_golf_courses_self_supplied_surface_water_withdrawals_for_golf_courses_fresh_in_mgal_d)

  for ( col in 1:ncol(water_use)){
    colnames(water_use)[col] <-  sub("_.*", "", colnames(water_use)[col])
  }

  water_use <- water_use %>%
    mutate(Public = public + domestic,
           Thermoelectric = total,
           Irrigation = livestock1 + livestock2 + aquacultere1 + irrigation1 + irrigation2) %>%
    select(1:6, Public, Irrigation, Industrial = industrial, Mining = mining,  Thermoelectric)
  water_use <- water_use %>%
    tidyr::unite('fips', statefips, countyfips)
  water_use$fips <- gsub("_", "", water_use$fips)
  water_use$fips <- as.numeric(water_use$fips)
  water_use <- water_use %>%
    tidyr::pivot_longer(6:10, names_to = "sector", values_to = "withdrawals")
  #
  # water_use <- rename(water_use, YEAR = year)
  # water_use <- group_by(water_use, Sector, YEAR)
}
# library(ggplot2)
# library(highcharter)
# highchart() %>%
#   hc_add_series(la_county, type = "line", hcaes(x = year, y = withdrawals, group = sector))
# ggplot(la_county, aes(x = year, y = withdrawals)) +
#   geom_line(aes(col = sector))
#



