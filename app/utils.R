#
#
#
# wu <- dataRetrieval::readNWISuse("California", "Los Angeles") %>%
#   janitor::clean_names() %>%
#     replace(is.na(.), 0)
# pub <- wu %>% select(1:6,starts_with("public"))
# dom <- wu %>% select(1:6,starts_with("domestic"))
# comm <- wu %>% select(1:6,starts_with("commericial"))
# ind <- wu %>% select(1:6,starts_with("industrial"))
# thermo <- wu %>% select(1:6,contains("thermoelectric"))
# mine <- wu %>% select(1:6,starts_with("mining"))
# livestock <- wu %>% select(1:6,starts_with("livestock"))
# aqua <- wu %>% select(1:6,starts_with("aqua"))
# hydro <- wu %>% select(1:6,starts_with("hydro"))
# irrig <- wu %>% select(1:6,starts_with("irrigation"))
# waste <- wu %>% select(1:6,starts_with("wastewater"))
#
# stats <- wu %>% select(1:6)
#
# pub <- pub %>%
#   mutate(across(7:27, as.numeric)) %>%
#   replace(is.na(.), 0) %>%
#   pivot_longer(7:27, names_to = "public", values_to = "withdrawals")
#
# dom <- dom %>%
#   mutate(across(7:24, as.numeric)) %>%
#   replace(is.na(.), 0) %>%
#   pivot_longer(7:24, names_to = "domestic", values_to = "withdrawals")
#
# la_county <- water_use_data("California", "Los Angeles")


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



