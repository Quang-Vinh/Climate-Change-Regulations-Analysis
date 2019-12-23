# Load libraries
library(countrycode)
library(ggrepel)
library(fable)
library(feasts)
library(forecast)
library(ggthemes)
library(janitor)
library(lmtest)
library(lubridate)
library(plotly)
library(plyr)
library(skimr)
library(tidyverse)
library(tsibble)
library(viridis)

# # Load and clean value per activity data from data.oecd
# subject_mappings <- tribble(
#   ~code, ~name,
#   "AGRFORTFISH", "Agriculture and Fishery",
#   "CONSTR", "Construction",
#   "FINANCEINS", "Finance and Insurance",
#   "INDUSENRG", "Industry and Energy",
#   "INFCOMM", "Information Communication",
#   "MFG", "Manufacturing",
#   "OTHSERVACT", "Other Service Activities",
#   "PROSCISUPP", "Profesional and Scientific Support Services",
#   "PUBADMINEDUSOC", "Public",
#   "REALEST", "Real Estate",
#   "SERV", "Services",
#   "TOT", "Total Growth",
#   "WHLEHTELTRANSP", "Wholesale, Retail, Trade, Transport, Accomodation"
# )
# 
# value_per_activity <- 
#   read_csv('../data/value_added_per_act.csv') %>% 
#   clean_names() %>%
#   select(-indicator, -flag_codes, -frequency) %>% 
#   dplyr::rename(year = time, country = location) %>% 
#   mutate(country = countrycode(country, 'iso3c', 'country.name.en', nomatch=NULL)) %>%   # Replace 3 letter code with full name
#   mutate(country = replace(country, country == 'EU', 'European Union')) %>%     # European union is not covered by the country code
#   mutate(subject = mapvalues(subject, from=subject_mappings$code, to=subject_mappings$name)) %>%  # Replace subject code with name
#   pivot_wider(names_from = measure, values_from = value) 



# Who are biggest producers of GHG ----------------------------------------

# Load green house emissions data
country_ghg_emissions <- 
  read_csv('../data/CAIT Country GHG Emissions/CAIT Country GHG Emissions.csv', skip=2) %>% 
  clean_names() %>% 
  select(country, year, total_ghg_emissions_mtco2e = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>% 
  filter(country != 'World', !str_detect(country, 'European Union')) %>%  # Remove as they are not countries
  na.omit()

year_range <- country_ghg_emissions %>% 
  summarise(min_year = min(year), max_year = max(year))

# List of countries that will be omitted from analysis. They do not contain data from full year ranges 1990-2014
country_omit <- 
  country_ghg_emissions %>%
  group_by(country) %>% 
  summarise(max_year=max(year), min_year=min(year)) %>% 
  filter(max_year != year_range$max_year |
        min_year != year_range$min_year)

# Remove from df the omitted countries
country_ghg_emissions <- country_ghg_emissions %>% 
  filter(!(country %in% country_omit$country))

# Total emissions per country
country_total_ghg_emissions <- country_ghg_emissions %>% 
  group_by(country) %>% 
  summarise(total_ghg_emissions_mtco2e = sum(total_ghg_emissions_mtco2e))

# View only top n percent countries based on ghg emissions
percent_cutoff <- 0.80
country_top_total_emissions <- country_total_ghg_emissions %>% 
  arrange(total_ghg_emissions_mtco2e %>% desc()) %>%
  mutate(cum_percent_ghg = cumsum(total_ghg_emissions_mtco2e)/sum(total_ghg_emissions_mtco2e),
         percent_global_ghg_emissions = total_ghg_emissions_mtco2e / sum(total_ghg_emissions_mtco2e)) %>% 
  filter(cum_percent_ghg < percent_cutoff) %>% 
  select(-cum_percent_ghg)

# Barplot of top 80% countries avg ghg emission
country_top_total_emissions %>% 
  ggplot(aes(reorder(country, total_ghg_emissions_mtco2e), total_ghg_emissions_mtco2e)) +
  geom_bar(stat='identity') +
  geom_col(position = 'dodge') +
  geom_text(aes(label = round(percent_global_ghg_emissions, digits=4)), hjust = -0.2, size = 3) +
  coord_flip() + 
  labs(
    title = paste0('Total GHG emissions of countries making up ', percent_cutoff*100, '% of global GHG emission'),
    subtitle = (paste0(year_range$min_year, '-', year_range$max_year)),
    x = 'Country',
    y = 'Total GHG emission (mtco2e)'
  ) +
  theme_gdocs()



# World map visualization -------------------------------------------------
world_data <- map_data('world') %>% 
  filter(region != 'Antarctica') %>% 
  mutate(region = countrycode(region, 'iso3c', 'country.name.en', nomatch=NULL))  # Replace 3 letter codes with full name

# Manually map the countries together
country_mappings <-
  tribble(
    ~world_data, ~country_emissions,
    'North Korea', 'Korea (North)',
    'South Korea', 'Korea (South)', 
    'Russia', 'Russian Federation',
    'UK', 'United Kingdom',
    'Ivory Coast', "Cote d'Ivoire",
    'Democratic Republic of the Congo', 'Congo, Dem. Republic',
    'Republic of Congo', 'Congo',
    'Antigua', 'Antigua & Barbuda',
    'Barbuda', 'Antigua & Barbuda',
    'Bahamas', 'Bahamas, The',
    'Bosnia and Herzegovina', 'Bosnia & Herzegovina',
    'Gambia', 'Gambia, The',
    'Trinidad', 'Trinidad & Tobago',
    'Tobago', 'Trinidad & Tobago',
    'Macedonia', 'Macedonia, FYR',
    'Saint Kitts', 'Saint Kitts & Nevis',
    'Nevis', 'Saint Kitts & Nevis',
    'Saint Vincent', 'Saint Vincent & Grenadines',
    'Grenadines', 'Saint Vincent & Grenadines',
    'Sao Tome and Principe', 'Same Tome & Principe',
    'United States', 'United States of America'
  )

world_emission_data <- world_data %>% 
  mutate(region = mapvalues(region, country_mappings$world_data, country_mappings$country_emissions)) %>% 
  inner_join(country_total_ghg_emissions, by=c('region'='country')) %>% 
  select(long, lat, group, region, total_ghg_emissions_mtco2e) %>% 
  na.omit()

ggplot() +
  geom_polygon(data=world_data, aes(x=long, y=lat, group=group), fill='white', colour='grey', size=0.01) +
  coord_equal() +
  geom_polygon(data=world_emission_data, aes(x=long, y=lat, group=group, fill=total_ghg_emissions_mtco2e)) +
  scale_fill_viridis(name = 'Total GHG emissions (mtco2e)') +
  labs(
    title = 'Total GHG emissions',
    subtitle = paste0(year_range$min_year, '-', year_range$max_year)
  ) +
  theme(
    legend.position = 'bottom'
  ) +
  theme_map()


# Kyoto Protocol  ---------------------------------------------------------
# Determine if since it's creation it had an effect starting 2005. The treaty was continued by parties so we will
# do our analysis on the intervention at 2005

# As all the data is yearly we will do our analysis in terms of years
kyoto_protocol_effective_date <- c(start = ymd("2005-02-16"), end=ymd("2012-12-31"))
kyoto_start_year <- 2005

# Check worldwide effect
world_ghg_emissions_ts <- country_ghg_emissions %>% 
  group_by(year) %>% 
  summarise(total_ghg_emissions_mtco2e = sum(total_ghg_emissions_mtco2e)) %>%
  as_tsibble(index=year)

p <- world_ghg_emissions_ts %>% 
  autoplot(total_ghg_emissions_mtco2e) +
  geom_point() +
  geom_vline(xintercept=kyoto_start_year, linetype='dashed', colour='blue') +
  geom_text(aes(x=kyoto_start_year, y=max(world_ghg_emissions_ts$total_ghg_emissions_mtco2e)), label='Kyoto Protocol adopted') +
  labs(
    title = paste0('World GHG emissions (', year_range$min_year, '-', year_range$max_year, ')'),
    x = 'Year',
    y = 'GHG emissions (mtco2e)'
  ) +
  theme_gdocs()
ggplotly(p)


# Intervention analysis using Regression with ARIMA errors ----------------
# https://stats.stackexchange.com/questions/43623/comparing-pre-retrofit-utility-bill-time-series-data-to-post-retrofit-data?noredirect=1&lq=1
# http://freerangestats.info/blog/2018/08/14/fuel-prices
# https://otexts.com/fpp2/regarima.html chapter 9.2 
# https://newonlinecourses.science.psu.edu/stat510/lesson/9/9.2
# Based on equation it seems like it detects constant change to mean and not gradual changes


# Fit regression with arima errors with intervention variable
# >= since  we will assume that the kyoto protocol starting beginning that year (close ish Feb 16) so the countries
# were under the protocol for 2005
arimaWithKyotoIntervention <- function(df_tsibble) {
  z <- df_tsibble %>% 
    transmute(z = year >= kyoto_start_year) %>% 
    pull(z) %>%
    as.numeric()
  fit <- df_tsibble %>% auto.arima(xreg=z)
  return (fit)
}

world_ghg_emissions_arima <- world_ghg_emissions_ts %>% arimaWithKyotoIntervention()

# Plot fitted values. Looks pretty good
p <- world_ghg_emissions_arima$x %>% 
  autoplot(series='Actual') +
  autolayer(world_ghg_emissions_arima$fitted, series='Fitted') +
  geom_vline(xintercept=kyoto_start_year, linetype='dashed') +
  geom_text(aes(x=kyoto_start_year, y=max(world_ghg_emissions_ts$total_ghg_emissions_mtco2e)), label='Kyoto Protocol adopted') +
  scale_x_continuous(breaks = seq(year_range$min_year, year_range$max_year, by = 1)) +
  labs(
    title = 'Fitted values from ARIMA on World GHG emissions',
    x = 'Year',
    y = 'GHG emissions (mtco2e)',
    colour = 'Type'
  ) +
  theme_hc()
ggplotly(p)

# https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
# (1-pnorm(abs(aa$coef)/sqrt(diag(aa$var.coef))))*2 or with coeftest
# The xreg has a high p-value of 0.5867 thus we can conclude that it is not significant. Which matches what the data looks like
# as there doesn't seem to be much change after the intervention. As well, xreg is positive so again doesn't show
# that the kyoto protocol helped decrease the ghg emissions
world_ghg_emissions_arima
coeftest(world_ghg_emissions_arima)

# Residuals seem normal enough. As well we can conclude that the errors are independent as p-value is quite high 0.1408
world_ghg_emissions_arima %>% checkresiduals()
world_ghg_emissions_arima %>%
  residuals(type='innovation') %>%
  as_tsibble() %>%
  ggplot(aes(sample=value)) +
  stat_qq() + stat_qq_line()


# CONCLUSION: No the kyoto accord did not have any significant impact on global ghg 
# The accord includes 192 countries but has not shown to be effective in reducing the global ghg emissions
# However, it would be better if we had more data points as 1990-2014 yearly does not seem like enough to draw conclusions from


# Intervention analysis per country ---------------------------------------
# https://unfccc.int/process/parties-non-party-stakeholders/parties-convention-and-observer-states?field_partys_partyto_target_id%5B512%5D=512
# Manually going through the site (because they do not allow use of their data in derivative works) to confirm that
# every country in the country_top_total_emissions list is part of the kyoto protocol
# Assume that all countries were admitted at 2005, as we cannot get that data
# Canada left in Dec 15, 2012 but assume that the intervention still had an effect on canada's regulations even after


# GHG emissions for top 80% countries over time
country_top_total_emissions_ts <-
  country_top_total_emissions %>% 
  select(country) %>% 
  inner_join(country_ghg_emissions, by='country') %>%  
  as_tsibble(key=country, index=year)

# Plot emissions time series for each country
p <- 
  country_top_total_emissions_ts %>% 
  mutate(country = fct_reorder(country, total_ghg_emissions_mtco2e, tail, n=1, .desc=TRUE)) %>% 
  autoplot(total_ghg_emissions_mtco2e) +
  geom_vline(xintercept=kyoto_start_year, linetype='dashed') +
  scale_x_continuous(breaks = seq(year_range$min_year, year_range$max_year, by = 1)) +
  labs(
    title = paste0('Total GHG emissions of countries making up ', percent_cutoff*100, '% of global GHG emission excluding Canada',
                   ' (', year_range$min_year, '-', year_range$max_year, ')'),
    y = 'GHG emission (mtco2e)',
    x = 'Year',
    colour = 'Country'
  ) + 
  theme_hc()
ggplotly(p)

# Create arima model per country
countries <- country_top_total_emissions_ts %>% pull(country) %>% unique()
countries_arima <- vector('list', length(countries))

for (i in 1:length(countries)) {
  country <- countries[[i]]
  country_emissions_ts <- 
    country_top_total_emissions_ts %>% 
    filter(country == countries[i])
  countries_arima[[country]] <- country_emissions_ts %>% arimaWithKyotoIntervention()
}


# Check for which countries the xreg coefficient was significant
# https://multithreaded.stitchfix.com/blog/2015/10/15/multiple-hypothesis-testing/
# https://www.statisticshowto.datasciencecentral.com/familywise-error-rate/
# Use family wise error rate Bonferroni correction alpha/n to reject, low power though and high prob of type II errors

alpha <- 0.05 / length(countries)

countries_arima_significant <- vector('character')
for (i in 1:length(countries)) {
  country <- countries[[i]]
  p_val <- coeftest(countries_arima[[country]])['xreg','Pr(>|z|)']
  if (p_val < alpha) {
    countries_arima_significant <- c(countries_arima_significant, country)
  }
}

# Only indonesia has significant xreg coefficient
p <- 
  country_top_total_emissions_ts %>% 
  filter(country %in% countries_arima_significant) %>% 
  ggplot(aes(x=year, y=total_ghg_emissions_mtco2e, colour=country)) +
  geom_line() +
  geom_vline(xintercept=kyoto_start_year, linetype='dashed') +
  scale_x_continuous(breaks = seq(year_range$min_year, year_range$max_year, by = 1)) +
  labs(
    title = paste0('GHG emissions of countries with significant p-values',
                   ' (', year_range$min_year, '-', year_range$max_year, ')'),
    y = 'GHG emission (mtco2e)',
    x = 'Year'
  ) + 
  theme_hc()
ggplotly(p)


## CONCLUSION: No Kyoto protocol has no significant impact in the mean change of countries ghg emissions, except for Indonesia
# in this case it increased and however cannot definitively conclude that it was due to the event. In either case it definitely
# did not decrease





# Check country regulation activity -----------------------------------------------
regulations <- read_csv('../data/law_search/data.csv') %>% 
  clean_names() %>% 
  rename(year = year_passed) %>% 
  mutate(categories = categories %>% 
           str_to_lower() %>% 
           str_split(';')) 


# Analyze total number of regulations implemented after kyoto protocol between countries
year_start <- kyoto_start_year - (max(regulations$year) - kyoto_start_year)

regulations_compare <- regulations %>% 
  filter(year >= year_start) %>% 
  mutate(pre_kyoto = year < kyoto_start_year) %>% 
  group_by(country, pre_kyoto) %>% 
  summarise(number_regulations = n()) %>% 
  mutate(pre_kyoto = ifelse(pre_kyoto, 'pre_kyoto', 'post_kyoto')) %>% 
  pivot_wider(names_from=pre_kyoto, values_from=number_regulations) %>% 
  mutate(pre_kyoto = replace_na(pre_kyoto, 0), 
         post_kyoto = replace_na(post_kyoto, 0)) %>% 
  ungroup()
  

# Plot difference in regulations post vs pre kyoto protocol
regulations_compare %>% 
  mutate(regulations_diff = post_kyoto - pre_kyoto) %>% 
  mutate(country = fct_reorder(country, regulations_diff)) %>% 
  ggplot(aes(x=regulations_diff)) +
  geom_histogram(bins=30) +
  labs(
    title = 'Difference in number of regulations created before vs after the Kyoto Protocol',
    x = 'Difference in number of regulations'
  ) +
  theme_minimal()


# avg_regulations <- regulations %>% 
#   mutate(pre_kyoto = year < kyoto_protocol_effective_years['start']) %>% 
#   group_by(country, pre_kyoto) %>% 
#   summarise(avg_yearly_regulations = n() / abs(kyoto_protocol_effective_years['start'] - 
#                                                ifelse(pre_kyoto[1], 2005-14, max(regulations$year)))) %>% 
#   mutate(pre_kyoto = ifelse(pre_kyoto, 'pre_kyoto', 'post_kyoto')) %>% 
#   mutate(avg_yearly_regulations = replace(avg_yearly_regulations, ))
#   pivot_wider(names_from=pre_kyoto, values_from=avg_yearly_regulations) %>% 
#   mutate(avg_yearly_reg_diff = post_kyoto - pre_kyoto)
# 
# avg_regulations %>% 
#   inner_join(country_top_total_emissions %>% select(country), by='country') %>% 
#   ggplot(aes(x=country, y=avg_yearly_reg_diff)) +
#   geom_bar(stat='identity') +
#   coord_flip()












# Other Work --------------------------------------------------------------













# ## Intervention analysis.
# 
# # Decreasing acf indicated trend, no seasonility patterns obvious. Seems to match what the plot above shows
# ggAcf(world_ghg_emissions_ts) +
#   labs(title = 'World GHG emissions ACF')
# 
# # Arima on time period before kyoto protocol effective date. ARIMA(0,2,0) meaning that after differencing twice
# # we get a random walk to model our data
# world_ghg_arima <- auto.arima(world_ghg_emissions_ts %>% window(end=kyoto_protocol_effective_date['start'] %>% year() - 1))
# 
# # No obvious patterns in ACF, residuals seem almost normal. Residuals also look like white noise, with p-value=0.4309
# # Thus we can confirm that the residuals are independent. As well based on the qq-plot we may assume normality
# world_ghg_arima %>% checkresiduals()
# world_ghg_arima %>%
#   residuals() %>%
#   as_tsibble() %>%
#   ggplot(aes(sample=value)) +
#   stat_qq() + stat_qq_line()
# 
# # Set h based on kyoto protocol effective dates
# forecasts <- forecast(world_ghg_arima, h=kyoto_protocol_length)
# 
# # Plot actual with forecasts
# p +
#   geom_line(data=forecasts$mean, colour='red') +
#   geom_point(data=forecasts$mean, colour='red')
# 
# 
# # Check how well the forecasts compare to the actual values. We see that the errors grow over time, possibly indicating that
# # the time series changed after the event and decreased over time
# forecasts_errors <- world_ghg_emissions_ts - forecasts$mean
# 
# ggAcf(forecasts_errors)
# 
# forecasts_errors %>% autoplot() +
#   labs(
#     title = 'Forecast errors',
#     x = 'Year',
#     y = 'GHG emissions (megatonnes of co2 equivalent)'
#   )


# Kyoto Accord per country ------------------------------------------------
country_top_avg_emission_ts <- country_top_avg_emissions %>%
  select(country) %>%
  inner_join(country_ghg_emissions, by='country') %>%
  rename(total_ghg_emissions = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>%
  select(country, year, total_ghg_emissions) %>%
  as_tsibble(key=country, index=year)

# Plot emissions per country
kyoto_accord_per_country_plot <- country_top_avg_emission_ts %>%
  mutate(country = fct_reorder(country, total_ghg_emissions, tail, n=1, .desc=TRUE)) %>%
  forecast::autoplot(total_ghg_emissions) +
  geom_vline(xintercept=kyoto_protocol_effective_date['start'] %>% year(), linetype='dashed') +
  geom_vline(xintercept=kyoto_protocol_effective_date['end'] %>% year(), linetype='dashed') +
  labs(
    title = 'GHG emissions from countries making up top 80% of ghg emissions globally',
    x = 'Year',
    y = 'GHG emissions (megatonnes of co2 equivalent)'
  )
ggplotly(kyoto_accord_per_country_plot)

# Model arima for all countries, on only pre intervention period
country_top_avg_emissions_arima <- country_top_avg_emission_ts %>%
  filter(year < kyoto_protocol_effective_date['start'] %>% year()) %>%
  model(arima = ARIMA(total_ghg_emissions))

# TODO: ACF for original data
# TODO: Check residuals for all arima models, and check normality

# Forecasts post intervention period and get errors
country_top_avg_emissions_forecasts <- country_top_avg_emissions_arima %>%
  fabletools::forecast(h = paste(kyoto_protocol_length, 'years')) %>%
  rename(total_ghg_emissions_forecast = total_ghg_emissions) %>%
  inner_join(country_top_avg_emission_ts, by=c('country', 'year')) %>%
  mutate(forecast_error = total_ghg_emissions - total_ghg_emissions_forecast)

# Plot the forecast errors
forecast_errors_plot <- country_top_avg_emissions_forecasts %>%
  select(country, year, forecast_error) %>%
  mutate(country = fct_reorder(country, forecast_error, tail, n=1, .desc=TRUE)) %>%
  forecast::autoplot(forecast_error) +
  labs(
    title = 'ARIMA forecast errors for post intervention period per country',
    x = 'Year',
    y = 'error (megatonnes of co2 equivalent)'
  )
ggplotly(forecast_errors_plot)






# Effects of Regulations --------------------------------------------------

"Let's go through each country of India, China, US, Canada. Individually
Assumptions, the categories and sectored described by the different files refer to the same set of activities"

# TODO: Figure out way to cleanly store this as a tibble
regulations <- read_csv('../data/law_search/data.csv') %>% 
  clean_names() %>% 
  rename(year = year_passed) %>% 
  mutate(categories = categories %>% 
           str_to_lower() %>% 
           str_replace_all('energy supply|energy demand', 'energy') %>% 
           str_replace_all('; ', ';') %>% 
           str_split(';')) %>% 
  mutate(categories = map(categories, unique))
  
regulation_category_types <- regulations %>% 
  select(categories) %>% 
  unlist() %>% 
  str_trim() %>% 
  unique()


# # Common categories among regulations and ghg emissions per country
# common_categories <-
#   tribble(
#     ~regulation_categories, ~country_ghg_categories,
#     'REDD+ and LULUCF', 'land use change and forestry mt co2e',
#     'Energy Demand and Energy Supply', 'energy_mt_co2e',
#     'Industry', 'industrical_processes_mt_co2e',
#     'Transportation', 'transportation_mt_co2 (also part of energy)',
#   )

# Effects of Regulations - Canada -----------------------------------------

'Within the remit of this law, the Government announced the introduction of the clean fuel standard regulatory framework to 
achieve 30 megatonnes of annual reductions in GHG emissions by 2030, contributing to Canada’s effort to achieve its overall
GHG mitigation target of 30% emission reduction below 2005 levels by 2030.

We can check to see if Canada did even meet its goals'

# Data from Stats Can, most likely the best data for Canada GHG Emissions
# TODO: find a way to replace mt_co2e in all column names
canada_ghg_emissions <- read_csv('../data/GHG-emissions-sector-en.csv', skip=2) %>% 
  clean_names() %>% 
  filter(!str_detect(year, 'Note|Source|Available')) %>% 
  mutate(year = as.integer(year)) %>% 
  pivot_longer(cols=-year, names_to='category', values_to='mt_co2e') %>% 
  as_tsibble(key=category, index=year) %>% 
  fill_gaps(.full=TRUE)

# NOTE: Oil and gas is equivalent to carbon pricing?
canada_category_mappings <- 
  tribble(
    ~canada_category, ~regulation_category,
    'oil_and_gas_megatonnes_of_carbon_dioxide_equivalent', 'carbon pricing',
    'transportation_megatonnes_of_carbon_dioxide_equivalent', 'transportation',
    'buildings_megatonnes_of_carbon_dioxide_equivalent', 'buildings',
    'electricity_megatonnes_of_carbon_dioxide_equivalent', 'energy',
    'heavy_industry_megatonnes_of_carbon_dioxide_equivalent', 'industry',
    'waste_and_others_megatonnes_of_carbon_dioxide_equivalent', 'waste'
  )

# Canada regulations and their corresponding mtco2 values for that given year
canada_regulations <- regulations %>% 
  filter(country == 'Canada') %>% 
  unnest_longer(categories) %>% 
  left_join(canada_category_mappings, by=c('categories' = 'regulation_category')) %>% 
  left_join(canada_ghg_emissions, by=c('year' = 'year', 'canada_category' = 'category'))


# Display time series of canada ghg emission per subsector, along with corresponding regulations
p <- canada_ghg_emissions %>% 
  mutate(category = str_remove(category, '_megatonnes_of_carbon_dioxide_equivalent')) %>% 
  mutate(category = str_replace_all(category, '_', ' ')) %>% 
  ggplot(aes(x=year, y=mt_co2e)) +
  geom_line(aes(colour=category)) +
  geom_point(
    data = canada_regulations,
    aes(x = year, y = mt_co2e)
  ) +
  # geom_text_repel(
  #   data = canada_regulations,
  #   aes(x = year, y = mt_co2e, label = name),
  #   size = 2
  # ) +
  labs(
    title = 'Canada GHG emissions per subsector',
    x = 'Year',
    y = 'GHG emissions (megatonnes of CO2 equivalent)',
    colour = 'Category'
  )
ggplotly(p)


"Most of the regulations focus on electricity, transportation and oil and gas"


# Canada - Electricity ----------------------------------------------------
canada_electricity_ghg <- canada_ghg_emissions %>% 
  filter(str_detect(category, 'electricity')) %>% 
  select(-category)

# Ignoring certain regulations based on what they actually do. The ones below simply establish some agency 
canada_regulations_ignore <- c("Canada Foundation for Sustainable Development Technology Act (S.C. 2001, c. 23)", 
                               "Canada Emission Reduction Incentives Agency Act (S.C. 2005, c. 30, s. 87)")

# Filter for energy regulations and only for those that matter
canada_energy_regulations <- canada_regulations %>% 
  filter(categories == 'energy', !(name %in% electricity_regulations_ignore)) %>% 
  arrange(year)

# Plot ghg emission with regulation names
canada_electricity_ghg %>% 
  ggplot(aes(x=year, y=mt_co2e)) +
  geom_line() +
  geom_point(
    data = canada_energy_regulations,
    aes(x=year, y=mt_co2e)
  ) +
  geom_text_repel(
    data = canada_energy_regulations,
    aes(x=year, y=mt_co2e, label=name)
  ) +
  labs(
    title = 'Canada GHG emission from electricity and corresponding regulations',
    x = 'Year',
    y = 'GHG emission (megatonnes of CO2 equivalent)'
  ) +
  theme_minimal()

# Check acf, decreasing trend thus there is just a trend and no seasonality from the data 
# NOTE: Might need more data, or enough data to capture seasonality. However, based on what we know, there doesn't seem like
# there would be any seasonality. I would expect to see it across 12 months however our data is yearly
acf(canada_electricity_ghg)


# References https://newonlinecourses.science.psu.edu/stat510/lesson/9/9.2
# For the Intervention analysis we will assume that the regulation/policy was enabled that given year, and also
# The response is immediate
electriciy_arima_1 <- canada_electricity_ghg %>% 
  filter(year < canada_energy_regulations$year[1]) %>% 
  as_tsibble(index=year) %>% 
  model(arima = ARIMA(mt_co2e))

















