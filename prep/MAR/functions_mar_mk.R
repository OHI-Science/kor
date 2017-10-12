MAR <- function(layers){

  ## read in layers
  harvest_tonnes <- layers$data$mar_harvest_tonnes %>%
     select(region_id = rgn_id, category, name, year, tonnes)

  sustainability_score <- layers$data$mar_sustainability_score %>%
   select(region_id = rgn_id, name, sust_coeff)

  popn <- layers$data$mar_coastalpopn %>%
    select(region_id = rgn_id, year, pop_coast)

  ## set data year for assessment
  data_year <- 2016

  ## combine layers
  rky <-  harvest_tonnes %>%
    left_join(sustainability_score, by = c('region_id', 'name'))

  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", 5:12) # ncol(rky)

  # 4-year rolling mean of data (mk) why do this?
  m <- rky %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(region_id, name, sust_coeff) %>%
    arrange(region_id, name, year) %>%
    mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()

  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
    mutate(sust_tonnes = sust_coeff * sm_tonnes)     # without log
#    mutate(sust_tonnes = log(sust_coeff * sm_tonnes)) #log

  # aggregate all weighted timeseries per region, and divide by population
  ry = m %>%
    group_by(region_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>% #na.rm = TRUE assumes that NA values are 0
    left_join(popn, by = c('region_id','year')) %>%
#    mutate(mar_per_pop = sust_tonnes_sum / pop_coast * 1000) %>% # *1000 because the result numbers are too little
#    mutate(mar_per_pop = log(sust_tonnes_sum / pop_coast * 1000)) %>% # log
    mutate(mar_per_pop = log10(sust_tonnes_sum / pop_coast * 1000 +1)) %>%
    ungroup()

  # get reference quantile based on argument years
  ref_95pct_data <- ry %>%
   filter(year <= data_year)

  ref_95pct <- quantile(ref_95pct_data$mar_per_pop, 0.95, na.rm=TRUE)

  # identify reference region_id
  ry_ref = ref_95pct_data %>%
    arrange(mar_per_pop) %>%
    filter(mar_per_pop >= ref_95pct)
  message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct))
  message(sprintf('95th percentile region_id for MAR ref pt is: %s\n', ry_ref$region_id[1]))

  ry = ry %>%
    mutate(status = ifelse(mar_per_pop / ref_95pct > 1,
                           1,
                           mar_per_pop / ref_95pct))
  status <- ry %>%
    filter(year == data_year) %>%
    select(region_id, status) %>%
    mutate(status = round(status*100, 2))

#  trend_years <- (data_year-4):(data_year)
  trend_years <- (data_year-7):(data_year)
  first_trend_year <- min(trend_years)

  # get MAR trend
  trend = ry %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(mar_per_pop)) %>% #why do this?
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup()

  trend <- trend %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id = region_id, score = trend) %>%
    mutate(dimension = "trend")

  # return scores
  scores = status %>%
    select(region_id, score = status) %>%
    mutate(dimension='status') %>%
    rbind(trend) %>%
    mutate(goal='MAR')

  return(scores)
}
