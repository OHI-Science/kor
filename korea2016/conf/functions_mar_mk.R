MAR <- function(layers){

  ## read in layers
  harvest_tonnes <- layers$data$mar_harvest_tonnes %>%
#    select(region_id = rgn_id, taxa_code, year, tonnes)
     select(region_id = rgn_id, category, name, year, tonnes)
#  sustainability_score <- layers$data$mar_sustainability_score %>%
#   select(region_id = rgn_id, taxa_code, sust_coeff)

#  popn_inland25mi <- layers$data$mar_coastalpopn_inland25mi %>%
#    select(region_id = rgn_id, year, popsum) %>%
#    mutate(popsum = popsum + 1)  # so 0 values do not cause errors when logged
#    mutate(popsum = popsum) # mk: all popsum values are 1 in current test file
  mar_area <- layers$data$mar_area %>%
    select(region_id = rgn_id, year, category, area)
  mar_area2 <- layers$data$mar_area2 %>%
    select(region_id = rgn_id, year, category, area)

  ## set data year for assessment
  data_year <- 2016

  ## combine layers
#  rky <-  harvest_tonnes %>%
#    left_join(sustainability_score, by = c('region_id', 'taxa_code'))
  rky <- harvest_tonnes

  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", 4:11) # ncol(rky)


  # 4-year rolling mean of data
  m <- rky %>%
    mutate(year = as.numeric(as.character(year))) %>%
#    group_by(region_id, taxa_code, sust_coeff) %>%
#    arrange(region_id, taxa_code, year) %>%
     group_by(region_id, name) %>%
     arrange(region_id, name, year) %>%
    mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()

  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
#    mutate(sust_tonnes = sust_coeff * sm_tonnes)
    mutate(sust_tonnes = sm_tonnes)

  # calculate license area sum
  area = mar_area %>%
    group_by(region_id, year) %>%
    summarize(area_sum = sum(area, na.rm = TRUE)) %>%
    ungroup()

  # aggregate all weighted timeseries per region, and divide by area
  ry = m %>%
    group_by(region_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>% #na.rm = TRUE assumes that NA values are 0
#    left_join(mar_area2, by = c('region_id','year', 'category')) %>%
    inner_join(area, by = c('region_id','year')) %>%
    mutate(mar_per_area = sust_tonnes_sum / area_sum) %>%
    ungroup()


  # get reference quantile based on argument years
  ref_95pct_data <- ry %>%
    filter(year <= data_year)

  ref_95pct <- quantile(ref_95pct_data$mar_per_area, 0.95, na.rm=TRUE)

  # identify reference region_id
  ry_ref = ref_95pct_data %>%
    arrange(mar_per_area) %>%
    filter(mar_per_area >= ref_95pct)
  message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct))
  message(sprintf('95th percentile region_id for MAR ref pt is: %s\n', ry_ref$region_id[1]))

  ry = ry %>%
    mutate(status = ifelse(mar_per_area / ref_95pct > 1,
                           1,
                           mar_per_area / ref_95pct))
  status <- ry %>%
    filter(year == data_year) %>%
    select(region_id, status) %>%
    mutate(status = round(status*100, 2))

#  trend_years <- (data_year-4):(data_year)
  trend_years <- (data_year-5):(data_year)
  first_trend_year <- min(trend_years)

  # get MAR trend
  trend = ry %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(mar_per_area)) %>% #why do this?
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
