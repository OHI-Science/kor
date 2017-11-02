## functions.R.
## Each OHI goal model is a separate R function. The function name is the 2- or 3- letter code for each goal or subgoal; for example, FIS is the Fishing subgoal of Food Provision (FP).

## These models originate from Global OHI assessments (ohi-global), and should be tailored to represent the charastics, priorities, and data in your assessment area.

FIS <- function(layers){

  ## read in layers
  landings  <- layers$data$fis_landings_all %>%
    select(region_id = rgn_id, year, landings = landings_all)
  vessels  <- layers$data$fis_vessel %>%
    select(region_id = rgn_id, year, no_vessels)

  ## set data year for assessment
  data_year <- 2016

  ## combine layers
  flv <-  landings %>%
    left_join(vessels, by = c('region_id', 'year'))

  # fill in gaps with no data
  flv <- flv %>%
    arrange(region_id, year) %>%
    fill(no_vessels)

  # 4-year rolling mean of data
  m <- flv %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(region_id) %>%
    arrange(region_id, year) %>%
    mutate(sm_landings = zoo::rollapply(landings, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()

  # divide by no of vessels
  fv = m %>%
    # mutate(landings_per_vessel = log10(sm_landings / no_vessels * 1000 +1))
  # mutate(landings_per_vessel = (sm_landings / no_vessels * 1000 +1))
  mutate(landings_per_vessel = log10(sm_landings / no_vessels +1))

  # get reference quantile based on argument years
  ref_95pct_data_ <- fv %>%
    filter(year <= data_year)

  ref_95pct_ <- quantile(ref_95pct_data_$landings_per_vessel, 0.95, na.rm=TRUE)

  # identify reference region_id
  fv_ref = ref_95pct_data_ %>%
    arrange(landings_per_vessel) %>%
    filter(landings_per_vessel >= ref_95pct_)
  message(sprintf('95th percentile for FIS ref pt is: %s\n', ref_95pct_))
  message(sprintf('95th percentile region_id for FIS ref pt is: %s\n', fv_ref$region_id[1]))

  fv = fv %>%
    mutate(status = ifelse(landings_per_vessel / ref_95pct_ > 1,
                           1,
                           landings_per_vessel / ref_95pct_))
  status <- fv %>%
    filter(year == data_year) %>%
    select(region_id, status) %>%
    mutate(status = round(status*100, 2))

  trend_years <- (data_year-4):(data_year)
  first_trend_year <- min(trend_years)

  # get FIS trend
  trend = fv %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(landings_per_vessel)) %>%
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
    mutate(goal='FIS')

  return(scores)
}

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
    mutate(sust_tonnes = sust_coeff * sm_tonnes)

  # aggregate all weighted timeseries per region, and divide by population
  ry = m %>%
    group_by(region_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>% #na.rm = TRUE assumes that NA values are 0
    left_join(popn, by = c('region_id','year')) %>%
    #    mutate(mar_per_pop = sust_tonnes_sum / pop_coast * 1000) %>%
    #    mutate(mar_per_pop = log(sust_tonnes_sum / pop_coast * 1000)) %>% # log
    mutate(mar_per_pop = log10(sust_tonnes_sum / pop_coast * 1000 +1)) %>% # *1000 because the result numbers are too small
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

   trend_years <- (data_year-4):(data_year)
  # trend_years <- (data_year-7):(data_year)
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


FP = function(layers, scores){

  ## read in layers for weights
  w <-  layers$data$fp_wildcaught_weight %>%
    select(region_id = rgn_id, year, w_FIS = w_fis)

  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages due to potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}

LIV_ECO <- function(layers, subgoal){

  ## read in layers
  # le_gdp <- layers$data$le_gdp_krw  %>%
  #   select(region_id = rgn_id, year, gdp_krw)

  le_sum_total_output <- layers$data$le_sum_total_output  %>%
    select(region_id = rgn_id, year, sum_total_output)

  le_wages <- layers$data$le_wages %>%
    select(region_id = rgn_id, year, sector, wage_krw = wages)

  le_jobs <- layers$data$le_employed %>%
    select(region_id = rgn_id, year, sector, jobs = employed)

  le_jobs_wo <- layers$data$le_employed_wo_pay %>%
    select(region_id = rgn_id, year, sector, jobs_wo = employed_wo_pay)

  le_total_output <- layers$data$le_total_output %>%
    select(region_id = rgn_id, year, sector, rev = total_output)

  le_workforce_size <- layers$data$le_workforcesize %>%
    select(region_id = rgn_id, year, jobs_all)

  le_unemployment <- layers$data$le_unemployment %>%
    select(region_id = rgn_id, year, pct_unemployed = percent)

  # le_cpi <- layers$data$le_cpi %>%
  #   select(region_id = rgn_id, year, cpi = CPI)

  # multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('con', 'em', 'env', 'fism', 'pa', 'rd', 'sb', 'ser', 'tour','ts'),
                                'multiplier' = c(1, 1, 1, 2.141, 1, 1.88, 1, 1, 1, 1)) # no multiplers for tour (=1)
  # multipler for fism = (cf(1.582) + mar(2.7))/2 = 2.141, rd = wte(1.88)
  # multipliers_rev  = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(1.59, 1)) # not used because GDP data is not by sector

  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('region_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('region_id', 'year')) %>%
    # mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    mutate(jobs_adj = jobs_mult / employed) %>% #mk
    # adjust wages. mk. no adjusting again
    left_join(le_wages, by=c('region_id','year','sector')) %>%
    # left_join(le_cpi, by = c('region_id','year')) %>%  # mk
    mutate(wages_adj = wage_krw) %>%  # mk
    arrange(year, sector, region_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wages_adj))
  # aia/subcountry2014 crashing b/c no concurrent wage data, so adding this check
  if (nrow(liv_status)==0){
    liv_status = liv %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend = liv %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status = liv_status %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(region_id, year, sector) %>%
      # summarize across sectors
      group_by(region_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wages_adj, na.rm=T)) %>%
      group_by(region_id) %>%
      arrange(region_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        # score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
        score   = (x_jobs + x_wages) / 2 * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      select(region_id, score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_krw)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(region_id, year, sector) %>%
      group_by(region_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('region_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, region_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        region_id = region_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(region_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, region_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(region_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id,
        score)
  }


  # ECO calculations ----
  # eco = le_gdp %>%
  #   mutate(
  #     rev_adj = gdp_krw,
  #     sector = 'gdp') %>%
  #   # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
  #   dplyr::select(region_id, year, sector, rev_adj)

  eco = le_total_output %>%
    left_join(le_sum_total_output, by = c('region_id', 'year')) %>%
    # mutate(rev_adj = rev)
  mutate(rev_adj = rev / sum_total_output) #%>% # too small value
  # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
  # dplyr::select(region_id, year, sector, rev_adj)


  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(region_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(region_id, year) %>%
    group_by(region_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(region_id, year, sector) %>%
    group_by(region_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(region_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      region_id = region_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(region_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)

  ## read in layers (regional)
  # le_rgn_grdp <- layers$data$le_rgn_grdp  %>%
  #   select(region_id = rgn_id, year, grdp_krw)

  le_rgn_sum_total_output <- layers$data$le_rgn_sum_total_output  %>%
    select(region_id = rgn_id, year, sum_total_output)

  le_rgn_wages <- layers$data$le_rgn_wages %>%
    select(region_id = rgn_id, year, sector, wage_krw)

  le_rgn_jobs <- layers$data$le_rgn_employed %>%
    select(region_id = rgn_id, year, sector, jobs)

  le_rgn_jobs_wo <- layers$data$le_rgn_employed_wo_pay %>%
    select(region_id = rgn_id, year, sector, jobs_wo)

  le_rgn_total_output <- layers$data$le_rgn_total_output %>%
    select(region_id = rgn_id, year, sector, rev = total_output)

  le_rgn_workforce_size <- layers$data$le_rgn_workforcesize %>%
    select(region_id = rgn_id, year, jobs_all)

  le_rgn_unemployment <- layers$data$le_rgn_unemployment %>%
    select(region_id = rgn_id, year, pct_unemployed)

  # le_rgn_cpi <- layers$data$le_rgn_cpi %>%
  #   select(region_id = rgn_id, year, cpi = CPI)

  # calculate employment counts
  le_rgn_employed = le_rgn_workforce_size %>%
    left_join(le_rgn_unemployment, by = c('region_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  multipliers_jobs = data.frame('sector' = c('con', 'em', 'env', 'fism', 'pa', 'rd', 'sb', 'ser', 'tour','ts'),
                                'multiplier' = c(1, 1, 1, 2.141, 1, 1.88, 1, 1, 1, 1)) # no multiplers for tour (=1)

  liv_rgn =
    # adjust jobs
    le_rgn_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_rgn_employed, by= c('region_id', 'year')) %>%
    # mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    mutate(jobs_adj = jobs_mult / employed) %>% #mk
    # adjust wages. mk. no adjsting again
    left_join(le_rgn_wages, by=c('region_id','year','sector')) %>%
    # left_join(le_rgn_cpi, by = c('region_id', 'year')) %>%  # mk
    mutate(wages_adj = wage_krw) %>%  # mk
    arrange(year, sector, region_id)

  # LIV calculations (regional) ----

  # LIV status (regional)
  liv_status_rgn = liv_rgn %>%
    filter(!is.na(jobs_adj) & !is.na(wages_adj))
  # aia/subcountry2014 crashing b/c no concurrent wage data, so adding this check
  if (nrow(liv_status_rgn)==0){
    liv_status_rgn = liv_rgn %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend_rgn = liv_rgn %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status_rgn = liv_status_rgn %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(region_id, year, sector) %>%
      # summarize across sectors
      group_by(region_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wages_adj, na.rm=T)) %>%
      group_by(region_id) %>%
      arrange(region_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        # score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
        score   = (x_jobs + x_wages) / 2 * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      select(region_id, score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend_rgn = liv_rgn %>%
      filter(!is.na(jobs_adj) & !is.na(wage_krw)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(region_id, year, sector) %>%
      group_by(region_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('region_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, region_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        region_id = region_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(region_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, region_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(region_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id,
        score)
  }


  # ECO calculations (regional) ----

    eco_rgn = le_rgn_total_output %>%
    left_join(le_rgn_sum_total_output, by = c('region_id', 'year')) %>%
    # mutate(rev_adj = rev)
    mutate(rev_adj = rev / sum_total_output) #%>% # too small value.mk
  # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
  # dplyr::select(region_id, year, sector, rev_adj)


  # ECO status (regional)
  eco_status_rgn = eco_rgn %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(region_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(region_id, year) %>%
    group_by(region_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)

  # ECO trend (regional)
  eco_trend_rgn = eco_rgn %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(region_id, year, sector) %>%
    group_by(region_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(region_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      region_id = region_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(region_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)
  # copy status score to region 1 to 11
  liv_status_all <- data_frame(region_id = 1:11,
                               score = liv_status$score) %>%
    mutate(goal = "LIV",
           dimension = "status") %>%
    arrange(goal, dimension, region_id, score)

  eco_status_all <- data_frame(region_id = 1:11,
                               score = eco_status$score) %>%
    mutate(goal = "ECO",
           dimension = "status") %>%
    arrange(goal, dimension, region_id, score)

  # combine national and regional trend
  liv_trend_m <- data_frame(region_id = 1:11,
                               score = liv_trend$score) %>%
    mutate(goal = "LIV",
           dimension = "trend") %>%
    arrange(goal, dimension, region_id, score)

  liv_trend_all =  liv_trend_m %>%
    left_join(liv_trend_rgn, by =c("region_id", "goal", "dimension")) %>%
    mutate(score = (score.x + score.y) / 2) %>%
    dplyr::select(goal, dimension, region_id, score)

  eco_trend_m <- data_frame(region_id = 1:11,
                            score = eco_trend$score) %>%
    mutate(goal = "ECO",
           dimension = "trend") %>%
    arrange(goal, dimension, region_id, score)

  eco_trend_all =  eco_trend_m %>%
    left_join(eco_trend_rgn, by =c("region_id", "goal", "dimension")) %>%
    mutate(score = (score.x + score.y) / 2) %>%
    dplyr::select(goal, dimension, region_id, score)

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status_all, liv_trend_all)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status_all, eco_trend_all)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores_LE = scores %>%
    dplyr::filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(score = rowMeans(cbind(ECO, LIV), na.rm=TRUE)) %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores_LE)

  # return scores
  return(scores)
}

CW <- function(layers){

  ## read in layers
  pressure_lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash')
  trend_layers  <- c('cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')

  d_pressures <- rbind(layers$data$po_pathogens,
                       layers$data$po_nutrients_3nm,
                       layers$data$po_chemicals_3nm,
                       layers$data$po_trash) %>%
    select(region_id = rgn_id, year, pressure_score, layer)

  d_trends <- rbind(layers$data$cw_chemical_trend,
                    layers$data$cw_nutrient_trend,
                    layers$data$cw_trash_trend,
                    layers$data$cw_pathogen_trend) %>%
    select(region_id = rgn_id, year, trend, layer)

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  ## calculations
  d_pressures <- d_pressures %>%
    mutate(pressure = 1 - pressure_score) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()

  d_trends <- d_trends %>%
    mutate(trend = -1 * trend)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()


  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  return(scores)
}


HAB = function(layers){

  ## set data year for assessment
  data_year <- 2014

  ## read in layers
  extent_lyrs <- c('hab_seagrass_extent', 'hab_saltmarsh_extent', 'hab_softbottom_extent')
  health_lyrs <- c('hab_seagrass_health', 'hab_saltmarsh_health', 'hab_softbottom_health')
  trend_lyrs <- c('hab_seagrass_trend', 'hab_saltmarsh_trend', 'hab_softbottom_trend')

  # get data together. Note: SelectData2() is a function defined at the bottom of this script
  extent <- SelectData2(extent_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  health <- SelectData2(health_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- SelectData2(trend_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('region_id', 'habitat')) %>%
    full_join(extent, by = c('region_id', 'habitat')) %>%
    filter(habitat %in% c('saltmarsh','seagrass','soft_bottom')) %>%
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))

  if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
    warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  }

  if(sum(d$w %in% 1 & is.na(d$health)) > 0){
    warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  }


  ## calculate scores
  status <- d %>%
    group_by(region_id) %>%
    filter(!is.na(health)) %>%
    summarize(
      score = pmin(1, sum(health) / sum(w)) * 100,
      dimension = 'status') %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(status) == 0 ){
    status <- dplyr::bind_rows(
      status,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'status'))
  }

  trend <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%
    summarize(
      score =  sum(trend) / sum(w),
      dimension = 'trend')  %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(trend) == 0 ){
    trend <- dplyr::bind_rows(
      trend,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'trend'))
  }

  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score)

  ## create weights file for pressures/resilience calculations
  weights<- extent %>%
    filter(habitat %in% c('seagrass',
                          'saltmarsh',
                          'soft_bottom')) %>%
    filter(extent > 0) %>%
    mutate(boolean = 1) %>%
    mutate(layer = "element_wts_hab_pres_abs") %>%
    select(rgn_id=region_id, habitat, boolean, layer)

  layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(scores_HAB)
}

SPP = function(layers){

  ## read in layers and format
  scores <- rbind(layers$data$spp_status, layers$data$spp_trend) %>%
    mutate(goal = 'SPP') %>%
    mutate(dimension = ifelse(layer=="spp_status", "status", "trend")) %>%
    mutate(score = ifelse(dimension == 'status', score*100, score)) %>%
    select(region_id=rgn_id, goal, dimension, score)

  return(scores)
}

BD = function(scores){

  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))

  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop = TRUE)
  scores[scores$region_id==id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}


## Helper functions ----

# function to link data and scenario years based on
# conf/scenario_data_years.csv information

get_data_year <- function(layer_nm, layers=layers) { #layer_nm="le_wage_cur_base_value"

  all_years <- conf$scenario_data_years %>%
    mutate(scenario_year= as.numeric(scenario_year),
           data_year = as.numeric(data_year)) %>%
    filter(layer_name %in% layer_nm) %>%
    select(layer_name, scenario_year, year=data_year)


  layer_vals <- layers$data[[layer_nm]]

  layers_years <- all_years %>%
    left_join(layer_vals, by="year") %>%
    select(-layer)

  names(layers_years)[which(names(layers_years)=="year")] <- paste0(layer_nm, "_year")

  return(layers_years)
}


# useful function for compiling multiple data layers
# only works when the variable names are the same across datasets
# (e.g., coral, seagrass, and mangroves).
# it relies on get_data_year(), a function defined immediately above.
SelectData2 <- function(layer_names){
  data <- data.frame()
  for(e in layer_names){ # e="le_jobs_cur_base_value"
    data_new <- get_data_year(layer_nm=e, layers=layers)
    names(data_new)[which(names(data_new) == paste0(e, "_year"))] <- "data_year"
    data <- rbind(data, data_new)
  }
  return(data)
}

