# Wrapping the installation code in "require" only installs package when it is not yet installed.
# If pacman package already exists in the environment, it just loads it like library() does

if (!require(pacman)) install.packages("pacman")  

# Manual installation required
if(!require(remotes)) install.packages("remotes")
if(!require(ethiopianDate)) remotes::install_github("d-callan/ethiopianDate")

# Loading other packages using pacman
pacman::p_load(tidyverse, readxl, import, here, janitor, rio, magrittr, magrittr,
               stringdist, stringr, openxlsx, lubridate, reshape, RColorBrewer, ggsci, stringr,
               sp, tmap, spdep, sf, scales, ethiopianDate)


# function to abbreviate long variable names
abbr_var_names <- function(x){
  purrr::map(x, function(y){
    y %>% unlist() %>% str_split("_") %>%
      lapply(abbreviate) %>%
      unlist() %>%
      paste0(collapse="_") %>%
      str_replace_all("_of_", "_")} %>%
      str_replace_all("_cass", "") %>%
      str_replace_all("_case", "") %>%
      str_replace_all("no_", "n_")
  ) %>% unlist()
}


# Process sys time stamp into a string
timestamp <- function(){
  Sys.time() %>% 
    str_sub(1, 19) %>% 
    str_remove_all("-") %>% 
    str_remove_all(":") %>% 
    str_replace_all(" ", "_")
  
  
}

# create a filename with timestamp
add_t_to_filename <- function(filename){
  temp <- str_split(filename, "\\.") %>% unlist()
  stmp <- timestamp()
  paste0(temp[1], "_", stmp, ".", temp[2])
}

# see if the codebook exists in certain directory
codebook_exists <- function(dir){
  files <- list.files(dir)
  ifelse(lapply(files, str_detect, "codebook") %>% unlist() %>% sum()>0, T, F)
}

# save/overwrite the existing file and archive it in the archive folder
save_and_archive<- function(dir, dat, filename){
  name <- add_t_to_filename(filename)
  rio::export(dat, here::here(dir, "archive", name))
  rio::export(dat, here::here(dir, filename))
}


# Map the correct name from the dictionary and reference file to raw data
clean_data <- function(dat, woreda_dic, geo_ref){
  
  # Mp the clean woreda name from the dictionary to the raw data
  dat_clean <- left_join(dat, woreda_dic, by = c("region", "zone", "woreda"))
  
  # Map the correct region and zone name from the reference file to the raw data
  dat_clean2 <- dat_clean %>% mutate(
    woreda_l = tolower(gname)
  )
  
  geo_ref2 <- geo_ref %>% mutate(
    woreda_l = tolower(woreda)
  )
  
  dat_clean2 <- left_join(dat_clean2, geo_ref2, by = "woreda_l")
  
  dat_clean2 %<>% mutate(
    woreda = ifelse(is.na(gname), woreda.x, woreda.y),
    zone = ifelse(is.na(gname), zone.x, zone.y),
    region = ifelse(is.na(gname), region.x, region.y)
  )
  
  dat_clean3 <- dat_clean2 %>% dplyr::select(-woreda.x, -woreda.y, -zone.x, -zone.y, 
                                      -region.x, -region.y, -woreda_l, -gname) %>%
    filter(!is.na(region) & !is.na(zone) & !is.na(woreda))
  
  return(dat_clean3)
}

# Load and archive the current version of dictionary
load_dictionary <- function(){
woreda_dic <- rio::import(here::here("dictionary", "new_dictionary.csv"))
rio::export(woreda_dic, here::here("dictionary", "archive", add_t_to_filename("archived_dictionary.csv")))

return(woreda_dic)
}


# Load the existing dictionary (and archive it)
load_new_dic <- function(){
  woreda_dic <- rio::import(here::here("to be cleaned", "clean_this_file.csv"))
  rio::export(woreda_dic, here::here("to be cleaned", "cleaned", add_t_to_filename("cleaned.csv")))
  
  woreda_dic %<>% dplyr::select(region, zone, woreda, gname)
  return(woreda_dic)
}

# Aggregate the clean data
aggregate_data <- function(dat_clean, old_template = F){
  
  if(old_template){
    dat_clean_aggr <- dat_clean %>% dplyr::select(-month,
                                                  -date_rcvd,
                                                  
                                                  -tmln,
                                                  -date_wkxp,
                                                  -ethm,
                                                  -ethy
    )  
  }else{
    dat_clean_aggr <- dat_clean %>% dplyr::select(-month,
                                         -date_rcvd,
                                         -date_week_expc,
                                         -tmln
    )
  }
  
  dat_clean_aggr %<>%
    group_by(region,
             zone,
             woreda,
             ADM3_PCODE,
             year,
             epi_week
            ) %>%
    summarise(across(where(is.numeric), sum, .names = "{.col}"), .groups = "drop")
  
  if(old_template){
    colnames(dat_clean_aggr)[7] <- "totl_malr"
  }
  return(dat_clean_aggr)
}

# Validate the aggregated data vs. all woredas in the reference shapefile
validate_data <- function(dat_clean_aggr, geo_ref){
  
  # What are the woredas that doesn't have any data mapped
  woreda_wo_data <- geo_ref$woreda[!geo_ref$woreda %in% unique(dat_clean_aggr$woreda)]
  print(sprintf("These are the %.0f woredas that do not have any data mapped:", length(woreda_wo_data)))
  print(geo_ref %>% filter(woreda %in% woreda_wo_data))
}

# Log the aggregated data
log_data <- function(dat_clean_aggr){
  log <- table(dat_clean_aggr$woreda, dat_clean_aggr$year)
  log %<>% as.data.frame() %>% spread(key = "Var2", value = "Freq")
  colnames(log)[1] <- "woreda"
  log %<>% left_join(geo_ref, by = "woreda")
  log %<>% dplyr::select("region", "zone", "woreda", where(is.numeric))
  return(log)
}

# Load the existing clean data
load_clean_data <- function(){
  path <- here::here("clean data", "clean_dat.csv")
  if (file.exists(path)) {
    rio::import(path, guess_max=10000)
  } else {
    print("There is no existing clean data.")
  }
}

# Map the date from year-week combo
week_to_date <- function(year, week, last=T){
  
  week <- as.numeric(week)-1
  
  # first day of the US week
  date <- as.Date(paste0(year,ifelse(week>=10, week, paste0("0", week)), "0"), "%Y%U%w")
  
  # When last is TRUE, it will create a date of the last day of the week (Sat)
  if(last){ 
    date <- date+6
    
  }
  
  return(date)
}

add_dates <- function(dat_aggr_clean, last=T){
  dat_aggr_clean %<>% mutate(
    date = week_to_date(year, epi_week, last)
  ) 
  return(dat_aggr_clean)
}

add_eth_dates <- function(dat, date_var){
  dat %>% rowwise() %>%mutate(
    eth_date = toEthiopian(!!sym(date_var)),
    eth_year = format(!!sym(date_var), "%Y"),
    eth_month = format(!!sym(date_var), "%m"),
    eth_week = format(!!sym(date_var), "%U")
  )
}

# Querying & extracting function
subset_phem <- function(dat,
                        year_start = NaN,
                        year_end = NaN,
                        cat = NA,
                        vars = NA,
                        region_name = NA,
                        zone_name = NA,
                        woreda_name = NA){
  
  if(is.na(year_start)) year_start <- min(dat$year, na.rm=T)
  if(is.na(year_end)) year_end <- max(dat$year, na.rm=T)
  
  dat_sub <- dat %>% filter(year >=year_start, year <=year_end)
  
  
  if(!is.na(cat) & !is.na(var)){
    stop("You can subset the data using either cat or vars, not both.")
  }
  
  # category
  if(!is.na(cat)){
    # load the codebook
    
    # filter all the var names under cat
    
    # filter the data
    dat_sub %<>% filter()
  }
  
  # vars
  if(!is.na(vars)){
    dat_sub %<>% dplyr::select(vars)  
  }
  
  # geography
  
  if(sum(!is.na(region_name),!is.na(zone_name),!is.na(woreda_name))>1){
    stop("You can subset the data using either region, zone, or woreda name. Don't use more than one filter.")
  }
  
  
  if(!is.na(region_name)){
    dat_sub %<>% filter(tolower(region) %in% tolower(region_name))
  }
  
  if(!is.na(zone_name)){
    dat_sub %<>% filter(tolower(zone) %in% tolower(zone_name))
  }
  
  if(!is.na(woreda_name)){
    dat_sub %<>% filter(tolower(woreda) %in% tolower(woreda_name))
  }
  
  return(dat_sub)
}
