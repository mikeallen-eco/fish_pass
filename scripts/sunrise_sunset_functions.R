read_nonleap <- function(year){
  # year <- 2015
  
  file_name <- paste0("sun", as.character(year), ".txt")
  
  sun_file1 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 9,
                          nrows = 28
  ) %>%
    select(1,8:15) %>%
    rename(day = 1,
           apr.sr = 2,
           apr.ss = 3,
           may.sr = 4,
           may.ss = 5,
           jun.sr = 6,
           jun.ss = 7,
           jul.sr = 8,
           jul.ss = 9
    )
  
  sun_file2 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 37,
                          nrows = 2
  ) %>%
    select(1,6:13) %>%
    rename(day = 1,
           apr.sr = 2,
           apr.ss = 3,
           may.sr = 4,
           may.ss = 5,
           jun.sr = 6,
           jun.ss = 7,
           jul.sr = 8,
           jul.ss = 9
    )
  
  sun_file3 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 39,
                          nrows = 1
  ) %>%
    select(1,6:9) %>%
    rename(day = 1,
           may.sr = 2,
           may.ss = 3,
           jul.sr = 4,
           jul.ss = 5
    )
  
  sun_file <- sun_file1 %>%
    bind_rows(sun_file2) %>%
    bind_rows(sun_file3) %>%
    pivot_longer(cols = apr.sr:jul.ss, 
                 values_to = "time") %>%
    mutate(month = substr(name, 1, 3),
           type = substr(name, 5, 7)) %>%
    pivot_wider(id_cols = c(month, day),
                values_from = time,
                names_from = type) %>%
    filter(is.na(sr)==F) %>%
    mutate(sr.h = as.numeric(substr(sr, 1,1)),
           sr.m = as.numeric(substr(sr, 2,3)),
           ss.h = as.numeric(substr(ss, 1,2)),
           ss.m = as.numeric(substr(ss, 3,4)),
           year = year,
           mo = case_when(month == "apr" ~ 4,
                          month == "may" ~ 5,
                          month == "jun" ~ 6,
                          month == "jul" ~ 7),
           datetime.sr = ymd_hm(paste0(year,"-",mo, "-", day, " ", 
                                       sr.h,":",sr.m)),
           datetime.ss = ymd_hm(paste0(year,"-",mo, "-", day, " ", 
                                       ss.h,":",ss.m)),
           ord = yday(datetime.sr)) %>%
    arrange(datetime.sr)
  
  return(sun_file)
  
}

read_leap <- function(year){
  # year <- 2016
  
  file_name <- paste0("sun", as.character(year), ".txt")
  
  sun_file1 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 9,
                          nrows = 29
  ) %>%
    select(1,8:15) %>%
    rename(day = 1,
           apr.sr = 2,
           apr.ss = 3,
           may.sr = 4,
           may.ss = 5,
           jun.sr = 6,
           jun.ss = 7,
           jul.sr = 8,
           jul.ss = 9
    )
  
  sun_file2 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 38,
                          nrows = 1
  ) %>%
    select(1,6:13) %>%
    rename(day = 1,
           apr.sr = 2,
           apr.ss = 3,
           may.sr = 4,
           may.ss = 5,
           jun.sr = 6,
           jun.ss = 7,
           jul.sr = 8,
           jul.ss = 9
    )
  
  sun_file3 <- read.table(here("data",
                               "sun",
                               file_name), 
                          skip = 39,
                          nrows = 1
  ) %>%
    select(1,6:9) %>%
    rename(day = 1,
           may.sr = 2,
           may.ss = 3,
           jul.sr = 4,
           jul.ss = 5
    )
  
  sun_file <- sun_file1 %>%
    bind_rows(sun_file2) %>%
    bind_rows(sun_file3) %>%
    pivot_longer(cols = apr.sr:jul.ss, 
                 values_to = "time") %>%
    mutate(month = substr(name, 1, 3),
           type = substr(name, 5, 7)) %>%
    pivot_wider(id_cols = c(month, day),
                values_from = time,
                names_from = type) %>%
    filter(is.na(sr)==F) %>%
    mutate(sr.h = as.numeric(substr(sr, 1,1)),
           sr.m = as.numeric(substr(sr, 2,3)),
           ss.h = as.numeric(substr(ss, 1,2)),
           ss.m = as.numeric(substr(ss, 3,4)),
           year = year,
           mo = case_when(month == "apr" ~ 4,
                          month == "may" ~ 5,
                          month == "jun" ~ 6,
                          month == "jul" ~ 7),
           datetime.sr = ymd_hm(paste0(year,"-",mo, "-", day, " ", 
                                       sr.h,":",sr.m)),
           datetime.ss = ymd_hm(paste0(year,"-",mo, "-", day, " ", 
                                       ss.h,":",ss.m)),
           ord = yday(datetime.sr)) %>%
    arrange(datetime.sr)
  
  return(sun_file)
  
}