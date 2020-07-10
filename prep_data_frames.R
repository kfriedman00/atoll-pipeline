library(rgdal)
library(ncdf4)
library(raster)
library(tidyverse)
library(fishualize)
library(gganimate)
library(ggplot2)
source("/Users/sriramsundararajan/Desktop/BSURP/ellipse_generation.R")

##You need to change the directory above and the path directories in the first function, that's it
path_to_wd <<- "/Users/sriramsundararajan/Desktop/BSURP/atolls/"
setwd(path_to_wd)

##Creates path names, directories, urls, file names, and downloads unprocessed data files (if they don't exist)
create_url_and_files <- function(server, sst_id, start_date, end_date, lat, long_in, long_out, atoll) {
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  if (!dir.exists(path_to_atoll) ) {dir.create(path_to_atoll)}
  if (!dir.exists(path_to_plots) ) {dir.create(path_to_plots)}
  if (!dir.exists(path_to_data) ) {dir.create(path_to_data)}

  inside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min, ")", ":1:", "(", lat$max, ")", "]", "[", "(", long_in$min, ")", ":1:", "(", long_in$max, ")","]" )
  outside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min, ")", ":1:", "(", lat$max, ")", "]", "[", "(", long_out$min, ")", ":1:", "(", long_out$max, ")","]" )
  inside_file <<- paste0(atoll, "/Data/", atoll, "_inside_sst.csv")
  outside_file <<- paste0(atoll, "/Data/", atoll, "_outside_sst.csv")
  
  if(!file.exists(inside_file)) curl::curl_download(inside_url, inside_file)
  if(!file.exists(outside_file)) curl::curl_download(outside_url, outside_file)
}

##"Fixes" data frames (column class types, etc), combines them, and runs the ellipse_generation function


# fix_combine_data_frames <- function(inside_file, outside_file, atoll, major1, major2, minor1, minor2) {
#   inside <- read.csv(inside_file, stringsAsFactors = FALSE)
#   outside <- read.csv(outside_file, stringsAsFactors = FALSE)
#   
#   #####
#   inside = inside[-1,]
#   inside$latitude = as.numeric(as.character(inside$latitude))
#   inside$longitude = as.numeric(as.character(inside$longitude))
#   inside$analysed_sst = as.numeric(as.character(inside$analysed_sst))
#   lapply(inside, class)
#   
#   
#   outside = outside[-1,]
#   outside$latitude = as.numeric(as.character(outside$latitude))
#   outside$longitude = as.numeric(as.character(outside$longitude))
#   outside$analysed_sst = as.numeric(as.character(outside$analysed_sst))
#   lapply(outside, class)
#   
#   
#   lapply(inside, class)
#   lapply(outside, class)
#   
#   
#   inside_ellipse <- ellipse_generation(atoll, inside, major1, major2, minor1, minor2)
#   outside_ellipse <- ellipse_generation(atoll, outside, major1 + shift, major2+shift, minor1+shift, minor2+shift)
#   
#   outside_ellipse  = outside %>% mutate(
#     location  = as.factor("O")
#   )
#   outside_ellipse = inside %>% mutate(
#     location = as.factor("I")
#   )
#   
#   ellipses <- rbind(inside_ellipse, outside_ellipse)
#   ellipses$time <- as.character(ellipses$time)
#   ellipses$date <- as.Date(sapply(ellipses$time, function(x) strsplit(x, "T")[[1]][1]))
#   
#   
#   ellipses$month <- lubridate::month(ellipses$date)
#   ellipses$year <- lubridate::year(ellipses$date)
#   
#   write.csv(ellipses, paste0(atoll, "/Data/", atoll, "_processed_data.csv"))
#   print("Ellipses (after ellipse generation):")
#   print(head(ellipses))
#   return(ellipses)
#}

fix_combine_data_frames <- function() {
inside <- read.csv(inside_file)
outside <- read.csv(outside_file)

#####
inside = inside[-1,]
inside$latitude = as.numeric(as.character(inside$latitude))
inside$longitude = as.numeric(as.character(inside$longitude))
inside$analysed_sst = as.numeric(as.character(inside$analysed_sst))
inside$time = as.character(inside$time)
inside$date <- as.Date(sapply(inside$time, function(x) strsplit(x, "T")[[1]][1]))
inside$month <- lubridate::month(inside$date)
inside$year <- lubridate::year(inside$date)
lapply(inside, class)


outside = outside[-1,]
outside$latitude = as.numeric(as.character(outside$latitude))
outside$longitude = as.numeric(as.character(outside$longitude))
outside$analysed_sst = as.numeric(as.character(outside$analysed_sst))
outside$time = as.character(outside$time)
outside$date <- as.Date(sapply(outside$time, function(x) strsplit(x, "T")[[1]][1]))
outside$month <- lubridate::month(outside$date)
outside$year <- lubridate::year(outside$date)
lapply(outside, class)

outside  = outside %>% mutate(
  location  = as.factor("O")
)
inside = inside %>% mutate(
  location = as.factor("I")
)
#####

lapply(inside, class)
lapply(outside, class)


Full <<- rbind(outside, inside)
Full$location = as.factor(Full$location)
write.csv(Full, paste0(atoll, "/Data/", atoll, "_full.csv"))
print("Full (before ellipse generation):")
print(head(Full))

inside_ellipse <- ellipse_generation(atoll, paste0(atoll, "/Data/", atoll, "_full.csv"), "I", major1, major2, minor1, minor2)
outside_ellipse <- ellipse_generation(atoll, paste0(atoll, "/Data/", atoll, "_full.csv"), "O", major1+shift, major2+shift, minor1+shift, minor2+shift)

#####
inside_ellipse = inside_ellipse[-1,]
inside_ellipse$latitude = as.numeric(as.character(inside_ellipse$latitude))
inside_ellipse$longitude = as.numeric(as.character(inside_ellipse$longitude))
inside_ellipse$analysed_sst = as.numeric(as.character(inside_ellipse$analysed_sst))
inside_ellipse$time = as.character(inside_ellipse$time)
inside_ellipse$date <- as.Date(sapply(inside_ellipse$time, function(x) strsplit(x, "T")[[1]][1]))
inside_ellipse$month <- lubridate::month(inside_ellipse$date)
inside_ellipse$year <- lubridate::year(inside_ellipse$date)
lapply(inside_ellipse, class)


outside_ellipse = outside_ellipse[-1,]
outside_ellipse$latitude = as.numeric(as.character(outside_ellipse$latitude))
outside_ellipse$longitude = as.numeric(as.character(outside_ellipse$longitude))
outside_ellipse$analysed_sst = as.numeric(as.character(outside_ellipse$analysed_sst))
outside_ellipse$time = as.character(outside_ellipse$time)
outside_ellipse$date <- as.Date(sapply(outside_ellipse$time, function(x) strsplit(x, "T")[[1]][1]))
outside_ellipse$month <- lubridate::month(outside_ellipse$date)
outside_ellipse$year <- lubridate::year(outside_ellipse$date)
lapply(outside_ellipse, class)

outside_ellipse  = outside_ellipse %>% mutate(
  location  = as.factor("O")
)
inside_ellipse = inside_ellipse %>% mutate(
  location = as.factor("I")
)
#####

ellipses <<- rbind(inside_ellipse, outside_ellipse)[,-1]
write.csv(ellipses, paste0(atoll, "/Data/", atoll, "_processed_data.csv"))
print("Ellipses (after ellipse generation):")
print(head(ellipses))
}

##Runs all of Sriram's data analysis, creates the plots, and spits them out into the plots folder as .png's
plots <- function(){
  #average temperature by plot over time
  #######
  
  temp_over_time1 <- ellipses %>%
    group_by(location, year, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    ggplot(aes(date, Mean, color = location, group = location)) +
    geom_point() + geom_smooth(se = F) +
    ggtitle("Temperature over Years") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    xlab("Date") + ylab("Location Average (Celcius)") 
  
  ggsave(file = paste0(path_to_plots, "/TempOverTime1.png"), temp_over_time1)
  
  temp_over_time2 <- ellipses %>%
    group_by(location, year, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    ggplot(aes(date, Mean, color = location, group = location)) +
    geom_point(alpha = 0.05) + geom_smooth(se = F) +
    ggtitle("Temperature over Years") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    xlab("Date") + ylab("Location Average (Celcius)") 
  
  ggsave(file = paste0(path_to_plots, "/TempOverTime2.png"), temp_over_time2)
  
  #days above average monthly maximum
  
  #______________________________________________________
  #percent days bleached stats
  #####
  inside <- filter(ellipses, location == "I")
  outside <- filter(ellipses, location == "O")
  #mean of hottest month per year
  #####
  inside_monthly = inside %>%
    group_by(year, month) %>%  
    summarise(
      monthly_avg = mean(analysed_sst)
    ) %>%
    filter(
      !duplicated(month)
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(
      monthly_avg == max(monthly_avg)
    ) %>%
    ungroup() %>%
    filter(
      year != 2020 #2020 isnt over :( 
    ) 
  
  outside_monthly = outside %>%
    group_by(year, month) %>%
    summarise(
      monthly_avg = mean(analysed_sst)
    ) %>%
    filter(
      !duplicated(month)
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(
      monthly_avg == max(monthly_avg)
    )   %>%
    ungroup() %>%
    filter(
      year != 2020
    )
  
  
  inside_max_mean_summer_monthly <<- mean(inside_monthly$monthly_avg)
  outside_max_mean_summer_monthly <<- mean(outside_monthly$monthly_avg)
  rm(inside_monthly)
  rm(outside_monthly)
  #####
  
  #how many days above mean monthly max as a whole area
  #degree days average
  #####
  inside_day = inside %>% group_by(
    date
  ) %>%
    mutate(
      bleached = mean(analysed_sst) >(inside_max_mean_summer_monthly + bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>%
    filter(
      !duplicated(date)
    )
  inside_day = inside_day[-c(1)]
  inside_day = inside_day[-c(3)]
  
  
  outside_day = outside %>% group_by(
    date
  ) %>%
    mutate(
      bleached = mean(analysed_sst) > (outside_max_mean_summer_monthly +bleaching_threshold_C), 
      mean_sst = mean(analysed_sst)
    ) %>%
    filter(
      !duplicated(date)
    ) 
  outside_day = outside_day[-c(1)]
  outside_day = outside_day[-c(3)]
  
  percent_days_bleach_inside <<- (sum(inside_day$bleached)*100)/nrow(inside_day)
  percent_days_bleach_outside <<- (sum(outside_day$bleached)*100)/nrow(outside_day)
  
  days_in_dataset <<- nrow(outside_day)
  
  #_______________________________________________________________________
  inside_day = inside_day %>%
    filter(
      mean_sst > (inside_max_mean_summer_monthly + degree_day_threshold)
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean_sst - inside_max_mean_summer_monthly
    ) %>%
    ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    group_by(consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy)
    ) %>%
    filter(
      !duplicated(consec)
    )
  
  
  degree_days_inside <<- sum(inside_day$degree_days)
  avg_run_inside <<-mean(inside_day$run_length)
  avg_degree_days_inside <<- mean(inside_day$degree_days)
  
  
  
  inside_day = inside_day%>% group_by(year) %>%
    mutate(
      degreedays_year = sum(degree_days),
      run_length_year = mean(run_length),
      avg_run_degree_days = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1 = sum(dummy)
    ) %>% ungroup() %>%
    group_by(year, month) %>%
    mutate(
      degreedays_month = sum(degree_days),
      run_length_month = mean(run_length),
      avg_run_degree_days_m = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1_m = sum(dummy)
    ) 
  

  n_multi_day_runs_inside<<- sum(inside_day$dummy)
  
  inside_day = inside_day[c("date","year", "month", "degreedays_year","run_length_year", "avg_run_degree_days", "n_runs_over_1" , 
                            "degreedays_month", "run_length_month", "avg_run_degree_days_m","n_runs_over_1_m","location")]
  
  inside_yearly <<- inside_day %>%
    group_by(year, month) %>%
    filter(
      !duplicated(month)
    )
  

  outside_day = outside_day %>%
    filter(
      mean_sst > (outside_max_mean_summer_monthly + degree_day_threshold)
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean_sst - outside_max_mean_summer_monthly
    ) %>%
    ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    group_by(consec) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy)
    ) %>%
    filter(
      !duplicated(consec)
    )
  
  
  degree_days_outside <<- sum(outside_day$degree_days)
  avg_run_outside <<-mean(outside_day$run_length)
  avg_degree_days_outside <<- mean(outside_day$degree_days)

  
  
  
  outside_day = outside_day%>% group_by(year) %>%
    mutate(
      degreedays_year = sum(degree_days),
      run_length_year = mean(run_length),
      avg_run_degree_days = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1 = sum(dummy)
    ) %>% ungroup() %>%
    group_by(year, month) %>%
    mutate(
      degreedays_month = sum(degree_days),
      run_length_month = mean(run_length),
      avg_run_degree_days_m = mean(degree_days),
      dummy = run_length >1, 
      n_runs_over_1_m = sum(dummy)
    ) 
  
  
  n_multi_day_runs_outside<<- sum(outside_day$dummy)
  
  outside_day = outside_day[c("date","year", "month", "degreedays_year","run_length_year", "avg_run_degree_days", "n_runs_over_1" , 
                            "degreedays_month", "run_length_month", "avg_run_degree_days_m","n_runs_over_1_m","location")]
  
  outside_yearly <<- outside_day %>%
    group_by(year, month) %>%
    filter(
      !duplicated(month)
    )
  
  rm(inside_day)
  rm(outside_day)
  
  #####
  
  
  
  
  
  
  #
  #point data
  #percent bleached over time
  ######
  #points bleached and percentage of total area
  inside = inside %>%
    mutate(
      point_bleach = analysed_sst> (inside_max_mean_summer_monthly + bleaching_threshold_C)
    )
  
 
  n_points = inside %>%
    group_by(year, month, date) %>%
    tally()
  n_points = n_points$n[2]
  
  
  inside = inside %>%
    group_by(year, month, date) %>%
    mutate(
      percent_bleach = sum(point_bleach)/n_points
    )
  
  rm(n_points)
  
  outside = outside %>%
    mutate(
      point_bleach = analysed_sst> outside_max_mean_summer_monthly +bleaching_threshold_C
    )
  
  n_points = outside %>%
    group_by(year, month, date) %>%
    tally()
  n_points = n_points$n[2]
  
  outside = outside %>%
    group_by(year, month, date) %>%
    mutate(
      percent_bleach = sum(point_bleach)/n_points
    )
  
  inside_day = inside %>%
    filter(
      !duplicated(date)
    )
  outside_day = outside %>%
    filter(
      !duplicated(date)
    )
  
  
  rm(n_points)
  rm(inside_day)
  rm(outside_day)
  
  
  #_________________________________________________________
  #degree days
  inside_degreeday <<- inside %>% 
    mutate(
      degreeabove = analysed_sst - inside_max_mean_summer_monthly
    ) %>%
    filter(
      degreeabove > degree_day_threshold
    )
  
  
  inside_degreeday = inside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    ungroup() %>%
    group_by(
      latitude, longitude, consec
    ) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy),
      dummy = run_length >1
    ) %>%
    ungroup() %>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(consec))%>% ungroup() %>%
    group_by(latitude, longitude, year) %>%
    mutate(
      degree_days_year = sum(degree_days),
      run_length_year = mean(run_length)
    )
  
  

    inside_degreeday  = inside_degreeday %>%
      group_by(latitude, longitude, year) %>%
      mutate(n_multiday_runs = sum(dummy))
    
    inside_degreeday =   inside_degreeday%>%
    group_by(latitude, longitude) %>%
    filter(
      !duplicated(year)
    )
  
  
  inside_degreeday = inside_degreeday[c("latitude","longitude","year","degree_days_year", "run_length_year", "n_multiday_runs")]
  
  outside_degreeday <<- outside %>% 
    mutate(
      degreeabove = analysed_sst - outside_max_mean_summer_monthly
    ) %>%
    filter(
      degreeabove > degree_day_threshold
    )
  
  
  outside_degreeday = outside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1)),
      dummy = T
    ) %>%
    ungroup() %>%
    group_by(
      latitude, longitude, consec
    ) %>%
    mutate(
      degree_days = sum(degreeabove),
      run_length = sum(dummy),
      dummy = run_length >1
    ) %>%
    ungroup() %>%
    group_by(latitude, longitude) %>%
    filter(!duplicated(consec))%>% ungroup() %>%
    group_by(latitude, longitude, year) %>%
    mutate(
      degree_days_year = sum(degree_days),
      run_length_year = mean(run_length)
    )
  
  
  
  outside_degreeday  = outside_degreeday %>%
    group_by(latitude, longitude, year) %>%
    mutate(n_multiday_runs = sum(dummy))
  
  outside_degreeday =   outside_degreeday%>%
    group_by(latitude, longitude) %>%
    filter(
      !duplicated(year)
    )
  
  
  outside_degreeday = outside_degreeday[c("latitude","longitude","year","degree_days_year", "run_length_year", "n_multiday_runs")]
  # test = outside_degreeday %>% filter(
  #   latitude ==16.77, longitude == -169.3, consec == 81
  # )
  
  ########
  
  #_________________________________
  
  #percent days bleached raster
  #####
  
  degree_days_outside_2016 <- outside_degreeday %>% 
    group_by(latitude, longitude) %>% 
    filter(year == 2016) %>%
    filter(!duplicated(year)) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = degree_days_year)) +
    #scale_fill_gradientn(colours=viridis::plasma(5)) +
    ggtitle("Degre Days Outside\n 2015") +  
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Degree Days") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  ggsave(file = paste0(path_to_plots, "/degree_days_outside_2016.png"), degree_days_outside_2016)
  
  degree_days_inside_2016 <- inside_degreeday %>% 
    group_by(latitude, longitude) %>% 
    filter(year == 2016) %>%
    filter(!duplicated(year)) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = degree_days_year)) +
    #scale_fill_gradientn(colours=viridis::plasma(5)) +
    ggtitle("Degre Days Inside\n 2015") +  
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Degree Days") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  
  ggsave(file = paste0(path_to_plots, "/degree_days_inside_2016.png"), degree_days_inside_2016)
  
  
  # tiff(filename=paste0(path_to_plots, "/bleachOutside.tiff"),
  #      res=100, width=6, height=4, units="in", compression="lzw")
  bleach_outside_graph <- outside %>%
    group_by(latitude, longitude) %>%
    summarise(time_bleached  = sum(point_bleach)*100/days_in_dataset) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = time_bleached)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    ggtitle("Percent Days Bleached Outside") +
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Percent Days \nBleached") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  #dev.off()
  
  ggsave(file = paste0(path_to_plots, "/bleach_outside.png"), bleach_outside_graph)

  # tiff(filename=paste0(path_to_plots, "/bleachInside.tiff"),
  #      res=100, width=6, height=4, units="in", compression="lzw")
  bleach_inside_graph <- inside %>%
    group_by(latitude, longitude) %>%
    summarise(time_bleached  = sum(point_bleach)*100/days_in_dataset) %>%
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = time_bleached)) +
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12)) +
    ggtitle("Percent Days Bleached Inside") +
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    ylab("Latitude") + xlab("Longitude") +
    labs(fill = "Percent Days \nBleached") +
    theme(legend.title = element_text(color = "red", hjust = 0.5))
  #dev.off()

  ggsave(file = paste0(path_to_plots, "/bleach_inside.png"), bleach_inside_graph)
  
  #####
  
  
  #_______________________________________________________
  #max ssst
  
  ########
  # tiff(filename=paste0(path_to_plots, "/maxTemp2015Outside.tiff"), 
  #      res=100, width=6, height=4, units="in", compression="lzw")
  max_sst_out <- outside %>% 
    group_by(latitude, longitude) %>%
    filter(year == 2015) %>%
    summarise(sst = max(analysed_sst)) %>% 
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = sst)) + 
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(29.9, 30.5)) +
    ggtitle("Sea Surface Temp \n2015 Outside") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    #dashed lines
    # theme(panel.grid.major = element_line(color = 'black', linetype = 'dashed'), panel.ontop = T,
    #       panel.grid.minor = element_blank(), panel.background = element_rect(fill = NA)) +
    labs(fill = "Max Sea Surface \nTemperature (Celcius)") +
    theme(legend.title = element_text(size = 8, colour = "red", hjust = 0.5))+
    ylab("Latitude") + xlab("Longitude")
  # dev.off()
  
  ggsave(file = paste0(path_to_plots, "/max_sst_out.png"), max_sst_out)
  
  # tiff(filename=paste0(path_to_plots, "/maxTemp2015Outside.tiff"), 
  #      res=100, width=6, height=4, units="in", compression="lzw")
  max_sst_in <- ellipses %>% 
    group_by(latitude, longitude) %>%
    filter(location == "I", year == 2015) %>%
    summarise(sst = max(analysed_sst)) %>% 
    ggplot(aes(longitude, latitude)) +
    geom_raster(aes(fill = sst)) + 
    #scale_fill_gradientn(colours=viridis::plasma(5), limits=c(29.9, 30.5)) +
    ggtitle("Sea Surface Temp \n2015 Inside") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) +
    #dashed lines
    #theme(panel.grid.major = element_line(color = 'black', linetype = 'dashed'), panel.ontop = T,
    #      panel.grid.minor = element_blank(), panel.background = element_rect(fill = NA)) +
    labs(fill = "Max Sea Surface \nTemperature (Celcius)") +
    theme(legend.title = element_text(size = 8, colour = "red", hjust = 0.5))+
    ylab("Latitude") + xlab("Longitude") +
    coord_cartesian(xlim = c(167.33, 167.63), ylim=c(9.02, 9.32))
  # dev.off()
  
  ggsave(file = paste0(path_to_plots, "/max_sst_in.png"), max_sst_in)
  ########
  
  overall_values <<- data.frame( Variable = c("Percent Days Bleached Inside", "Percent Days Bleached Outside", 
                                       "Degree Days Inside", "Degree Days Outside",
                                       "Max Mean Summer Monthly Inside", "Max Mean Summer Monthly Outside", "Average Run Length Inside",
                                       "Average Run Length Outside", "Average Degree Days/ Run Inside", "Average Degree Days/ Run Outside",
                                       "Multi Day Runs Inside", "Multi Day Runs Outside"), 
                                 Value = c(percent_days_bleach_inside, percent_days_bleach_outside,
                                       degree_days_inside, degree_days_outside,
                                       inside_max_mean_summer_monthly, outside_max_mean_summer_monthly, avg_run_inside, avg_run_outside,
                                       avg_degree_days_inside, avg_degree_days_outside, n_multi_day_runs_inside, n_multi_day_runs_outside))
 
  
   write.csv(overall_values, paste0(path_to_data, "/Overall_Values.csv"))

  
  heat_histogram_total = ellipses %>%
    group_by(location, date) %>%
    summarise(Mean = mean(analysed_sst)) %>%
    filter(Mean > outside_max_mean_summer_monthly) %>%
    ggplot(aes(Mean)) +
    geom_histogram(aes(x = Mean,group = location, color = location, fill= location),position = 'dodge') +
    xlab("Mean Celcius") +
    ylab("Frequency") +
    ggtitle("Mean Celcius by Day") + 
    theme(plot.title = element_text(color = "red", hjust = 0.5)) 
  ggsave(file = paste0(path_to_plots, "/histogram_total.png"), heat_histogram_total)
  
  
  yearly_total <<- rbind(inside_yearly, outside_yearly)
  yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= degreedays_year, group = location, color = location)) +
    geom_smooth(aes(x=year, y= degreedays_year, group = location, color = location), se = F) +
    ggtitle("Yearly degree days") 
  
  yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= run_length_year, group = location, color = location)) +
    geom_smooth(aes(x=year, y= run_length_year, group = location, color = location), se = F) +
    ggtitle("Yearly Mean Run Length")
  
  yearly_total %>%
    ggplot() +
    geom_point(aes(x=year, y= n_runs_over_1, group = location, color = location)) +
    geom_smooth(aes(x=year, y= n_runs_over_1, group = location, color = location), se = F) +
    ggtitle("Yearly #of Runs")
  
  
  
  yearly_total %>%
    ggplot() +
    geom_point(aes(x=date, y= degreedays_month, group = location, color = location)) +
    geom_smooth(aes(x=date, y= degreedays_month, group = location, color = location), se = F) +
    ggtitle("Monthly Degreedays")
  
  yearly_total %>%
    ggplot() +
    geom_point(aes(x=date, y= run_length_month, group = location, color = location)) +
    geom_smooth(aes(x=date, y= run_length_month, group = location, color = location), se = F) +
    ggtitle("Monthly Mean Run Length")
  
  yearly_total %>%
    filter(year>2015) %>%
    ggplot() +
    geom_point(aes(x=date, y= n_runs_over_1_m, group = location, color = location)) +
    geom_smooth(aes(x=date, y= n_runs_over_1_m, group = location, color = location), se = F) +
    ggtitle("Monthly #of Runs After 2015")
}

##Just runs everything at once
big_ol_run <- function() {
  create_url_and_files(server, sst_id, start_date, end_date, lat, long_in, long_out, atoll)
  #fix_combine_data_frames(inside_file, outside_file, atoll, major1, major2, minor1, minor2)
  fix_combine_data_frames()
  plots()
}

##INPUT THIS INFORMATION THEN RUN SCRIPTS; Remember to change out the info for each atoll, but this is all you need to change
server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
sst_id <- "jplMURSST41.csv?analysed_sst"
lat <- list (min = 9.02, max = 9.32)
long_in <- list(min = 167.33, max = 167.63)
shift <- c(0, 0.5)
long_out <- list(min = (as.numeric(long_in[1])+shift[2]), max = (as.numeric(long_in[2])+shift[2]))
start_date <- as.Date("2002-06-22T09:00:00Z")
end_date <- as.Date("2020-06-22T09:00:00Z")
atoll <- "Kwajelein"

#degrees above max monthly mean that matters
bleaching_threshold_C = 0.5
degree_day_threshold = 0

major1 <- c(9.28924, 167.3501)
major2 <- c(9.08047, 167.6275)
minor1 <- c(9.11949, 167.42603)
minor2 <- c(9.26929, 167.52696)



#create_url_and_files(server, sst_id, start_date, end_date, lat, long_in, long_out, atoll)
#fix_combine_data_frames()
#plots()
big_ol_run()