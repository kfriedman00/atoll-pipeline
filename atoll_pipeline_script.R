library(rgdal)
library(ncdf4)
library(raster)
library(tidyverse)
library(fishualize)
library(gganimate)
library(ggplot2)
source("PUT PATH TO THE ellipse_generation.R FILE HERE")

##You need to change the directory above and the path directories in the first function, that's it
path_to_wd <<- "INSERT PATH TO atolls HERE. PATH MUST END WITH FORWARD SLASH"
setwd(path_to_wd)

##Creates path names, directories, urls, file names, and downloads unprocessed data files (if they don't exist)
create_url_and_files <- function(server, sst_id, start_date, end_date, lat, long_in, long_out, atoll) {
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  if (!dir.exists(path_to_atoll) ) {dir.create(path_to_atoll)}
  if (!dir.exists(path_to_plots) ) {dir.create(path_to_plots)}
  if (!dir.exists(path_to_data) ) {dir.create(path_to_data)}
  cat(green("DIRECTORIES SUCCESSFULLY CREATED") %+% "\n")
  
  inside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min, ")", ":1:", "(", lat$max, ")", "]", "[", "(", long_in$min, ")", ":1:", "(", long_in$max, ")","]" )
  outside_url <<- paste0(server, sst_id, "[", "(", start_date, ")", ":1:", "(", end_date, ")", "]", "[", "(", lat$min, ")", ":1:", "(", lat$max, ")", "]", "[", "(", long_in$min+shift[2], ")", ":1:", "(", long_in$max+shift[2], ")","]" )
  inside_file <<- paste0(atoll, "/Data/", atoll, "_inside_sst.csv")
  outside_file <<- paste0(atoll, "/Data/", atoll, "_outside_sst.csv")
  
  cat(red("FILE DOWNLOAD COMMENCING") %+% "\n")
  if(!file.exists(inside_file)) curl::curl_download(inside_url, inside_file)
  cat(green("INSIDE ATOLL DATA SUCCESSFULLY DOWNLOADED") %+% "\n")
  if(!file.exists(outside_file)) curl::curl_download(outside_url, outside_file)
  cat(green("OUTSIDE ATOLL DATA SUCCESSFULLY DOWNLOADED") %+% "\n")
}

##"Fixes" data frames (column class types, etc), combines them, and runs the ellipse_generation function
fix_combine_data_frames <- function() {
  inside <- read.csv(inside_file)
  outside <- read.csv(outside_file)
  
  cat(blue("COMBINING SQUARE DATA NOW")%+% "\n")
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
  cat(green("SQUARE DATA COMBINED AND SAVED AS ") %+% black(atoll) %+% black(" _full.csv") %+% "\n")
  print("Full (before ellipse generation):")
  print(head(Full))
  
  cat(green("CREATING ELLIPTICAL MASKS NOW") %+% "\n")
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
  cat(green("ELLIPTICAL DATA COMBINED AND SAVED AS ") %+% black(atoll) %+% black(" _processed_data.csv") %+% "\n")
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
      bleached = mean(analysed_sst) >(inside_max_mean_summer_monthly + 1) 
    ) %>%
    filter(
      !duplicated(date)
    )
  
  
  outside_day = outside %>% group_by(
    date
  ) %>%
    mutate(
      bleached = mean(analysed_sst) > (outside_max_mean_summer_monthly +1)
    ) %>%
    filter(
      !duplicated(date)
    ) 
  
  percent_days_bleach_inside <<- sum(inside_day$bleached)/nrow(inside_day)
  percent_days_bleach_outside <<- sum(outside_day$bleached)/nrow(outside_day)
  
  days_in_dataset = nrow(outside_day)
  
  #_______________________________________________________________________
  inside_day = inside_day %>%
    subset(
      bleached == T
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean(analysed_sst) - inside_max_mean_summer_monthly
    ) %>%
    ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1))
    ) %>%
    group_by(consec) %>%
    mutate(
      degree_days = mean(degreeabove)*sum(bleached)
    )
  
  degree_days_inside <<- mean(inside_day$degree_days)
  
  inside_day = inside_day%>% group_by(year) %>%
    mutate(
      degreeedays_year = mean(
        degree_days
      )
    ) 
  inside_day = inside_day[c("year", "degreeedays_year")]
  inside_yearly <<- inside_day %>%
    filter(
      !duplicated(year)
    )
  
  
  outside_day = outside_day %>%
    subset(
      bleached == T
    ) %>%
    group_by(date) %>%
    mutate(
      degreeabove = mean(analysed_sst) - outside_max_mean_summer_monthly
    ) %>%
    ungroup() %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1))
    ) %>%
    group_by(consec) %>%
    mutate(
      degree_days = mean(degreeabove)*sum(bleached)
    )
  
  degree_days_outside <<- mean(outside_day$degree_days)
  
  outside_day = outside_day%>% group_by(year) %>%
    mutate(
      degreeedays_year = mean(
        degree_days
      )
    ) 
  outside_day = outside_day[c("year", "degreeedays_year")]
  outside_yearly <<- outside_day %>%
    filter(
      !duplicated(year)
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
      point_bleach = analysed_sst> inside_max_mean_summer_monthly + 1
    )
  
  
  n_points = inside %>%
    group_by(year, month, date) %>%
    tally()
  n_points = n_points$n[1]
  
  
  inside = inside %>%
    group_by(year, month, date) %>%
    mutate(
      percent_bleach = sum(point_bleach)/n_points
    )
  
  rm(n_points)
  
  outside = outside %>%
    mutate(
      point_bleach = analysed_sst> outside_max_mean_summer_monthly +1
    )
  
  n_points = outside %>%
    group_by(year, month, date) %>%
    tally()
  n_points = n_points$n[1]
  
  outside = outside %>%
    group_by(year, month, date) %>%
    mutate(
      percent_bleach = sum(point_bleach)/n_points
    )
  
  rm(n_points)
  
  #_________________________________________________________
  #degree days
  inside_degreeday <<- inside %>% 
    mutate(
      degreeabove = analysed_sst - inside_max_mean_summer_monthly
    ) %>%
    filter(
      point_bleach == T
    )
  
  inside_degreeday = inside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1))
    ) %>%
    group_by(
      latitude, longitude, consec
    ) %>%
    mutate(
      degree_days = mean(degreeabove)*sum(point_bleach)
    ) %>%
    ungroup(
    ) %>%
    group_by(
      latitude, longitude, year
    ) %>%
    mutate(
      degree_days_year = mean(degree_days)
    )
  inside_degreeday = inside_degreeday[c("latitude","longitude","year","degree_days", "degree_days_year")]
  
  outside_degreeday <<- outside %>% 
    mutate(
      degreeabove = analysed_sst - outside_max_mean_summer_monthly
    ) %>%
    filter(
      point_bleach == T
    )
  
  outside_degreeday = outside_degreeday %>%  group_by(latitude, longitude) %>%
    mutate(
      consec = cumsum(!c(TRUE, diff(date) == 1))
    ) %>%
    group_by(
      latitude, longitude, consec
    ) %>%
    mutate(
      degree_days = mean(degreeabove)*sum(point_bleach)
    ) %>%
    ungroup(
    ) %>%
    group_by(
      latitude, longitude, year
    ) %>%
    mutate(
      degree_days_year = mean(degree_days)
    )
  
  test = outside_degreeday %>% filter(
    latitude ==16.77, longitude == -169.3, consec == 81
  )
  
  
  outside_degreeday = outside_degreeday[c("latitude","longitude","year","degree_days", "degree_days_year")]
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
    summarise(time_bleached  = sum(point_bleach)/days_in_dataset) %>%
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
    summarise(time_bleached  = sum(point_bleach)/days_in_dataset) %>%
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
  max_sst_in <- inside %>% 
    group_by(latitude, longitude) %>%
    filter(year == 2015) %>%
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
    ylab("Latitude") + xlab("Longitude")
  # dev.off()
  
  ggsave(file = paste0(path_to_plots, "/max_sst_in.png"), max_sst_in)
  
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
  ########
  
  ellipse_area <- ellipse_area(major1, major2, minor1, minor2)
  ellipse_area_km <- 12321*ellipse_area
  
  overall_values <<- data.frame( Variable = c("Percent Days Bleached Inside", "Percent Days Bleached Outside", 
                                              "Degree Days Inside", "Degree Days Outside",
                                              "Max Mean Summer Monthly Inside", "Max Mean Summer Monthly Outside", "Ellipse Area", "Ellipse Area (km)"), 
                                 Value = c(percent_days_bleach_inside, percent_days_bleach_outside,
                                           degree_days_inside, degree_days_outside,
                                           inside_max_mean_summer_monthly, outside_max_mean_summer_monthly, ellipse_area, ellipse_area_km))
  
  write.csv(overall_values, paste0(path_to_data, "/", atoll, "_Overall_Values.csv"))
  
  cat(green("PLOTS SUCCESSFULLY DOWNLOADED"))
}
  
ellipse_area <- function(major1, major2, minor1, minor2){
  major_dist <- sqrt((major2[1] - major1[1])^2 + (major2[2] - major1[2])^2)
  minor_dist <- sqrt((minor2[1] - minor1[1])^2 + (minor2[2] - minor1[2])^2)
  area <- pi*major_dist*minor_dist
  return(area)
}

##Just runs everything at once
big_ol_run <- function() {
  create_url_and_files(server, sst_id, start_date, end_date, lat, long_in, long_out, atoll)
  fix_combine_data_frames()
  plots()
}

##INPUT THIS INFORMATION THEN RUN SCRIPTS; Remember to change out the info for each atoll, but this is all you need to change
server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
sst_id <- "jplMURSST41.csv?analysed_sst"
lat <- list (min = ..., max = ...)
long_in <- list(min = ..., max = ...)
start_date <- as.Date("2002-06-22T09:00:00Z")
end_date <- as.Date("2020-06-22T09:00:00Z")
atoll <- "PUT ATOLL NAME HERE"

major1 <- c(..., ...)
major2 <- c(..., ...)
minor1 <- c(..., ...)
minor2 <- c(..., ...)

shift <- c(0, ...)

create_url_and_files(server, sst_id, start_date, end_date, lat, long_in, atoll)
fix_combine_data_frames()
plots()
