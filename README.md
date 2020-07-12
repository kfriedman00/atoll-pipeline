# atoll-pipeline
N.B. the R packages used in this script are rgdal, ncdf4, raster, tidyverse, fishualize, ggplot2, and crayon, please make sure these are installed before running by running the install.packages('PACKAGE NAME') commnad in the R terminal. 

both the elipse generation file and the atoll_pipeline file need to be saved, compiled, and run for the desired output to be produced

This is a collection of scripts that download and process satellite data for the analysis of coral bleaching rates inside and outside of coral atolls. This script is designed to take sea surface temperatures from level 4 Multiscale Ultrahigh Resolution analysis from 7 different instruments at a resolution of 0.01 degrees (Latitude) x 0.01 degrees (Longitude) and a 1 day temporal resolution from the coastwatch.pfeg.noaa.gov server. more information can be found here https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1#citation_table

The elipse generation file is a function that creates an elipse based on major and minor axis endpoints given in the atoll_pipeline file by using a parametric equation and filtering out points outside the elipse boundry. The atoll_


The user inputs needed are: 

the path to the elipse generation file on the machine given as a string: line 9 source("PUT PATH TO THE ellipse_generation.R FILE HERE") 
eg. source("/Users/sriramsundararajan/Desktop/BSURP/ellipse_generation.R")

a path to the working directory in which all outputs will be saved: line 12:path_to_wd <<- "INSERT PATH TO atolls HERE. PATH MUST END WITH FORWARD SLASH" 
e.g. path_to_wd <<- "/Users/sriramsundararajan/Desktop/BSURP/atolls/"

the the minimum and maximum latitude of the rectangle that surrounds the atoll or the "inside" area of interest: line 756 lat <- list (min = ..., max = ...)

the the minimum and maximum longitutde of the rectangle that surrounds the atoll or the "inside" area of interest: line 757 long_in <- list(min = ..., max = ...)

the start date of the subset of data desired: line 758 start_date <- as.Date("YYYY-MM-DDT09:00:00Z") (all measurements of this dataset were taken at 9:00 so "T09:00:00Z" does not need to be changed"

the end date of the subset of data desired: line 759 end_date <- as.Date("YYYY-MM-DDT09:00:00Z") (all measurements of this dataset were taken at 9:00 so "T09:00:00Z" does not need to be changed"

the name of the atoll for reference: line 760 atoll <- "PUT ATOLL NAME HERE" (this is only used for filing/ organizational purposes)

A "bleaching threshold" that represents the degrees celcius above the mean monthly maximum that will be the threshold to define a particular measurement as a bleaching event: line 763 bleaching_threshold_C = 0.5

A degree day threshold that represents the degrees celcius above the mean monthly maximum that will be the threshold to allow a particular measurement to be counted in the degree days counts: line 764 degree_day_threshold = 0

the number of degrees longitude the outside area of interest is east of the inside, defined as the distance between the eatern most point of the inside area of interest to the eastern most point of the outside: line 771: shift <- c(0, ...) (the first element is 0 because the areas of interest were not different in terms of latitude) 


OPTIONAL CHANGES:
in order to compare heatmap style plots by setting the color scale, modify the limits argument in lines 555, 570, 587, 604, 629, and 650 e.g. 
scale_fill_gradientn(colours=viridis::plasma(5), limits=c(0.09,0.12))




