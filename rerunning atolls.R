## need to pre load the atoll pipeline functions into environment
## need to correctly set variables (ie lat/long/shift/etc.) based on ordering in atolls doc
## need to standardize mask path naming to match atoll naming so we can automate loading
# correct mask (or add mask path as a column in the atoll csv)

#fixing atolls plots + calculations + running in bulk
library(varhandle)
library(googledrive)
source("C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/atoll_pipeline_script.R")

# this is where we'll temporarily save data
path_to_wd <<- "C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/"
setwd(path_to_wd)

#need to load in functions from pipeline manually, could automate using kyle's thing

server <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
sst_id <- "jplMURSST41.csv?analysed_sst"
start_date <- as.Date("2003-01-01T09:00:00Z")
end_date <- as.Date("2019-12-31T09:00:00Z")

#build a list of all the atolls you want to run
atoll_list <- read.csv("other data/atolls_sheet.csv", stringsAsFactors = F)

# need to know how this csv is set up to properly set run variables
full_run <- atoll_list[-1,-1]

#constants
bleaching_threshold_C = 0.5
degree_day_threshold = 0

#drive stuff
##### 
#this makes you log in to drive
drive_auth()

#need a single directory with all the masks, update link here
mask_path <- "C:/Users/kanoe/Documents/Research Data/BSURP/Atolls/Vertex files/"
#this is the folder we want the atoll directories to go in
drive_url <- "https://drive.google.com/drive/u/1/folders/1OyVeEd-SG2J9N7-erd6WBFjm63d7Jc7h"


#copy lines 63 through 67

#data format - 1: atoll, 2/3: lat, 4/5: long, majors, minors, shift x/y, 
#####
for(num in 4:6) {
  bleaching_threshold_C = 0.5
  degree_day_threshold = 0
  current_dat <- full_run[num,]
  
  # need standardized order of inputs, double check + clean csv
  lat  <- list(min = as.numeric(current_dat[2]), max = as.numeric(current_dat[3]))
  long_in <- list(min = as.numeric(current_dat[4]), max = as.numeric(current_dat[5]))
  
  atoll <- (current_dat[1])
  print(atoll)
  shift <- as.numeric(c(current_dat$shift_lat,current_dat$shift_lon))
  
  # we could tag with region here too
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  
  #need to know mask naming scheme here
  ROI_file <- paste0(mask_path, atoll, " vertices.csv")
  #vertices <- read.csv(paste0(mask_path, atoll), stringsAsFactors = FALSE)
  
  x <- drive_mkdir(
    name=atoll[1,1],
    path = as_id(drive_url),
  )
  
  dat <- drive_mkdir(
    name="Data",
    path=x
  )
  
  plts <- drive_mkdir(
    name="Plots",
    path=x
  )
  
  #need source vertices reading code 
  big_ol_run()
  
  file = paste0(path_to_wd, atoll, "/")
  #push the file to drive
  
  all_plots <- list.files(path=path_to_plots)
  for(pl in all_plots) {
    p = paste0(path_to_plots, "/", pl)
    drive_upload(p, path=plts)
  }
  #clear the file from computer
  unlink(file, recursive=T)
  rm.all.but(keep = c("server", "sst_id", "start_date", "path_to_wd", 
                      "end_date", "full_run", "drive_url", "mask_path"), 
             keep_functions = T)
}


