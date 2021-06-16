## need to pre load the atoll pipeline functions into environment
## need to correctly set variables (ie lat/long/shift/etc.) based on ordering in atolls doc
## need to standardize mask path naming to match atoll naming so we can automate loading
# correct mask (or add mask path as a column in the atoll csv)

#fixing atolls plots + calculations + running in bulk
library(varhandle)
library(googledrive)

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
full_run <- atoll_list[,-1]

#constants
bleaching_threshold_C = 0.5
degree_day_threshold = 0

#drive stuff
##### 
#this makes you log in to drive
drive_auth()

#need a single directory with all the masks, update link here
mask_url <- "https://drive.google.com/drive/u/0/folders/1zf5Laugu1Xbmz8w4q5QYXxEfbBMFQ9NO"
#this is the folder we want the atoll directories to go in
drive_url <- "https://drive.google.com/drive/u/0/folders/1zf5Laugu1Xbmz8w4q5QYXxEfbBMFQ9NO"

#we're just going to download all the masks to one mask folder in wd
drive_download(file=as_id(mask_url), path=paste0(path_to_wd, 'masks/'))
mask_path = paste0(path_to_wd, 'mask/')

#data format - 1: atoll, 2/3: lat, 4/5: long, majors, minors, shift x/y, 
#####
for(num in 1:length(full_run$Atoll)) {
  current_dat <- full_run[num,-1]
  
  # need standardized order of inputs, double check + clean csv
  lat  <- list(min = as.numeric(current_dat[2]), max = as.numeric(current_dat[3]))
  long <- list(min = as.numeric(current_dat[4]), max = as.numeric(current_dat[5]))
  
  atoll <- (current_dat[1])
  shift <- as.numeric(c(current_dat[15],current_dat[14]))
  
  # we could tag with region here too
  
  path_to_atoll <<- paste0(path_to_wd, atoll)
  path_to_plots <<- paste0(path_to_wd, atoll , "/Plots")
  path_to_data <<- paste0(path_to_wd, atoll, "/Data")
  
  #need to know mask naming scheme here
  #what type of file is the mask?
  mask <- read.csv(paste0(mask_path, atoll), stringsAsFactors = FALSE)
    
  big_ol_run()
  rm.all.but(keep = c("server", "sst_id", "start_date", "path_to_wd", "end_date", "full_run"), keep_functions = T)
  
  #push the file to drive
  file = paste0(path_to_wd, "/", atoll, "/")
  temp <- drive_upload(
    file,
    path = as_id(drive_url),
    type = "folder"
  )
  
  #clear the file from computer
  unlink(file)
  
}