#####
#This file takes in a dataframe named square consisting of time series temperature data within a 
#square latitude/longitude boundary
#based on the end points of major and minor axis, it contructs a filter by categorizing each
#unique spatial location as inside or outside the ellipse boundar

#find the center point of ellipse
get_center <- function(pt1, pt2, pt3, pt4) {
  midpt1 <- c((pt1[1] + pt2[1]) / 2, (pt1[2] + pt2[2]) / 2)
  midpt2 <- c((pt3[1] + pt4[1]) / 2, (pt3[2] + pt4[2]) / 2)
  (midpt1 + midpt2) / 2
}

#calculate the angle from horizontal of the given data point
get_theta <- function(pt, center) {
  if(pt[2] == center[2]) {
    pi/2
  } else {
    val <- atan((pt[1] - center[1])/(pt[2] - center[2]))
    val
  }
}

is_inside <- function(pt, angle) {
  #find angle between the major axis and the point
  this_angle <- get_theta(pt) - angle
  
  temp_y <- pt[1] - center[1]
  temp_x <- pt[2] - center[2]
  
  #reverse the rotation
  y <- temp_x * sin(-angle) + temp_y * cos(-angle)
  x <- temp_x * cos(-angle) - temp_y * sin(-angle)
  if(x == 0) {
    adjusted_theta <- pi / 2
  } else {
    adjusted_theta <- atan((y * r_major) / (x * r_minor))
  }
  max_y <- r_minor * sin(adjusted_theta) 
  max_x <- r_major * cos(adjusted_theta) 
  
  ((x * x + y * y) <= (max_x * max_x + max_y * max_y))
}

#all ellipse parameters are tuples with lat, long pairs
ellipse_generation <- function(square, major1, major2, minor1, minor2){
  #calculate vectors representing axis of ellipse
  majoraxis <- c(major2[1] - major1[1], major2[2] - major1[2])/2
  minoraxis <- c(minor2[1] - minor1[1], minor2[2] - minor1[2])/2
  

  #find center and angle of tilt
  center <- get_center(major1, major2, minor1, minor2)
  angle <- get_theta(majoraxis, c(0,0))
  
  #this function is currently based on euclidean geometry since all spatial scales are very small
  #could refactor to be based on spherical geometry and use lonlat = TRUE
  r_major <- pointDistance(major1, major2, lonlat = FALSE) / 2
  r_minor <- pointDistance(minor1, minor2, lonlat = FALSE) / 2
  
  #get info for data from one day, add row w/ true and false repeating, then filter             
  day_dat <- unique(square[,c("latitude", "longitude")])
  
  #check that there are the correct number of spatial points in the chosen day, else throw error
  print(length(day_dat[,1]))
  inside <- apply(day_dat, 1, is_inside)
  
  inside_data <- square[inside,]
  return(inside_data)
}
