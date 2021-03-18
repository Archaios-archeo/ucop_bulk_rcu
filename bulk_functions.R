library(tidyverse)

# opposite of %in%
`%ni%` <- Negate(`%in%`)



#' function to retrieve the number of times a separation pattern appears for any modality in a variable and repeat the information 
#' contained in another variable according to the number of separation
#' @example: if we have surveyors names in a variable "surveyors" and each name is separated by a "|" in that column,
#' we could duplicate the information contained in the "date" variable for as many surveyors as we have in each row
#' @param x: data table related to a sheet of one of the Arches spreadsheets “bulk upload”
#' @param variable_a_bon_pattern: variable that serves as a reference for the duplication of information 
#' (in the @example it is the variable “surveyors”)
#' @param variable_revue: variable that must be modified according to the number of separation pattern accounted
#' (in the @example it is the variable "date")

repetition_pattern_n_exact <- function(x, variable_a_bon_pattern, variable_revue){
  
  x$pattern <- str_count(string = x[[variable_a_bon_pattern]], pattern = "\\|") + 1
  
  for (i in 1:nrow(x)) {
    nombre_de_pattern <- x[i,"pattern", drop = TRUE]
    
    x[i, variable_revue] <- paste0(
      replicate(n = nombre_de_pattern, expr = x[i, variable_revue]), 
      collapse = "|")
    
  }
  
  x <- x %>%
    select(-pattern)
  
  return(x)
  
}


#' function for undetermined polygones > need to add documentation
st_queen <- function(a, b = a) st_relate(a, b, pattern = "****1****") # pattern to find sides, not corners



## function from github spatialEco > https://github.com/jeffreyevans/spatialEco/blob/master/R/convexHull.R
#' @title Alpha Convex Hull
#'
#' @description 
#' Calculates a convex hull on a point feature class using the Pateiro-Lopez (2009) 
#' Alphahull model 
#' 
#' @param x         SpatialPoints, SpatialPointsDataFrame, sf or matrix object 
#'                  representing [x,y] coordinates
#' @param alpha     Alpha parameter for adjusting boundary tension
#' @param sp        Output an sp SpatialPolygonsDataFrame object (TRUE/FALSE)
#'
#' @return SpatialPolygons object  
#'
#' @note 
#' This method provides flexibility over traditional convex functions in that the the alpha 
#' parameter can be adjusted to relax or increase tension between boundary-edge points
#' Due to licensing constraints associated with the alphahull package, this function is not 
#' available in the CRAN release. The function must be called from the package NAMESPACE 
#' using:  spatialEco:::convexHull. If sp = FALSE an sf polygon class object will be 
#' returned 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references 
#' Pateiro-Lopez & Rodriguez-Casal (2009) Generalizing the Convex Hull of a Sample: 
#'   The R Package alphahull. Journal of Statistical Software 34(5):1-28 
#'                                                                    
#' @examples 
#' library(sp)
#' library(sf)
#' library(dplyr)
#' 
#' #### points example
#'  data(meuse)
#'   coordinates(meuse) = ~x+y
#'  a <- convexHull(meuse, alpha=100000)
#'    plot(a)
#'      points(meuse, pch=19)
#'
#'  # Test multiple alpha values
#'   par(mfcol=c(2,2))
#'     for (a in c(500, 1500, 5000, 100000)) {
#'     ch <- convexHull(meuse, alpha = a)
#'       plot(ch)
#'        points(meuse, pch=19)
#'          title( paste0("alpha=", a))		 
#'     }
#'
#' \dontrun{
#' #### Polygon example
#' data(meuse)
#'   coordinates(meuse) = ~x+y
#'   meuse <- as(meuse, "sf")
#'   meuse_poly <- st_buffer(meuse, dist = meuse$elev*15)
#' 
#' # Create [x,y] points representing polygon boundaries 
#'
#' poly_points <- sf::st_segmentize(meuse_poly, dfMaxLength = 5) %>% 
#'  sf::st_coordinates() %>% 
#'    as.data.frame() %>% 
#'      subset(select = c(X, Y)) %>% 
#'        sf::st_as_sf(coords = c("X", "Y"))  
#'   			
#' a <- spatialEco::convexHull(poly_points, alpha = 100000, 
#'                             sp=FALSE)
#'  plot(sf::st_geometry(a), cex=1.5, col="red") 
#'     plot(sf::st_geometry(meuse_poly), col="black", add=TRUE)
#'}
#'
#' @export convexHull
convexHull <- function(x, alpha = 250000, sp = TRUE)	{
  if(!any(which(utils::installed.packages()[,1] %in% "alphahull")))
    stop("please install alphahull package before running this function")
  if (!inherits(x, "SpatialPointsDataFrame") &  
      !inherits(x, "SpatialPoints") &
      !inherits(x, "sf") &
      !inherits(x, "matrix")) 
    stop(deparse(substitute(x)), " x must be a spatial (sp, df) or matrix object")
  if(inherits(x, "sf")) { xy <- as.data.frame(sf::st_coordinates(x)) }
  if(inherits(x, "SpatialPointsDataFrame") &  inherits(x, "SpatialPoints") ) {
    xy <- as.data.frame(sp::coordinates(x))
  }
  if(inherits(x, "matrix")) {
    xy <- as.data.frame(x)
  }
  xy <- xy[!duplicated(xy[c(1,2)]),]  
  a <- alphahull::ashape(as.matrix(xy), alpha = alpha)
  l <- list()
  for (i in 1:nrow(a$edges)) { l[[i]] <-  sp::Line(rbind(a$edges[i, 3:4], a$edges[i, 5:6])) }
  a <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(sp::Lines(l, as.character("1")))),
                                 data.frame(name ="ashape"), match.ID = FALSE)
  a <- sf::st_polygonize(sf::st_as_sf(a))
  if(sp) a <- as(a, "Spatial") 									   
  return( a )	
}


