#' Update the vertical flux components
#' 
#' Update the grids with net recharge and discharge, also computes the baseflow
#'  from each grid.
#' 
#' @param h Initial head raster
#' @param sy Specific yield [0-1].
#' @param net.recharge Net recharge (recharge - draft)
#' @param p.baseflow parameter for the baseflow [0-1]. 
#' @param hmin.baseflow head corresponding to zero baseflow.
#' @return Returns a list containing the updated head and computed baseflow.
#' @examples
#' 
#' 
#' # create synthetic head and parameters
#' h <- 10
#' sy <- 0.01
#' net.recharge <- 1
#' p.baseflow <- 0.7
#' hmin.baseflow <- 5
#' 
#' # update the head and compute baseflow
#' out <- update_vertically(h, sy, net.recharge, p.baseflow, hmin.baseflow)
#' out
#' 
#' @import raster
#' @export
update_vertically <- function(h, sy, net.recharge,
                              p.baseflow, hmin.baseflow){
  
  # baseflow
  baseflow <- sy*(1-p.baseflow)*(h-hmin.baseflow + net.recharge/sy)
  baseflow[baseflow<0] <- 0
  
  # update head
  h <- h + (net.recharge-baseflow)/sy
  
  return(list(h=h, baseflow=baseflow))
}
