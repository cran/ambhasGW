#' Ground Water Modelling
#' 
#' \code{ambhasGW} Computes the groundwater head for each time step 
#' in raster format
#' @param input.file yml file having input parameters information
#' @import yaml raster rgdal
#' @examples
#' 
#' # Create necessary input file to make dummy run
#' # Dummy directory to run
#' dummy.dir <- tempdir()
#' 
#' # Make dummy run
#' create_inputs(dummy.dir)
#' input.file <- file.path(dummy.dir , 'input/parameters.yml')
#' ambhasGW(input.file)
#' 
#' @export
 
ambhasGW <- function(input.file){

  ############ read input file ########
  config <- yaml::yaml.load_file(input.file)
  
  ######## initial condition ###############
  hini <- raster::raster(config$hini.file)
  rainfall <- raster::raster(config$rainfall.file)
  draft <- raster::raster(config$draft.file)
  rf.rech.factor <- config$rf.rech.factor
  p.baseflow  <- config$p.baseflow
  hmin.baseflow <- config$hmin.baseflow
  trans <- config$trans
  sy <- config$sy
  max.t <- config$max.t
  dx <- config$dx

  # initial computations
  dt <- 1 # defaul as time step is 1
  beta <- (trans/sy)*(dt/dx**2)
  
  # Recharge due to rainfall
  rf.recharge <- rf.rech.factor * rainfall
  
  # Net recharge (all recharge - all draft)
  net.recharge <- rf.recharge - draft
  
  

  # loop over time
  h <- hini 
  for (i in 1:max.t){
    # Update head for vertical flow
    temp <- update_vertically(h, sy, net.recharge,
                              p.baseflow, hmin.baseflow)
    h <- temp$h
    
    # Update head for horizontal flow
    h <- update_horizontally(h, beta)
    
    # Write head into raster
    out.file <- file.path(config$out.dir, sprintf('head_%03d.tif', i))
    raster::writeRaster(h, filename=out.file, format='GTiff', overwrite=TRUE)
  }
}