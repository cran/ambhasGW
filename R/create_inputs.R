#' Create dummy input data
#' 
#' Creates dummy data and corresponding yml file to run the model.
#' 
#' @param dummy.dir Dummy directory to input and output data
#' @examples
#' 
#' 
#' 
#' # Creates input data and yml file
#' create_inputs('~')
#' 
#' 
#' @importFrom stats runif
#' @export
create_inputs <- function(dummy.dir){
  
  # Creates dummy directory if does not exist
  if(!file.exists(dummy.dir)){
    dir.create(dummy.dir)
    print('Directory not found, created it!')
  }
  
  # Creates dummy input directory if does not exist
  in.dir <- file.path(dummy.dir, 'input')
  if(!file.exists(in.dir)){
    dir.create(in.dir)
  }
  
  # Creates dummy output directory if does not exist
  out.dir <- file.path(dummy.dir, 'output')
  if(!file.exists(out.dir)){
    dir.create(out.dir)
  }
  
  set.seed(150) # to make the results repeatable
  
  # create dummy initial head data file
  h.ras <- raster::raster(matrix(stats::runif(9, 0, 100), ncol=3))
  raster::writeRaster(h.ras, file.path(in.dir, 'hini.tif'), overwrite=TRUE)
  
  # create dummy rainfall data file
  rain.ras <- raster::raster(matrix(stats::runif(9, 0, 1), ncol=3))
  raster::writeRaster(rain.ras, file.path(in.dir, 'rain.tif'), overwrite=TRUE)

  # create dummy draft (pumping) data file
  draft.ras <- raster::raster(matrix(stats::runif(9, 0, 0.5), ncol=3))
  raster::writeRaster(draft.ras, file.path(in.dir, 'draft.tif'), 
                      overwrite=TRUE)
  
  # Creates input yml file
  yml.file <- yaml::as.yaml(list(hini.file = file.path(in.dir, 'hini.tif'),
               rainfall.file=file.path(in.dir, 'rain.tif'),
               draft.file=file.path(in.dir, 'draft.tif'),
               rf.rech.factor=0.1,
               p.baseflow=0.9,
               hmin.baseflow=750,
               trans=1500,
               sy=0.02,
               max.t=5,
               dx=raster::res(h.ras)[1],
               out.dir=out.dir))
  writeLines(yml.file, file.path(in.dir, 'parameters.yml'))
}