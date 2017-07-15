#' Parameters definition
#' 
#' Prints the parameters definition of given parameter
#' 
#' @param par Parameter name
#' @examples
#'  
#' parameter_definition('hini')
#' 
#' @export

parameter_definition <- function(par){
  
  definition <- list()
  
  definition['hini'] <- "Intial groundwater head in meter unit."
  
  definition['rainfall'] <- "Rainfall in meter unit."
  
  definition['draft'] <- "Draft or groundwater pumping in meter unit."
  
  definition['rf.rech.factor'] <- "Rainfall recharge factor i.e. recharge = recharge factor*rainfall. This parameter is dimensonless."
  
  definition['p.baseflow'] <- "Parameter for baseflow (dimensonless). A higher value produces lesser baseflow."
  
  definition['hmin.baseflow'] <- "Groundwater head corresponding to the zero baseflow i.e. when
  groundwater head is <= hmin.baseflow, there is no baseflow from a grid."
  
  definition['trans'] <- "Transmissivity (m**2/day)"
  
  definition['sy'] <- "Specific yield (dimensonless)"
 
  definition['max.t'] <- "The timestep to run the model (days)"
  
  definition['dx'] <- "The grid spacing (m). It should be equal in x and y direction."
  
  if (is.na(names(definition[par]))){
    print('key not found, print all definitions.')
    for (name in names(definition)){
      print(sprintf("%s: %s",name, definition[name]))
    }
  } else {
    print(sprintf("%s: %s",par, definition[par]))
  }
}

