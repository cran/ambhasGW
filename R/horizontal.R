#' Horizontal distribution of the flow in grid.
#' 
#' Simulates the horizontal distribution of the flow in grids based on the diffusivity.
#' 
#' @param h Initial head
#' @param beta a parameter based on the diffusivity, time step and grid size.
#' @return Returns the updated head.
#' @examples
#' 
#' 
#' 
#' # create synthetic head:
#' h <- cbind(c(1,2,3),c(4,5,6),c(7,8,9))
#' h
#' beta <- 0.5
#' h1 <- update_horizontally(h, beta)
#' h1
#' 
#' # increase the value of beta:
#' beta <- 1.0
#' h1 <- update_horizontally(h, beta)
#' 
#' # decrease the value of beta:
#' beta <- 0
#' h1 <- update_horizontally(h, beta)
#' h1
#' 
#' @import raster
#' @export
update_horizontally <- function(h, beta){
  nx <- raster::ncol(h)
  ny <- raster::nrow(h)
  hn <- 0*h
  hs <- 0*h
  he <- 0*h
  hw <- 0*h
  
  # groundwater spatial modelling
  # no flux boundary condition is assumed at each boundary.
  # fluxes are allowed to leave as baseflow only.
  hn[2:ny,] <- h[1:(ny-1),]
  hn[1,] <- h[1,]
  hn[is.na(hn)] <- h[is.na(hn)]
  
  hs[1:(ny-1),] <- h[2:ny,]
  hs[ny,] <- h[ny,]
  hs[is.na(hs)] <- h[is.na(hs)]
  
  he[,2:nx] <- h[,1:(nx-1)]
  he[,1] <- h[,1]
  he[is.na(he)] <- h[is.na(he)]
  
  hw[,1:(nx-1)] <- h[,2:nx]
  hw[,nx] <- h[,nx]
  hw[is.na(hw)] <- h[is.na(hw)]
  
  h <- (1-4*beta)*h + beta*(hn+hs+he+hw)
  
  return(h)
}