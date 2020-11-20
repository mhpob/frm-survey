#' This is an attempt to figure out what spacing we want between deployed pots.
#' 
#' If we assume that there is a linear response with increasing distance from a turbine, then even spacing works. However, if there is a logarithmic response, then it might be better to space the pots logarithmically, with the base of the logarithm being the number of desired pots.
#' 
#' At this point, we are aiming for 15 pots and 900m or 450m trawl lines.

log_spacing <- function(trawl_length, n_pots, round = NULL){
  spacing <- (1:n_pots) ^ log(trawl_length - 10, n_pots) + 10
  
  if(!is.null(round)){
    round(spacing, round)
  } else{
    spacing
  }
}

#' If a 900m trawl length:
log_spacing(900, 15, round = 0)

#' If a 450m trawl length:
log_spacing(450, 15, round = 0)


#' Compare how this looks with a linear spacing of pots:

plot(y = rep(0, 15), x = log_spacing(900, 15, round = 0),
     type = 'n', yaxt = 'n', xlab = 'Distance from turbine (m)', ylab = '')

rect(0, -10, 100, 10, col = 'pink')
rect(100, -10, 300, 10, col = 'darkgoldenrod1')
rect(300, -10, 950, 10, col = 'lightblue')

points(y = rep(0.5, 15), x = log_spacing(900, 15, round = 0))
points(y = rep(0.25, 15), x = seq(10, 900, length.out = 15), pch = 2)

points(y = rep(-0.25, 15), x = log_spacing(450, 15, round = 0), col = 'red')
points(y = rep(-0.5, 15), x = seq(10, 450, length.out = 15), pch = 2, col = 'red')
