n_trawl_ctrl <- 4
n_trawl_imp <- 8


n_phase <- 3


n_month <- 9
n_year <- 6


log_spacing <- function(trawl_length, n_pots, round = NULL){
  spacing <- (1:n_pots) ^ log(trawl_length - 10, n_pots) + 10
  
  if(!is.null(round)){
    round(spacing, round)
  } else{
    spacing
  }
}

n_dist <- length(log_spacing(900, 15, round = 0))

library(data.table)

expdat <- data.table(
  zone = rep(
    c(rep('control', n_trawl_ctrl * n_dist),
      rep('impact', n_trawl_imp * n_dist)),
    times = n_month * n_year),
  
  phase = rep(c('before', 'during', 'after'),
              each = (n_trawl_ctrl + n_trawl_imp) * n_dist * n_month * (n_year / n_phase)),
  
  dist = rep(log_spacing(900, 15, round = 0),
             times = (n_trawl_ctrl + n_trawl_imp) * n_month * n_year),
  
  month = rep(rep(month.abb[3:11],
              each = (n_trawl_ctrl + n_trawl_imp) * n_dist),
              times = n_year),

  year = rep(1:6,
             each = (n_trawl_ctrl + n_trawl_imp) * n_dist * n_month)
)
expdat[, trawlid := paste0('Y', year, month, toupper(substr(zone, 1, 1)),
                           toupper(substr(phase, 1, 1)), '_')]

expdat[, trawlid := paste0(trawlid,
                           rep(rep(c(1:4, 1:8), each = n_dist), times = n_month*n_year))]



k <- dcast(expdat, trawlid + month + year + dist ~ zone + phase,
           fun.aggregate = function(.) ifelse(length(.) > 0, 1, 0))



# Among-individual variation in treatment 1 – Hereby referred to as the homogeneous
# (low-variation) treatment
theta_among.homog_vec <- 0.2

# Among-individual variation in treatment 2 – Hereby referred to as the variable (highvariation) treatment
theta_among.var_vec <- 0.4

# Observation level variation/overdisperison (within-individual variation)
theta_obs_vec <- 0.58


fish_per_trawl <- 60
fish_per_pot <- fish_per_trawl / n_dist

beta <- c(
  log(fish_per_pot), # intercept: zonecontrol, phaseafter
  0, #dist
  log(fish_per_pot * 10) - log(fish_per_pot), #zoneimpact
  0, #phasebefore
  0, #phaseduring
  -0.008, #dist:zoneimpact
  0, #dist:phasebefore
  0, #dist:phaseduring
  -(log(fish_per_pot * 10) - log(fish_per_pot)), #zoneimpact:phasebefore
  -(log(fish_per_pot * 2) - log(fish_per_pot)), #zoneimpact:phaseduring
  0.0073, #dist:zoneimpact:phasebefore
  -0.004  #dist:zoneimpact:phaseduring
)


# beta <- c(6,
          # -0.8)


theta <- c(theta_obs_vec, theta_among.var_vec,
           theta_among.homog_vec)

library(lme4)
#START HERE (ish)
# Need to add betas
#     Adjust things to fit, see which order theyre presented
#     Adjust to fit properly.
j <- simulate(~ dist*zone*phase + (1|month:year) + (1|trawlid),
              family = poisson(),
              newdata = expdat,
              newparams = list(
                theta = c(0.58, 0.2),
                beta = beta
              ))

expdat$sim <- j


xtabs(sim ~ zone + phase, data = expdat)
expdat[, .(sum(sim)), by = c('trawlid')][unique(expdat, by = c('zone', 'phase', 'trawlid')), 
                                         on = 'trawlid'][
    , mean(V1), by = c('zone', 'phase')]

k <- glmer(sim ~ dist*zone*phase  + (1|trawlid),
           family = poisson, data = expdat)







simulate(~ttt+(0+homog|indiv:ttt)+(0+var|indiv:ttt)+
           (1|total_obs), nsim=nsim, family=poisson,
         # weights=rep(5,nrow(expdat)),
         newdata=expdat, newparams=list(theta=theta,beta=beta))
