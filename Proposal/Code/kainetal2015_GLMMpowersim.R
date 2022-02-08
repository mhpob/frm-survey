# n individuals
rep_vec <- 12

# n measures within individuals
repeat_vec <- 15

# Among-individual variation in treatment 1 – Hereby referred to as the homogeneous
# (low-variation) treatment
theta_among.homog_vec <- 0.2

# Among-individual variation in treatment 2 – Hereby referred to as the variable (highvariation) treatment
theta_among.var_vec <- 0.4

# Observation level variation/overdisperison (within-individual variation)
theta_obs_vec <- 0.58

# set up data frame
expdat <- expand.grid(
  indiv = factor(seq(rep_vec)),
  obs = seq(repeat_vec),
  ttt = c("homog", "var")
)

expdat <- transform(expdat,
                    homog = as.numeric(ttt == "homog"),
                    var = as.numeric(ttt == "var"))

expdat$total_obs <- factor(seq(nrow(expdat)))


nsim <- 20


power_sim <- array(NA,dim=c(length(theta_among.homog_vec),
                            length(theta_among.var_vec), length(theta_obs_vec),
                            length(rep_vec), length(repeat_vec),nsim, 9),
                   dimnames=list(theta_h=theta_among.homog_vec,
                                 theta_var=theta_among.var_vec,
                                 theta_obs=theta_obs_vec,num.indiv=rep_vec,
                                 repeat.meas=repeat_vec, sim.count=seq(nsim),
                                 var=c("est","stderr","zval","ttt.pval","obsvar",
                                       "indivvar.homog","indivvar.var","devdiff","var.pval")))








beta <- c(0.5, 0)
theta <- c(theta_obs_vec, theta_among.var_vec,
           theta_among.homog_vec)
.progress <- "text"
verbose <- TRUE


errmat <- matrix(NA,nrow=nsim,ncol=9)

library(lme4)

ss2 <-simulate(~ttt+(0+homog|indiv:ttt)+(0+var|indiv:ttt)+
                 (1|total_obs), nsim=nsim, family=poisson,
               # weights=rep(5,nrow(expdat)),
               newdata=expdat, newparams=list(theta=theta,beta=beta))
expdat$resp <- ss2[[1]]


library(plyr)

fitfun <- function(expdat,i,.progress="text",verbose=TRUE){
  fit2 <- try(glmer(resp~ttt+(0+homog|indiv:ttt)+
                      (0+var|indiv:ttt)+(1|total_obs), family=poisson,
                    weights=rep(5,nrow(expdat)),
                    data=expdat),silent=TRUE)
  if(is(fit2,"try-error")) return(errmat)
  fit2B <-
    try(update(fit2,.~ttt+(1|indiv:ttt)+(1|total_obs)),
        silent=TRUE)
  if (is(fit2B,"try-error")) return(errmat)
  
  fits=laply(seq(nsim),function(i)
    fitsim2(i,models=list(fit2,fit2B)),.progress=.progress)
  return(fits)
} 



fitsim2 <- function(i,models=list(fit2,fit2B)) {
  r1 <- try(refit(models[[1]],ss2[[i]]),silent=TRUE)
  if (is(r1,"try-error")) return(rep(NA,9))
  r1B <- try(refit(models[[2]],ss2[[i]]),silent=TRUE)
  ss <- try(summary(r1))
  cc <- if (is(ss,"try-error")) rep(NA,4) else
    coef(summary(r1))["tttvar",]
  res <- c(cc,unlist(VarCorr(r1)))
  if (is(r1B,"try-error")) return(c(res,rep(NA,2)))
  aa <- anova(r1,r1B)[2,]
  devdiff <- unlist(c(aa["Chisq"]))
  var.pval <- unlist(c(aa["Pr(>Chisq)"]))
  return(c(res,devdiff,var.pval))
}


power_sim[1,1,1,1,1,,] <- fitfun(expdat, i = i)


library(reshape2)
d2 <- do.call(rbind, power_sim)

d2 <- melt(power_sim)
d3 <- data.frame(d2[1:20, ],
                 est = d2[d2$var=="est", ]$value,
                 stderr = d2[d2$var=="stderr", ]$value,
                 var.pval = d2[d2$var=="var.pval", ]$value,
                 indivvar.homog = d2[d2$var=="indivvar.homog", ]$value,
                 indivvar.var = d2[d2$var=="indivvar.var", ]$value)

with(d3, mean(var.pval < 0.05))


library(ggplot2)
ggplot(arrange(d3, est),
       aes(x = seq(nsim), y = est,
           ymin = est - 1.96 * stderr, ymax = est + 1.96 *
             stderr)) +
  geom_pointrange() +
  geom_hline(yintercept = mean(d3$est), colour = "red") +
  theme_bw()


truevals <- data.frame(variable = c("indivvar.homog",  "indivvar.var"),
                       trueval = c(mean(d3$indivvar.homog),
                                   mean(d3$indivvar.var)))


d4 <- d2[d2$var=="indivvar.homog" | d2$var=="indivvar.var", ]


ggplot(d4, aes(x = sim.count, y = value, colour = var)) +
  geom_point(lwd = 3) +
  geom_hline(data = truevals,
             aes(yintercept = trueval, colour = variable), lty = 2) +
  theme_bw()
