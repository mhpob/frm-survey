library(targets)
library(readxl)
library(dplyr)
library(brms)
library(mgcv)

cpue <- tar_read('raw_data_pot') |> 
  read_excel(sheet = 'CPUE') |> 
  rename_with(function(.) tolower(gsub('[ /_]', '', .))) |> 
  filter(species == 'black sea bass') |> 
  mutate(station = as.factor(station),
         catch = as.numeric(catch),
         month = factor(month.abb[lubridate::month(recoverdate)]),
         montho = factor(month, levels = month.abb[6:8], ordered=T),
         type = ifelse(grepl('^C', station), 'control', 'turbine'))


m1 <- gam(catch ~ 0 + month + s(potnumber, by = month)  + 
            type + s(station, bs = 're'),
          family = poisson(),
          data = cpue,
          method = 'REML')
m2 <- gam(catch ~ s(station, bs = 're'),
          family = poisson(),
          data = cpue,
          method = 'REML')


new_data <- data.frame(potnumber = seq(1, 15, 0.25))
new_data$pred <- predict(m1, newdata = new_data, type = 'response',
                         exclude = 's(station)',
                         newdata.guaranteed = T)

plot(x = new_data$potnumber, y =new_data$pred)




library(future)
plan(multisession,
     workers = availableCores(logical = F))

m1_brm <- brm(catch ~ type + month + s(potnumber, by = month) + (1|station),
              family = poisson(),
              data = cpue,
              future = TRUE,
              iter = 5000,
              warmup = 2500,
              thin = 5,
              control = list(adapt_delta = 0.97))

library(tidybayes)
library(ggplot2)
get_variables(m1_brm)
m1_brm |> 
  spread_draws(b_Intercept, r_station[station, term]) |> 
  # head()
  
  
  # spread_draws(b_cw_Intercept, r_site__cw[site,]) %>%
  # add the grand mean to the group-specific deviations
  mutate(mu = b_Intercept + r_station,
         mu = poisson()$linkinv(mu)) |> 
  ungroup()  |>
  # plot
  ggplot(aes(x = mu, y = reorder(station, mu)))+
  geom_vline(xintercept = poisson()$linkinv(fixef(m1_brm)[1, 1]),
             color = "#839496", size = 1) +
  geom_vline(xintercept = poisson()$linkinv(fixef(m1_brm)[1, 3:4]),
             color = "#839496", linetype = 2) +
  stat_halfeye(.width = .5, size = 2/3, fill = "#859900") +
  labs(x = "Predicted black sea bass catch",
       y = "Survey sites ordered by mean predicted catch") +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0),
        text = element_text(family = "Ubuntu"))  +
  coord_cartesian(xlim = c(0, 20))
