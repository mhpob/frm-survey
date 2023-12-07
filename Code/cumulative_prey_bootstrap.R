library(dplyr) # package for data manipulation
library(ggplot2) # package for plotting
theme_set(theme_minimal())

bsb_diet <- read.csv(
  'data/Black Sea Bass Analysis - 2023.csv',
  na.strings = '' 
)

bsb_month <- bsb_diet |> 
  filter(Survey == 'pot') |> 
  mutate(
    Date_Diss = as.Date(Date_Diss, format = '%m-%d-%y'),
    new_taxa = FALSE
  ) |> 
  arrange(Date_Diss) |> 
  select(Fish_ID, Lowest_Classification, new_taxa, Date_Caught, Date_Diss) |> 
  # use regular expressions (like ctrl+F)
  # "Starts with '4'"
  filter(
    # "Does this value in the Date_Caught column START WITH ("^") the
    # character "4"
    grepl('^4', Date_Caught)
  )


# "!" means "NOT"
# bsb_month[1, 'new_taxa'] <- !is.na(bsb_month[1, 'Lowest_Classification'])
bsb_month[1, 'new_taxa'] <- 
  ifelse(
    test = is.na(bsb_month[1, 'Lowest_Classification']),
    yes = FALSE,
    no = TRUE
  )
  
 

for(i in 2:nrow(bsb_month)){
  bsb_month[i, 'new_taxa'] <- 
    !(bsb_month[i, 'Lowest_Classification'] %in% 
        bsb_month[1:(i-1), 'Lowest_Classification'])
}



bsb_plot <-
  bsb_month |> 
  # For each fish... (keeping Date_diss in there for ordering)
  group_by(Date_Diss, Fish_ID) |> 
  # Add the number of new taxa
  summarize(new_taxa = sum(new_taxa)) |>
  ungroup() |>
  mutate(
    # Note what number stomach it was
    n_stomachs = 1:n(),
    # and take the cumulative sum of new taxa
    new_taxa = cumsum(new_taxa)
  )

ggplot(data = bsb_plot,
       aes(x = n_stomachs, y = new_taxa)) +
  geom_step() +
  geom_smooth(method = 'lm',
              formula = y ~ log(x))


# linear model
# y = mx + b is y ~ x in R speak
#   It assumes you want an intercept (b) and it fits the slope for x (m)
mod <- lm(new_taxa ~ n_stomachs, data = bsb_plot)
summary(mod) #gives coefs and significance

mod <- lm(new_taxa ~ log(n_stomachs), data = bsb_plot)

mod_pred <- predict(
  object = mod,
  newdata = data.frame(
    n_stomachs = seq(1,
                     max(bsb_plot$n_stomachs),
                     length.out = 100)
  ),
  se.fit = T
)
mod_pred <- data.frame(n_stomachs = seq(1,
                                        max(bsb_plot$n_stomachs),
                                        length.out = 100),
                       pred = mod_pred$fit,
                       lci = mod_pred$fit - 1.96 * mod_pred$se.fit,
                       uci = mod_pred$fit + 1.96 * mod_pred$se.fit)

ggplot(data = mod_pred, aes(x=n_stomachs)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'lightgray') +
  geom_line(aes(y = pred)) +
  geom_point(data = bsb_plot, aes(x = n_stomachs, y = new_taxa)) +
  geom_step(data = bsb_plot, aes(x = n_stomachs, y = new_taxa))



# bootstrapping
random_indices <- sample(
  x = 1:nrow(bsb_month),
  size = nrow(bsb_month)
  )
bsb_strap <- bsb_month[random_indices,]


bsb_strap[1, 'new_taxa'] <- 
  ifelse(
    test = is.na(bsb_strap[1, 'Lowest_Classification']),
    yes = FALSE,
    no = TRUE
  )
for(i in 2:nrow(bsb_strap)){
  bsb_strap[i, 'new_taxa'] <- 
    !(bsb_strap[i, 'Lowest_Classification'] %in% 
        bsb_strap[1:(i-1), 'Lowest_Classification'])
}
bsb_plot <- bsb_strap |> 
  # For each fish... (keeping Date_diss in there for ordering)
  group_by(Date_Diss, Fish_ID) |> 
  # Add the number of new taxa
  summarize(new_taxa = sum(new_taxa)) |>
  ungroup() |> 
  mutate(
    # Note what number stomach it was
    n_stomachs = 1:n(),
    # and take the cumulative sum of new taxa
    new_taxa = cumsum(new_taxa)
  )

ggplot(data = bsb_plot, aes(x = n_stomachs, y = new_taxa)) +
  geom_step() +
  geom_smooth(method = 'lm',
              formula = y ~ log(x))



bootstrap_function <- function(input_data){
  random_indices <- sample(1:nrow(input_data), nrow(input_data))
  data_strap <- input_data[random_indices,]
  
  
  data_strap[1, 'new_taxa'] <- 
    ifelse(
      test = is.na(data_strap[1, 'Lowest_Classification']),
      yes = FALSE,
      no = TRUE
    )
  for(i in 2:nrow(data_strap)){
    data_strap[i, 'new_taxa'] <- 
      !(data_strap[i, 'Lowest_Classification'] %in% 
          data_strap[1:(i-1), 'Lowest_Classification'])
  }
  data_plot <- data_strap |> 
    # For each fish... (DROP Date_Diss!)
    group_by(Fish_ID) |> 
    # Add the number of new taxa
    summarize(new_taxa = sum(new_taxa)) |>
    ungroup() |> 
    mutate(
      # Note what number stomach it was
      n_stomachs = 1:n(),
      # and take the cumulative sum of new taxa
      new_taxa = cumsum(new_taxa)
    )
  
  return(data_plot)
}

replicate(
  n = 5,
  expr = bootstrap_function(bsb_month),
  simplify = FALSE
)




bootstrap_function <- function(input_data){
  
  random_sample <- sample(unique(input_data$Fish_ID),
                          length(unique(input_data$Fish_ID)))
  data_strap <- input_data |> 
    mutate(random_fish = factor(Fish_ID,
                                levels = random_sample,
                                ordered = T)
    ) |> 
    arrange(random_fish)
  
  data_strap[1, 'new_taxa'] <- 
    ifelse(
      test = is.na(data_strap[1, 'Lowest_Classification']),
      yes = FALSE,
      no = TRUE
    )
  for(i in 2:nrow(data_strap)){
    data_strap[i, 'new_taxa'] <- 
      !(data_strap[i, 'Lowest_Classification'] %in% 
          data_strap[1:(i-1), 'Lowest_Classification'])
  }
  
  data_plot <- data_strap |> 
    # For each fish... (DROP Date_Diss!)
    group_by(random_fish) |> 
    # Add the number of new taxa
    summarize(new_taxa = sum(new_taxa)) |>
    ungroup() |> 
    mutate(
      # Note what number stomach it was
      n_stomachs = 1:n(),
      # and take the cumulative sum of new taxa
      new_taxa = cumsum(new_taxa)
    ) |> 
  select(new_taxa, n_stomachs)
  
  return(data_plot)
}

boot_result <- replicate(
  n = 100,
  expr = bootstrap_function(bsb_month),
  simplify = FALSE
) |> 
  bind_rows(.id = 'iter')



ggplot(data = boot_result,
       aes(x = n_stomachs, y = new_taxa, group = iter),
       alpha = 0.1) +
  geom_step() + 
  geom_smooth(method = 'lm',
              formula = y ~ log(x),
              se = F)





bootstrap_function <- function(input_data){
  
  random_sample <- sample(unique(input_data$Fish_ID),
                          length(unique(input_data$Fish_ID)))
  data_strap <- input_data |> 
    mutate(random_fish = factor(Fish_ID,
                                levels = random_sample,
                                ordered = T)
    ) |> 
    arrange(random_fish)
  
  data_strap[1, 'new_taxa'] <- 
    ifelse(
      test = is.na(data_strap[1, 'Lowest_Classification']),
      yes = FALSE,
      no = TRUE
    )
  for(i in 2:nrow(data_strap)){
    data_strap[i, 'new_taxa'] <- 
      !(data_strap[i, 'Lowest_Classification'] %in% 
          data_strap[1:(i-1), 'Lowest_Classification'])
  }
  
  data_plot <- data_strap |> 
    # For each fish... (DROP Date_Diss!)
    group_by(random_fish) |> 
    # Add the number of new taxa
    summarize(new_taxa = sum(new_taxa)) |>
    ungroup() |> 
    mutate(
      # Note what number stomach it was
      n_stomachs = 1:n(),
      # and take the cumulative sum of new taxa
      new_taxa = cumsum(new_taxa)
    ) |> 
    select(new_taxa, n_stomachs)
  
  mod <- lm(new_taxa ~ log(n_stomachs), data = data_plot)
  
  coef(mod)
}

boot_result <- replicate(
  n = 100,
  expr = bootstrap_function(bsb_month),
  simplify = FALSE
) |> 
  bind_rows(.id = 'iter')

boot_pred <- lapply(
  split(boot_result, boot_result$iter),
  function(.){
    data.frame(
      n_stomachs = seq(min(bsb_plot$n_stomachs),
                       max(bsb_plot$n_stomachs),
                       length.out = 100),
      pred = 
        .$`(Intercept)` +
        .$`log(n_stomachs)` *
        log(seq(min(bsb_plot$n_stomachs),
                max(bsb_plot$n_stomachs),
                length.out = 100))
    )
  }) |> 
  bind_rows(.id = 'iter')

ggplot(data = boot_pred) +
  geom_line(aes(x = n_stomachs,
            y = pred,
            group = iter)) +
  geom_point(data = bsb_plot,
             aes(x = n_stomachs, y = new_taxa),
             color = 'red')
  


boot_cis <- boot_result |> 
  reframe(quant = c(0.025, 0.5, 0.975),
          int = quantile(`(Intercept)`, quant),
          log_sto = quantile(`log(n_stomachs)`, quant)
  )

n_sto_to_pred = seq(min(bsb_plot$n_stomachs),
                    max(bsb_plot$n_stomachs),
                    length.out = 100)

data.frame(
  n_stomachs = n_sto_to_pred,
  pred = boot_cis[boot_cis$quant == 0.5,]$int +
    boot_cis[boot_cis$quant == 0.5,]$log_sto *
    log(
      n_sto_to_pred
    ),
  
  lci = boot_cis[boot_cis$quant == 0.025,]$int +
    boot_cis[boot_cis$quant == 0.025,]$log_sto *
    log(
      n_sto_to_pred
    ),
  
  uci = boot_cis[boot_cis$quant == 0.975,]$int +
    boot_cis[boot_cis$quant == 0.975,]$log_sto *
    log(
      n_sto_to_pred
    )
) |> 
  ggplot(aes(x = n_stomachs)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'lightgray') +
  geom_line(aes(y = pred)) +
  geom_point(data = bsb_plot, aes(x = n_stomachs, y = new_taxa))+
  geom_line(data = boot_pred,
            aes(x = n_stomachs,
                y = pred,
                group = iter),
            color = 'red',
            alpha = 0.2) +
  # ggplot(data = mod_pred, aes(x=n_stomachs)) +
  geom_ribbon(data = mod_pred,
              aes(x=n_stomachs, ymin = lci, ymax = uci),
              fill = 'blue',
              alpha = 0.2) +
  geom_line(data = mod_pred,
             aes(x=n_stomachs, y = pred),
            linetype = 'dashed')





mod <- lm(new_taxa ~ log(n_stomachs), data = bsb_plot)
# predict for just our data
mod_pred <- predict(mod, 
        bsb_plot
)
plot(x = bsb_plot$n_stomachs, y = mod_pred)

dumb_mod <- lm(new_taxa ~ n_stomachs,
   data = bsb_plot[8:12,]) 
dumb_mod <- dumb_mod|> 
  predict(se.fit = T)
dumb_mod <- data.frame(
  fit = dumb_mod$fit,
  lci = dumb_mod$fit - 1.96*dumb_mod$se.fit,
  uci = dumb_mod$fit + 1.96*dumb_mod$se.fit,
  n_stomachs = 8:12
)

ggplot(data = boot_pred) +
  geom_line(aes(x = n_stomachs,
                y = pred,
                group = iter),
            alpha = 0.2) +
  geom_point(data = bsb_plot,
             aes(x = n_stomachs, y = new_taxa),
             color = 'red') +
  geom_ribbon(data = dumb_mod,
              aes(x = n_stomachs, ymin = lci, ymax= uci), fill = 'blue',
              alpha = 0.3) +
  geom_line(data = dumb_mod,
            aes(x = n_stomachs, y = fit),
            color = 'blue') +
  geom_hline(yintercept = 3.2)
