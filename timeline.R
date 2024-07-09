library(data.table)

timeline <-
'project,   stdate,       enddate,    pam,  telem
 MarMam,    2014-11-01,   2017-11-01, T,    T
 BSB,       2016-06-12,   2016-11-02, F,    T
 WindFish,  2016-11-01,   2018-12-01, F,    T
 BSB,       2017-06-22,   2017-10-26, F,    T
 BSB,       2018-07-01,   2018-10-26, T,    T
 PALS,      2019-06-07,   2019-10-08, T,    T
 TailWinds, 2023-04-01,   2024-06-01, T,    T' |> 
  fread(text = _) |> 
  _[, project := factor(project,
                        levels = rev(unique(project)),
                        ordered = TRUE)]
 
library(ggplot2)

proj_gantt <- ggplot(data = timeline) +
  geom_segment(aes(x = stdate, xend = enddate, y = project,
                   color = project),
               linewidth = 4, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text = element_text(size = 18)) +
  labs(x = NULL, y = NULL)

timeline_melt <- melt(timeline, measure.vars = c('pam', 'telem')) |> 
  _[, variable := ifelse(variable == 'pam', 'PAM', 'AT')]

gear_gantt <- ggplot(data = timeline_melt[value == 'T']) +
  geom_segment(aes(x = stdate, xend = enddate, y = variable),
               linewidth = 4, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text = element_text(size = 18)) +
  labs(x = NULL, y = NULL)

library(patchwork)

proj_gantt / gear_gantt + 
  plot_layout(axes = 'collect')
