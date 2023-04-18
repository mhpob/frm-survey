library(readxl)

excel_sheets('data/hobo/21425201 2023-04-14 11_02_23 EDT.xlsx')

h201 <- read_excel('data/hobo/21425201 2023-04-14 11_02_23 EDT.xlsx',
                  sheet = 'Data',
                  range = cell_cols(2:3))

names(h201) <- c('datetime', 'tempC')
#201 was every 5s. Excessive


library(ggplot2)
ggplotly(
ggplot(data = h201) +
  geom_line(aes(x = datetime, y = tempC))
)


h200 <- read_excel('data/hobo/21425200 2023-04-14 11_14_50 EDT.xlsx',
                   sheet = 'Data',
                   range = cell_cols(2:3))
names(h200) <- c('datetime', 'tempC')


ggplotly(
ggplot(data = h200) +
  geom_line(aes(x = datetime, y = tempC))
)
