library(dplyr) # package for data manipulation
library(ggplot2) # package for plotting

# Read in data (this is where it's located on my computer, saved as CSV)
bsb_diet <- read.csv(
  'data/Black Sea Bass Analysis - 2023.csv',
  # just so happens that NAs are listed as nothing ('') rather than NA
  na.strings = '' 
)

bsb_pot <- bsb_diet |> 
  # filter the data for "pot" in the Survey column: Survey EQUALS 'pot'
  filter(Survey == 'pot') |> 
  # mutate makes new columns (MUTATES the data)
  mutate(
    # Tell R that the Date_Diss column is a date in month-day-year format
    Date_Diss = as.Date(Date_Diss, format = '%m-%d-%y'),
    # Create a "new taxa" dummy column for later
    new_taxa = FALSE
  ) |> 
  # Sort the data by date dissected
  arrange(Date_Diss) |> 
  # Select Fish, prey, and the dummy column
  select(Fish_ID, Lowest_Classification, new_taxa, Date_Caught, Date_Diss)

View(bsb_pot)

# We're going to "loop", which is a little more advanced so don't worry too much
#   about it

# In R, a data set is indexed as my_data[ROW, COLUMN]. So if we wanted the value
#   in the cell of the first row and 3rd column it'd be bsb_pot[1,3]
#   Excel indexes by numeric rows and alphabetic columns, so the analogous cell would
#   be "C1 in sheet bsb_pot"

# We're going to go row-by-row and check if the value of "Lowest_Classification"
#   is in any of the rows before it.
#   We'll start at row 2 and ask if it has the same value as row 1.
#   Then row 3 and see if it is in row 1 or 2. Then 4 and if it's in 1, 2, or 3...
for(i in 2:nrow(bsb_pot[grepl('^6-27', bsb_pot$Date_Caught),])){
  # Assign to the i-th row of the "new_taxa" column "TRUE" if it is new (wasn't
  # in the previous rows) or FALSE if it was in the previous rows. In code speak...
  
  
  bsb_pot[grepl('^6-27', bsb_pot$Date_Caught),][i, 'new_taxa'] <- 
    # if the ith row IS in the previous rows (X %in% Y), we want the opposite,
    # so we use the "NOT" operator: "!"
    !(bsb_pot[grepl('^6-27', bsb_pot$Date_Caught),][i, 'Lowest_Classification'] %in% bsb_pot[grepl('^6-27', bsb_pot$Date_Caught),][1:(i-1), 'Lowest_Classification'])
  
  # Now the loop will restart and increase "i" by 1 to refer to the next row in 
  #   the series
}

# View(bsb_apr)

# Now we want to summarize the data

bsb_jun <- bsb_pot[grepl('^4', bsb_pot$Date_Caught),] |> 
  # For each fish...
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

# View(bsb_pot)

# now plot!

ggplot(data = bsb_jun, aes(x = n_stomachs, y = new_taxa)) +
  geom_step() +
  labs(title = '4-11 and 4-13-23') +
  geom_smooth()

# You can see that we're starting to level off at around 15 taxa after about 50
#   stomachs. This is for the WHOLE data set, though.

# In order to get an idea of how diet is changing per month, we want to do this
#   not for the whole data set, but per month. That's your homework!
