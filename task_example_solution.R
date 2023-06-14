
### Research question: Is there a positive correlation between minimum wage and 
### your experienced financial security, in EU-countries from 2015 until 2020?
# Insert what is missing in the following code 


#########################
######## IMPORT #########
#########################


install.packages("eurostat") # Installing the package "eurostat"

## Eurostat is the statistical office of the European Union. 
## They have made their own R-package which we can use to gather data directly from their database with the use of code
## For a full overview of their database, see https://ec.europa.eu/eurostat/web/main/data/database 

library(eurostat) # Open the package 

# We use the function get_eurostat() to upload data from the Eurostat package to R.
# Make sure you are connected to wifi when you run the code. 

# Give the datasets we upload the following names: unexp, pop, and minwage.

unexp <- get_eurostat("ilc_mdes04") # Percentage of population with inability to face unexpected financial expenses
pop <- get_eurostat("demo_pjan") # Population on 1 January by age and sex
minwage <- get_eurostat("earn_mw_cur") # Monthly minimum wages - bi-annual data


###################################
######## TIDY & TRANSFORM #########
###################################

## 1. Get an overview of the data

head(unexp) # Printing the six first rows of the dataset unexp.
glimpse(unexp) # Have a look at the type of variables, and the number of rows and columns in the dataset unexp.

head(pop) # Printing the six first rows of the dataset pop.
glimpse(pop) # Have a look at the type of variables, and the number of rows and columns in the dataset pop.

head(minwage) # Printing the six first rows of the dataset minwage
glimpse(minwage) # Have a look at the type of variables, and the number of rows and columns in the dataset minwage.

names() # Put one of the datasets into this function, what does the function tell us about the dataset?
# We see the variable names (column names) in the dataset

nrow() # Put another dataset into this function, what does the function tell us about the dataset?
# We see the number of observations (number of rows) in the dataset. 

## 2. Missing values

unexp %>%
  complete.cases() # Print a table that shows us all units with missing values on at least one variable.  

minwage %>%
  select(values) %>% # Get the variable "values" from the dataset.
  is.na() %>%
  table() # Count the number of units which are missing on the variable "values".

## 3. Are the variables useful? 

## the time-variable 

table(unexp$time) # Print the values of the variable "time" from the dataset "unexp".
# What does this variable tell us? Is it useful?
# Is it important for us to know that the data is for the 1st of January each year, or only that its valid for that year?
# Do we need data for all years? (see the research question at the top of the script)

unexp_reduced <- unexp %>% # change the variable "time" with the function ifelse() so it only contains the year, and not the date. 
  mutate(time = ifelse(time == "2015-01-01", "2015",
                       ifelse(time == "2016-01-01", "2016", 
                              ifelse(time == "2017-01-01", "2017",
                                     ifelse(time == "2018-01-01", "2018",
                                            ifelse(time == "2019-01-01", "2019",
                                                   ifelse(time == "2020-01-01", "2020", 
                                                          time))))))) %>%
  filter(time %in% c("2015", "2016", "2017", "2018", "2019", "2020")) # Get the rows with the years 2015, 2016, 2017, 2018, 2019 and 2020. 

table(unexp_reduced$time) # Check if your code is correct. Remember that we have now made a new dataset with a new name. 

table(pop$time) # Check if the dataset "pop" has it's time-variable coded the same way as in the dataset unexp. 

pop_reduced <- pop %>%
  mutate(time = substr(time, 1, 4)) # Make a new variable "time" in the dataset "pop" with only year, not date
# suggestion for a more effective way to do this: use the function substr() to get the first four characters in a character vector. 

pop_reduced <- pop_reduced %>%
  filter(time %in% c("2015", "2016", "2017", "2018", "2019", "2020")) # Get only the years we need


# The minwage-dataset is different. It is biannual, which means that it's for every other year. 
table(minwage$time) 

# Since the other datasets we have are for years, this dataset also need to be in years for us to compare them. 
# Therefore we get the rows with minimum wage for the 1st of January. 

# Make sure the following code is in the right order:
minwage_reduced <- minwage %>% 
  mutate(time = as.character(time)) %>%
  filter(!time %in% c("2020-07-01", "2019-07-01", "2018-07-01", "2017-07-01", "2016-07-01", "2015-07-01")) %>%
  mutate(time = substr(time, 1, 4)) %>%
  filter(time %in% c("2015", "2016", "2017", "2018", "2019", "2020"))


## the value-variable

class(unexp_reduced$values) # Find the class of the variable "values" in the unexp-dataset. Comment what the class is. 
# numeric 

max(unexp_reduced$values, na.rm = TRUE) # Find the maximum value of the variable "values" in the dataset unexp.
min(unexp_reduced$values, na.rm = TRUE) # Find the minimum value of the variable "values" in the dataset unexp.
# What does this varible tell us?

head(unexp_reduced)
table(unexp_reduced$unit)
# Run the code above and look at the variable "unit". It has the value "PC". This indicates that the variable "values" is given in percentage. 
# So the "values" show the percentage in each EU-country ("geo") who says that they could not manage a sudden financial cost.
# (according to household type ("hhtyp") and income group ("incgrp")).

# Is "values" a useful name for the variable?

unexp_reduced <- unexp_reduced %>%
  rename(unexp_percent = values) # Change the name of the variable from "values" to "unexp_percent". 


# Check if the datasets pop and minwage also have variables called "values". 


glimpse(pop_reduced) # What type of value do you think the "values"-variable is given in? Comment below. 
table(pop_reduced$unit)
# NR - which stands for number

glimpse(minwage_reduced)
table(minwage_reduced$currency)
# EUR, NAC and PPS. Different currencies. 


# Change the name of these variables as well.
pop_reduced <- pop_reduced %>%
  rename(n_people = values) 

minwage_reduced <- minwage_reduced %>%
  rename(minwage_eur = values) 


## 4. Are the observations useful?

# What are the units in our research question? Which variable differentiate between the units in the dataset unexp? Comment below. 

glimpse(unexp_reduced)
# EU-countries, they are in the geo-variable.
# The units are grouped by two variables: hhtyp og incgrp. 

# Which values are in the hhtyp-variable (household type)?
table(unexp_reduced$hhtyp)

# Which values are in the incgrp-variable (income group)?

table(unexp_reduced$incgrp)


# Get only the row with the value "TOTAL" from the incgrp-variable. 

unexp_reduced <- unexp_reduced %>%
  filter(incgrp == "TOTAL")

# Get from the hhtyp-variable only the rows with the values "A1F", "A1M", "A2" and "TOTAL". 

unexp_reduced <- unexp_reduced %>%
  filter(hhtyp %in% c("A1F", "A1M", "A2", "TOTAL"))

# Now the variables "incgrp" and "unit" have the same values, and therefore does not give us more useful information. 
# Remove the variables "incgrp" and "unit" from the dataset. 


unexp_reduced <- unexp_reduced %>%
  select(hhtyp, geo, time, unexp_percent)


# We tidy the pop-dataset and the minwage-dataset in the same way. Fill in the code below so it's correct. 

pop_reduced <- pop_reduced %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T") %>%
  select(geo, time, n_people)


minwage_reduced <- minwage_reduced %>%
  filter(currency == "EUR") %>%
  select(-(currency)) # Minus with select() removes the variables


# 5. Descriptive statistics 

mean(unexp_reduced$unexp_percent, na.rm = TRUE) # Find the average percentage who says that they could manage a sudden financial cost. 

log(pop_reduced$n_people) # Change the variable n_people (population size) to log. 


# Find the average percentage who says they could not manage a sudden financial cost, grouped by year and country 
unexp_reduced %>%
  group_by(geo, time) %>% 
  summarise(unexp_pc_mean = mean(unexp_percent))



#####################################
########### VISUALIZE ###############
#####################################

# Make a boxsplot with unexp_percent (percentage with financial insecurity) on the x-axis. 
# geo (country) on the y-axis and colour by hhtyp (household type)
# What does this plot tell us?

unexp_reduced %>%
  ggplot(aes(x = unexp_percent, y = geo, color = hhtyp)) + 
  geom_boxplot() 

# The plottet whows us the percentage who says they could not manage a sudden financial cost over the year 2015-2020 according to country. 
# The line in the middle shows the median of the percentage, the box indicates the 1st quantile and the 3rd quantile, and the line is the data's "range". 
# The dots are outliers, and the boxes have different colours according to type of household.


# Filter out only the countries FR (France), DE (Germany), ES (Spain), BE (Belgium), NO (Norway), SE (Sweden) and DK (Denmark), then plot again.
# Recode the values on the hhtyp to something that is more understandable. 

unexp_reduced %>%
  filter(geo %in% c("FR", "DE", "ES", "BE", "NO", "SE", "DK")) %>%
  mutate(hhtyp = ifelse(hhtyp == "A1F", "Enslig kvinne",
                        ifelse(hhtyp == "A1M", "Enslig mann",
                               ifelse(hhtyp == "A2", "To voksne", hhtyp)))) %>%
  ggplot(aes(x = unexp_percent, y = geo, color = hhtyp)) + 
  geom_boxplot() 

# Which country has the highest percentage who reports financial insecurity?
# Spain

# Which country has the lowest percentage who reports financial insecurity?
# Norway

# Which type of household has the highest percentage who reports financial insecurity?
# Single women

# What is the approximate median of the percentage who reports financial insecurity among households with two adults in Germany?
# 20 percent

# Plot the minimum wage for the European countries over time with a line diagram. Colour the dots by country. 
# What do this plot tell us? 

minwage_reduced %>%
  ggplot(aes(x = time, y = minwage_eur, group = geo, color = geo)) + 
  geom_line() +
  labs(x = "", y = "Minstelønn")

# There has been a slight increase in the minimum wage in most European countries from 2015 to 2020. 
# (This is probably because the varible is in euro which is not adjusted for the change in prices). 
# We could adjust for change in prices, but then we have to download another dataset from the Eurostat-database
# which contains the price index for each country, and then adjust the minimum wage by the price index by adding them together. 


## Join the datasets together 

# Look at the datasets below. Which variables do they have in common (keys)?

head(unexp_reduced)
head(pop_reduced)
head(minwage_reduced)
# geo and time

df <- unexp_reduced %>%
  filter(hhtyp == "TOTAL") %>% # Only filter out the "TOTAL" from the hhtyp-variable, this gives us the financial insecurity for all household types. 
  left_join(pop_reduced, by = c("geo", "time")) %>%  # join with the pop_reduced dataset. 
  left_join(minwage_reduced, by = c("geo", "time")) # join with the minwage_reduced dataset. 


# Use the joined datasets to plot the relationship between minimum wage and financial insecurity in the European countries. 
# Colour them by country
# Add a linear regression line
# What do this plot tell us?


df %>%
  ggplot(aes(x = minwage_eur, y = unexp_percent)) + 
  geom_point(aes(color = geo)) + 
  geom_smooth(method = "lm")
# There is a negative bivariate correlation between financial insecurity and minimum wage. 
# The lower the minimum wage, the higher the financial insecurity for European countries between 2015 and 2020.


#####################################
############# MODEL #################
#####################################

# Make a linear model with unexp_percent as the dependent variable 
# and minwage_eur, n_peopleand time as independent variables. 
# Make time as a categorical variable

#Oppgi time som en kategorisk variabel (faste effekter / dummy-variable). 

model1 <- lm(unexp_percent ~ minwage_eur + n_people + factor(time), 
              na.action = "na.exclude",
              data = df)

summary(model1) # Make a summary of the model. 

library(stargazer) # Open the stargazer-package. 

stargazer(model1, # Make a nice table of model1
          type = "text") # Print the table in console


## Plot the effect of model1

# Make a vector in which the minwage_eur-variable varies between the minimum value and the maximum value
minwage_eur_vektor = c(seq(min(df$minwage_eur, na.rm = TRUE),
                           max(df$minwage_eur, na.rm =TRUE), 
                           by = 10)) # Let the variable vary in an interval of 10

n_people_vektor <- mean(df$n_people, na.rm = TRUE) # Make a vector with the average of n_people (population size) 

time_vektor <- "2017" # Make a vector with the with the year 2017


# Put all the vectors into one dataset. Name the variables minwage_eur, n_people and time. What type of constructed world is this?
snitt_data <- data.frame(minwage_eur = minwage_eur_vektor,
                         n_people = n_people_vektor,
                         time = time_vektor) 
# This is a world in which we have 193 countries (the number of rows in the dataset snitt_data), 
# The year is 2017, all countries have 62.678.031 inhabitants, and the minimum wage is between 162,69 euro and 2082,69 euro.


# Use the dataset snitt_data to predict the percentage with financial insecurity with the model you made.
pred_verdier <- predict(modell1, 
                        newdata = snitt_data, 
                        se = TRUE, interval = "confidence")


# Add the vectors which was made with predict() in the dataset snitt_data, so we have new columns.
snitt_data <- cbind(snitt_data, pred_verdier)


# What is the predicted percentage with financial insecurity for a country in 2017 with 62.678.031 inhabitants and minimum wage of 412.69 euro?

snitt_data %>%
  filter(minwage_eur == 412.69) %>%
  select(fit.fit)
# 42 percent


# What is the predicted percentage with financial insecurity for a country in 2017 with 62.678.031 inhabitants 
# and minimum wage at the average of the minwage-vector?

snitt_data %>%
  filter(minwage_eur == mean(minwage_eur_vektor)) %>%
  select(fit.fit)
# 32 percent

# What is the lower confidence interval of the predicted percentage with financial insecurity for a country in 2017 with 62.678.031 inhabitants
# and minimum wage at the maximum of the minwage-vector?

snitt_data %>%
  filter(minwage_eur == max(minwage_eur_vektor)) %>%
  select(fit.lwr)
# 13 percent

# Plot the impact of minimum wage on financial insecurity.
snitt_data %>%
  ggplot(aes(x = minwage_eur, y = fit.fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = fit.lwr, 
                  ymax = fit.upr), 
              alpha = .2) + 
  labs(x = "Minstelønn", y = "Forventet finansiell usikkerhet")


# Concerning what we have done so far, do you think this is a good model to use to answer our research question (see the top of the script)?
# Is there something you would have done different?

# Yes! For example: 
# Include more independent variables to avoid omitted variable bias.
# Create an index for financial insecurity with more variables. 
# adjust minimum wage for the increase in prices. 
# Include more years.
# Check the conditions for OLS.
# Get data for individuals' financial insecurity on micro level and run a multivariate analysis.
