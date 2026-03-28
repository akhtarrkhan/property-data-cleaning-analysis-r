# =========================================================
# Property Data Cleaning and Analysis
# =========================================================

library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(writexl)
library(openxlsx)

# Loading two datasets called "PropertyDataset1.csv" and "PropertyDataset2.csv"

PropertyDataset1 <- read.csv("C:/Users/njfbts/Downloads/PropertyDataset1.csv")
View(PropertyDataset1)

PropertyDataset2 <- read.csv("C:/Users/njfbts/Downloads/PropertyDataset2.csv")
View(PropertyDataset2)

# Question 1 - [Using PropertyDataset1.csv]

# Basic Cleaning before proceeding with the questions

names(PropertyDataset1)

# Putting the original dataset in a new data frame along with exclusion of first two columns(redundant)
# that listed only serial numbers

PDF_1 <- PropertyDataset1[, c(
  "geohash", "sale_date", "price", "post_code", "county",
  "property_description", "property_size_description",
  "formatted_description", "not_full_market_price",
  "vat_exclusive", "parking", "parking1", "bedrooms_bathrooms"
)]

# Put original data frame in a new df so incase we make some error with data, we have original backed-up
pdf1 <- PDF_1

# Investigating the dataset using the str(structure function) which gives information on the data type
# of all the columns
# Now we can coerce the data types(numeric, date, character) into the required formats for analysis so
# we can perform numeric calculations and time-series analysis

head(pdf1)
str(pdf1)
summary(pdf1)
colSums(is.na(pdf1))

# head func gives a gist into data
# We see missing geohash data, property_description & formatted_description columns have conflicting property type
# Summary function shows us outliers in price like a property listing of 0 price and another is extremely high being 69 mil
# colSums(is.na(pdf1)) doesn't take the blank columns into account while calculating. Need to convert the same to NAs first.

# Converting all the blanks to NAs for convenience using across function and defining to convert all the blanks
# where the data type is character as numeric and date columns do that automatically

pdf1 <- pdf1 %>%
  mutate(across(.cols = where(is.character), ~ na_if(.x, "")))

colSums(is.na(pdf1))

# Date column is in character format(basis str function) hence coercion of the format using lubridate package.

pdf1$sale_date <- dmy(pdf1$sale_date)

# Standardizing casing: converting the values into lower casing using tolower
# trimming any extra white space from the values
# Executing the above steps so we have a standard formatting of casing and we can execute functions
# without creating duplicates/or having errors due to casing

pdf1 <- pdf1 %>%
  mutate(across(where(is.character), ~ tolower(str_trim(.x))))

names(pdf1) <- str_to_title(names(pdf1))

# The property_description model lists all the properties as apartments hence we will rely on
# formatted description column for property type while derive property age from property description

pdf1 <- pdf1 %>%
  mutate(
    Property_age = case_when(
      str_detect(Property_description, "new") ~ "new",
      str_detect(Property_description, "second-hand") ~ "second-hand",
      TRUE ~ "unknown"
    )
  )

# Creating three new columns containing month, year and quarter for better classification in future
# month, year & quarter from lubridate help extract the same from Sale_date column
# This step will help us creating time-series analysis and studying trend over time.
# Data formats need to be aligned so the model know how to treat each variable

pdf1 <- pdf1 %>%
  mutate(
    Sale_month = month(Sale_date, label = TRUE),
    Sale_year = year(Sale_date),
    Sale_quarter = paste0("Q", quarter(Sale_date))
  )

# bedroom_bathroom column: splitting the column in two and creating numeric values for each along with NAs
# using na_if function which converts any particular value/blank to NA as specified hence getting rid of all the '/'
# from bedrooms_bathrooms column
# separating the bedroom & bathroom count using the separator '/' between them and in case there is only bedroom
# count data present in our column, then using 'fill = right' to fill bathroom column with NA
# Finally using parse_number function from readr package which extracts the first numeric value it gets and drops the
# remaining string
# Carrying out the above steps so we can have numerical calculations basis the bedroom/bathroom attributes

unique(pdf1$Bedrooms_bathrooms)

pdf1 <- pdf1 %>%
  mutate(Bedrooms_bathrooms = na_if(Bedrooms_bathrooms, "/")) %>%
  separate(Bedrooms_bathrooms, into = c("Bedrooms", "Bathrooms"), sep = "/", fill = "right") %>%
  mutate(
    Bedrooms = parse_number(Bedrooms),
    Bathrooms = parse_number(Bathrooms)
  )

# Standardizing values in multiple columns
# Using 'unique' function and mutate to standardize values in the columns
# Unique will help us with all the distinct values in the column

# Parking
unique(pdf1$Parking)

# Cleaning data
# Using case_when(multiple ifelse statements) functions where multiple conditions can be defined and conditions executed.
# Since there are multiple values("has", "no", NA, "has parking") denoting the same thing under the parking columns,
# hence updating the column with only yes and no values to simplify the data.
# Replacing blanks with NA and if still anything remains, we will retain the default value to check what that value is.

pdf1$Parking <- case_when(
  pdf1$Parking %in% c("has", "has parking") ~ "yes",
  pdf1$Parking %in% c("no") ~ "no",
  pdf1$Parking == "" ~ NA_character_,
  TRUE ~ pdf1$Parking
)

# parking1 column has details of the parking available which are NA in our Parking column hence imputing data from there

select(pdf1, Parking, Parking1)

# is.na func is giving true for all the na values basis which a subset is created by filter function
# and then later Parking and Parking1 called basis select

pdf1 %>%
  filter(is.na(Parking)) %>%
  select(Parking, Parking1)

# Imputing the NAs in parking with Parking1

pdf1 <- pdf1 %>%
  mutate(
    Parking = ifelse(
      is.na(Parking) & Parking1 == "has parking",
      "yes",
      Parking
    )
  )

# Cleaning county column
# observed that the same county is appearing in multiple columns as a new entry due to casing/spelling/spacing
# using sort & unique function

sort(unique(pdf1$County))

pdf1 <- pdf1 %>%
  mutate(
    County = case_when(
      County == "dubln" ~ "dublin",
      County == "tipp" ~ "tipperary",
      County == "ros comain" ~ "roscommon",
      TRUE ~ County
    )
  )

sort(unique(pdf1$County))

# sorting data using sale date

pdf1 <- arrange(pdf1, Sale_date)

# Outliers can skew averages while imputing or introduce bias / mislead our model
# Hence it is important to identify and validate them

pdf1_price_outlier <- pdf1 %>%
  filter(Price == 0 | Price >= 69000000)

# gc1zpr geohash property is the one with 69 mil price but the same seems to be invalid as the same geohash
# is present multiple times in the data with average price of those listings no more than Eur 168,182
# Hence dropping the said row

gc6866 <- filter(pdf1, Geohash == "gc6866")

sixty_nine_mil <- pdf1 %>%
  filter(Geohash == "gc1zpr" & Price < 69000000)

summary(sixty_nine_mil)

# Following the same steps for zero price properties
# These are all errors since the same geohash is present multiple times with price listed
# Hence it is a valid approach to drop these outlier rows

zeromil <- pdf1 %>%
  filter(Geohash == "gc7wn4" & Price < 69000000)

filter(pdf1, Geohash == "gc6866")

pdf1 <- pdf1 %>%
  filter(Price > 0 & Price < 69000000)

# Outlier in date column spotted which are future sale dates beyond 2025
# If x was not defined as blank, we would get a negative range on the axis which would cause confusion

ggplot(pdf1, aes(x = "", y = Sale_date)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 3) +
  labs(
    title = "Future Sale Date Outliers",
    x = "",
    y = "Sale Date"
  )

# Two listings found with future sale date. Dropping the same. Other listings of same geohash present with valid data

future_outliers <- pdf1 %>%
  filter(Sale_date > "2025-01-01")

filter(pdf1, Geohash == "gc7x20")

pdf1 <- pdf1 %>%
  filter(Sale_date < "2025-11-01")

# Checking property_size_description if we can standardize the values there
# Since we do not have precise/continuous value for size, we are going to create categories as size is an important
# factor influencing price
# Using str_detect func which checks if a pattern/value exists inside a string

unique(pdf1$Property_size_description)

pdf1 <- pdf1 %>%
  mutate(
    Property_size_cat = case_when(
      str_detect(Property_size_description, "less than 38") ~ "Small(<38)",
      str_detect(Property_size_description, "38") & str_detect(Property_size_description, "125") ~ "Medium(38-125)",
      str_detect(Property_size_description, "greater than 125") ~ "Large(>=125)",
      str_detect(Property_size_description, "equal to 125") ~ "Large(>=125)",
      TRUE ~ NA_character_
    )
  )

# Standardizing the values to "Yes", "No" & "NA" in columns "Not_full_market_price" and "Vat_exclusive"

pdf1 <- pdf1 %>%
  mutate(
    Not_full_market_price = case_when(
      Not_full_market_price == "true" ~ "yes",
      Not_full_market_price == "false" ~ "no",
      TRUE ~ NA_character_
    ),
    Vat_exclusive = case_when(
      Vat_exclusive == "true" ~ "yes",
      Vat_exclusive == "false" ~ "no",
      TRUE ~ NA_character_
    )
  )

# extracting actual property type from formatted_description - this adds more depth to our data

pdf1 <- pdf1 %>%
  mutate(
    Property_type = case_when(
      str_detect(Formatted_description, "apartment") ~ "apartment",
      str_detect(Formatted_description, "semi") ~ "semi-detached",
      str_detect(Formatted_description, "detached") & !str_detect(Formatted_description, "semi") ~ "detached",
      str_detect(Formatted_description, "end of terrace") ~ "end-of-terrace",
      str_detect(Formatted_description, "terraced") ~ "terraced",
      str_detect(Formatted_description, "duplex") ~ "duplex",
      str_detect(Formatted_description, "bungalow") ~ "bungalow",
      str_detect(Formatted_description, "townhouse") ~ "townhouse",
      str_detect(Formatted_description, "site") ~ "site",
      str_detect(Formatted_description, "house") ~ "house",
      TRUE ~ NA_character_
    )
  )

# No further issues observed with the data than the ones defined above hence dropping redundant columns
t(summary(pdf1))

pdf1 <- select(pdf1, -Parking1, -Property_description, -Property_size_description, -Formatted_description)

write_xlsx(pdf1, "C:/Users/njfbts/Downloads/pdf1.xlsx")

# checking duplicate geohash

count(pdf1, Geohash, sort = TRUE)

# checking a single Geohash to test out the theory that missing details can be imputed from other rows of same geohash entry

filter(pdf1, Geohash == "gc7rtx")

# With the assumption that same geohash apartments share similar attributes, we checked that the same listing may
# have missing details in some other listing.
# While price column cannot be imputed like that - but other values like bedrooms, county etc can be as the difference
# between them being stark is highly unlikely.
# Using geohash to populate columns. Created two new dfs/subsets of main pdf1. na_bath contains all the bathrooms
# where the value is missing but geohash is present.
# non_na_bath contains rows with bathroom values - rest is same as above
# Now we'll join these two dfs on geohash column which will give us a nice view to compare all the duplicate geohash
# values side by side.
# We can see that the property with the same geohash value which was missing values in some row might have values
# stored in a different row for the same geohash
# Hence will be filling up the table basis that.
# Subset used to create specific dfs and merge used to join the two dfs and suffix used to add "_na" and "_filled"
# to distinguish between the same column names from different dfs

na_bath <- subset(pdf1, !is.na(Geohash) & is.na(Bathrooms))
non_na_bath <- subset(pdf1, !is.na(Geohash) & !is.na(Bathrooms))
pairs_bath <- merge(na_bath, non_na_bath, by = "Geohash", suffixes = c("_na", "_filled"))

# Taking a look how the data looks in practical before going ahead with the application

names(pairs_bath)
select(pairs_bath, "Geohash", "Bedrooms_na", "Bathrooms_na", "Bedrooms_filled", "Bathrooms_filled")

# Time to impute data
# Grouping the same geohashs together to update the columns that can be imputed without harming the sale data
# Then the condition is go through each value(.x) of each group, and if it is NA(is.na) in that group then remove the NA(na.omit)
# And impute the first value(first) from that group and if is.na is false, then let the original value be

pdf1 <- pdf1 %>%
  group_by(Geohash) %>%
  mutate(
    across(
      c("Post_code", "Not_full_market_price", "Bedrooms", "Bathrooms", "Property_size_cat", "Property_type"),
      ~ ifelse(is.na(.x), first(na.omit(.x)), .x)
    )
  ) %>%
  ungroup()

# 60-80% columns updated with values

colSums(is.na(pdf1))

# Getting rid of the absolute duplicate rows where everything is the same.
# As this can create bias for the model and cause it to believe a pattern is popular when it is just duplicacy.

pdf1 <- pdf1 %>%
  distinct()

# Imputing missing bedrooms, bathrooms and property size using mode on county level
# These will add value but not change the meaning
# table func counts Bedroom in each county
# which.max finds the index of the highest count
# names converts the index back to the original bedroom number

pdf1 <- pdf1 %>%
  group_by(County) %>%
  mutate(
    Bedrooms = ifelse(
      is.na(Bedrooms),
      as.numeric(names(which.max(table(Bedrooms)))),
      Bedrooms
    )
  )

pdf1 <- pdf1 %>%
  group_by(County) %>%
  mutate(
    Bathrooms = ifelse(
      is.na(Bathrooms),
      as.numeric(names(which.max(table(Bathrooms)))),
      Bathrooms
    )
  )

pdf1 <- pdf1 %>%
  group_by(County) %>%
  mutate(
    Property_size_cat = ifelse(
      is.na(Property_size_cat),
      names(which.max(table(Property_size_cat))),
      Property_size_cat
    )
  )

##################################################################
###################################################################################################

# Question 2 - Cleaning PropertyDataset2.csv and merging with pdf1_updated

PDF_2 <- PropertyDataset2

# Taking a quick look at column names and basic structure

names(PDF_2)
head(PDF_2)
str(PDF_2)
summary(PDF_2)
colSums(is.na(PDF_2))

# Putting the original dataset in a new data frame with only the columns required for further cleaning
# Put original data frame in a new df so incase we make some error with data, we have original backed-up

pdf2 <- PDF_2[, c(
  "Date.of.Sale..dd.mm.yyyy.",
  "County",
  "Not.Full.Market.Price",
  "VAT.Exclusive",
  "Description.of.Property",
  "Property.Size.Description",
  "County_Price"
)]

# Converting blanks to NAs
pdf2 <- pdf2 %>%
  mutate(across(.cols = where(is.character), ~ na_if(.x, "")))

# Character cleaning - trimming spaces and converting to lower case
# This is to make the categories consistent and avoid duplicate categories due to casing/spacing issues

pdf2 <- pdf2 %>%
  mutate(across(where(is.character), ~ tolower(str_trim(.x))))

# Formatting the header of dataset by uppercasing the first alphabet of the word
# to better distinguish between headers and values - same as done for pdf1

names(pdf2) <- str_to_title(names(pdf2))

# Coercing sale date from character in "dd/mm/yyyy" format to Date format using lubridate::dmy

pdf2$Sale_date <- dmy(pdf2$Date.of.Sale..dd.mm.yyyy.)

# Creating Sale_month, Sale_year, quarter from Sale_date
# Using month(), year(), quarter() from lubridate to create month and year columns

pdf2 <- pdf2 %>%
  mutate(
    Sale_month = month(Sale_date, label = TRUE),
    Sale_year = year(Sale_date),
    Sale_quarter = paste0("Q", quarter(Sale_date))
  )

# Dropping the original raw date column as we now have a clean Sale_date column
pdf2 <- select(pdf2, -Date.of.Sale..dd.mm.yyyy.)

# Extracting property age as we did in dataset 1

pdf2 <- pdf2 %>%
  mutate(
    Property_age = case_when(
      str_detect(Description.of.Property, "new") ~ "new",
      str_detect(Description.of.Property, "second-hand") ~ "second-hand",
      TRUE ~ "unknown"
    )
  )

# Renaming columns to match the naming convention used in pdf1
pdf2 <- pdf2 %>%
  rename(
    Not_full_market_price = Not.Full.Market.Price,
    Vat_exclusive = VAT.Exclusive
  )

# Standardizing the categories from Property.Size.Description column

pdf2 <- pdf2 %>%
  mutate(
    Property_size_cat = case_when(
      str_detect(Property.Size.Description, "less than 38") ~ "Small(<38)",
      str_detect(Property.Size.Description, "38") & str_detect(Property.Size.Description, "125") ~ "Medium(38-125)",
      str_detect(Property.Size.Description, "greater than 125") ~ "Large(>=125)",
      str_detect(Property.Size.Description, "equal to 125") ~ "Large(>=125)",
      TRUE ~ NA_character_
    )
  )

# Extracting Price from County_Price and cleaning numeric format

# County_Price column contains County and Price in a single string e.g. "Dublin,343000"
# Using separate to split it into a temporary county check column and price column

pdf2 <- pdf2 %>%
  separate(
    County_Price,
    into = c("County", "Price"),
    sep = ",",
    fill = "right"
  )

# Using parse_number from readr to extract numeric price value from Price column
pdf2 <- pdf2 %>%
  mutate(Price = parse_number(Price))

# Adding columns that exist in pdf1 but are missing in PropertyDataset2

# PropertyDataset2 does not have geohash, post code, parking or bedroom/bathroom counts
# Creating these columns and filling with NA to keep the schema consistent for merging

pdf2 <- pdf2 %>%
  mutate(
    Geohash = NA_character_,
    Post_code = NA_character_,
    Parking = NA_character_,
    Bedrooms = NA_real_,
    Bathrooms = NA_real_,
    Property_type = NA_character_
  )

# Removing redundant columns
pdf2 <- select(pdf2, -Description.of.Property, -Property.Size.Description)

# Property_type column in PropertyDataset2 is unreliable as most entries default to 'apartment'
# Since Description.of.Property only provides age information and not structural type for pdf2,
# we will set Property_type to 'unknown' to avoid misleading classification

pdf2 <- pdf2 %>%
  mutate(Property_type = "unknown")

# Final formatting of column names and column order
# Reordering columns to match pdf1 (for cleaner bind_rows later)

pdf2 <- pdf2 %>%
  select(
    Geohash, Sale_date, Price, Post_code, County,
    Not_full_market_price, Vat_exclusive, Parking,
    Bedrooms, Bathrooms, Property_age,
    Sale_month, Sale_year, Sale_quarter,
    Property_size_cat, Property_type
  )

# Quick checks on the cleaned PropertyDataset2
str(pdf2)
summary(pdf2)
sapply(pdf2, class)

# Merging the two cleaned datasets - pdf1 and pdf2

# Using bind_rows since both datasets now share the same column structure
# This stacks the rows from PropertyDataset2 below those from PropertyDataset1

All_properties <- bind_rows(pdf1, pdf2)

# Final structure check of the merged dataset
str(All_properties)
summary(All_properties)
colSums(is.na(All_properties))

boxplot(All_properties$Sale_date)

# Property dataset 2 lacked Bedrooms, bathrooms and other details however since we have already imputed that on county level for property data 1.
# Repeating the same for final merged data as well

All_properties <- All_properties %>%
  group_by(County) %>%
  mutate(
    Bedrooms = ifelse(
      is.na(Bedrooms),
      as.numeric(names(which.max(table(Bedrooms)))),
      Bedrooms
    ),
    Bathrooms = ifelse(
      is.na(Bathrooms),
      as.numeric(names(which.max(table(Bathrooms)))),
      Bathrooms
    ),
    Property_size_cat = ifelse(
      is.na(Property_size_cat),
      names(which.max(table(Property_size_cat))),
      Property_size_cat
    ),
    Property_type = ifelse(
      is.na(Property_type),
      names(which.max(table(Property_type))),
      Property_type
    )
  ) %>%
  ungroup()

# Exploratory data analysis using visualisation

All_properties %>%
  filter(Price > 2000000) %>%
  count()

All_properties %>%
  filter(Price > 2000000) %>%
  count(County, sort = TRUE)

high_prices <- All_properties %>%
  filter(Price > 2000000)

View(high_prices)

# Since price is right skewed with high values, will plot price on a log scale to make county-level distributions visible
# most properties are between 100k-600k and a small number of very high-value sales in the millions
# This skewness is causing issues in visualization and is hiding lower values
# hence applied log10 to compress large values and spread the smaller values to make the distribution symmetric and reduce the impact of outliers
# Price across counties is right skewed distribution. Most properties fall in the typical range with only small proportion of luxury properties.

All_properties <- All_properties %>%
  mutate(Log_price = log10(Price))

# Price distribution across counties
# Right-skewed distribution but the log scale shows a central cluster meaning most properties are in the typical residential range
# with only a small proportion of luxury properties

ggplot(All_properties, aes(x = Log_price)) +
  geom_histogram(bins = 30, fill = "lightgrey", color = "black") +
  facet_wrap(~ County, scales = "free_y") +
  labs(
    title = "Price Distributions Across Counties (log scale)",
    x = "log10(Price)",
    y = "Count"
  )

# Lets take a look at the price trend over time
# We see that the price has always followed an upward trend and time/year is an influential factor
# The price peaked somewhere around 2019 and dipped again in 2020 and has been stable in the following years

year_price <- All_properties %>%
  group_by(Sale_year) %>%
  summarise(Mean_price = mean(Price, na.rm = TRUE))

# Overall distribution of the prices
ggplot(year_price, aes(x = Sale_year, y = Mean_price)) +
  geom_line(color = "green", size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Property Price Over Time",
    x = "Year",
    y = "Average Price"
  )

# Price distribution at level plot
# Location is one of the strongest predictors of property price
# Urban and coastal counties show a lot higher price distributions than rural regions
# Most of the spread is around 5-6 and dublin having outliers with high prices

ggplot(All_properties, aes(x = County, y = Log_price)) +
  geom_boxplot() +
  labs(
    title = "Property Price Distribution by County (Log Scale)",
    x = "County",
    y = "log10(Price)"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Price by property type
# Detached and semi-detached properties have higher log prices
# Apartments and terraced houses have lower medians
# Detached properties show the largest variation
# Property type also impacts the price. Larger, stand-alone properties have higher prices while apartments are more affordable

ggplot(All_properties, aes(x = Property_type, y = Log_price)) +
  geom_boxplot() +
  labs(
    title = "Price Distribution by Property Type (Log Scale)",
    x = "Property Type",
    y = "log10(Price)"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Bar chart for median price for each county
# Counties such as Dublin, Wicklow, Cork and Galway have much higher median prices as compared to rural counties like
# Longford, Leitrim, and Roscommon.
# This clearly shows location is a strong predictor of price

county_median <- All_properties %>%
  group_by(County) %>%
  summarise(Median_log_price = median(Log_price, na.rm = TRUE))

ggplot(county_median, aes(x = Median_log_price, y = reorder(County, Median_log_price))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Median Property Price by County (Log Scale)",
    x = "Median log10(Price)",
    y = "County"
  )

# Number of properties sold per county
# Dublin has the highest number of transactions which shows a more active property market, while smaller counties have fewer listings

ggplot(All_properties, aes(y = County)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = "Number of Properties Sold by County",
    x = "Count",
    y = "County"
  )

# Scatter plot for relationship between bathroom & bedroom with price

# properties with more bedrooms normally have higher prices
# The vertical spread within each bedroom category shows that bedrooms alone do not fully explain price
# Location, property type, and bathrooms also play significant roles

ggplot(All_properties, aes(x = Bedrooms, y = Log_price)) +
  geom_point(alpha = 0.3)

# More bathrooms normally mean higher log price
# Bathrooms also help in explaining price variation especially when used together with bedrooms and property type

ggplot(All_properties, aes(x = Bathrooms, y = Log_price)) +
  geom_point(alpha = 0.3)

#############################
# Creating a subset from the All_property data set which will be model ready.
# We will only include our predictors which are price driving factors and get rid of NAs for this because models cannot train on NA rows
# We now have a model ready data called property_model with the predictors and no NA value

property_model <- All_properties %>%
  select(
    Price, County, Bedrooms, Bathrooms,
    Property_type, Property_size_cat, Property_age,
    Parking, Not_full_market_price, Vat_exclusive,
    Sale_year, Sale_quarter
  ) %>%
  filter(!is.na(Parking))

write.xlsx(All_properties, "C:/Users/njfbts/Downloads/All_properties.xlsx")
write.xlsx(pdf1, "C:/Users/njfbts/Downloads/pdf1.xlsx")

# Q1 Using pdf1(cleaned PropertyDataset1.csv)

# a) Filter all of the houses from the data set that were sold in 2017, that have 3 bedrooms and 2 bathrooms and save it as a data frame

Q1a <- pdf1 %>%
  filter(
    Sale_year == 2017,
    Bedrooms == 3,
    Bathrooms == 2,
    Property_type != "apartment"
  )

Q1a

# b) Filter all of the data from the dataset for premises that were sold in Cork or Galway
# and are greater than 125 square metres. Order the data so that the price is listed
# from highest to lowest and save it as a data frame. (1.5 marks)

Q1b <- pdf1 %>%
  filter(
    County %in% c("cork", "galway"),
    Property_size_cat == "Large(>=125)"
  ) %>%
  arrange(desc(Price))

Q1b

# c) Create a data frame that contains the average price, maximum price and minimum
# price of all of the new dwelling houses or apartments sold in Dublin 18, Dublin 16,
# Dublin 15, Dublin 8 or Dublin 4. (1.5 marks)

Q1c <- pdf1 %>%
  filter(
    County == "dublin",
    Post_code %in% c("dublin 18", "dublin 16", "dublin 15", "dublin 8", "dublin 4"),
    Property_age == "new"
  ) %>%
  group_by(Post_code) %>%
  summarise(
    Avg_price = mean(Price),
    Max_price = max(Price),
    Min_price = min(Price)
  )

Q1c

# d) Using the "ggplot2" package, create a bar chart with all of the property sold in the
# different counties in Ireland. Colour each of the bars based on the type of properties
# sold in each county. (1.5 marks)

ggplot(pdf1, aes(County, fill = Property_type)) +
  geom_bar() +
  labs(
    title = "Properties Sold by County and Property Type",
    x = "County",
    y = "Count",
    fill = "Property Type"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# e) Create a plot using the "ggplot2" package which represents the number of different
# types of houses sold over the time period from 2015 to 2017, inclusive. (1.5 marks)

type_data <- pdf1 %>%
  filter(Sale_year >= 2015 & Sale_year <= 2017) %>%
  count(Sale_year, Property_type)

ggplot(type_data, aes(x = Sale_year, y = n, fill = Property_type)) +
  geom_col() +
  labs(
    title = "Property Types Sold (2015-2017)",
    x = "Year",
    y = "Count"
  )

# f) Remove all of the columns from "num_bedrooms" to "not_full_market_price" in the
# dataset and keep all of the rows that are from either Dublin, Cork or Galway and are
# a Semi-detached house and arrange the prices from lowest to highest. (1.5 marks)

Q1f <- pdf1 %>%
  select(-Bedrooms, -Bathrooms, -Not_full_market_price, -Vat_exclusive, -Parking) %>%
  filter(
    County %in% c("dublin", "cork", "galway"),
    Property_type == "semi-detached"
  ) %>%
  arrange(Price)

Q1f

# g) Filter all of the houses greater than 200,000 from Galway, Roscommon, Mayo or
# Sligo. Use ggplot to create a line graph. The line graph should consist of 4 lines
# representing the price of each of the 4 counties over the years in the data set. The
# lines must all be different colours on the plot.

Q1g <- pdf1 %>%
  filter(County %in% c("galway", "roscommon", "mayo", "sligo"), Price > 200000)

Q1g_plot <- Q1g %>%
  group_by(Sale_year, County) %>%
  summarise(Mean_price = mean(Price), .groups = "drop")

ggplot(Q1g_plot, aes(x = Sale_year, y = Mean_price, colour = County)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Price (>200k) in Western Counties by Year",
    x = "Year",
    y = "Average Price",
    colour = "County"
  )
