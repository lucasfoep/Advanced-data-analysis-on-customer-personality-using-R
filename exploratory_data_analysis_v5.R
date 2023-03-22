# Importing libraries

library(corrplot)
library(psych)

# Importing and exploring dataframe

marketing = read.table('Filled_data.csv', header=TRUE, sep=',')
marketing <- marketing[, c(-1, -27)]
head(marketing)
summary(marketing)
sapply(marketing, class)


# Creating a function that produces a subset with only numeric variables
numeric_columns <- function(df)
{
  col_names = colnames(df) # Creating list of column names
  numeric_col_names = c() # Creating an empty vector
  
  for (i in 1:ncol(df)) # Iterates through all columns of the dataframe
  {
    if((class(df[,i]) == 'numeric') || (class(df[,i]) == 'integer')) # Verifies if it is a numerical variable
    {
      numeric_col_names = append(numeric_col_names, col_names[i]) # Adds column name to vector of numeric column names.
    }
  }
  return(numeric_col_names) # Returns vector of name of numeric columns
}

numeric_col_names = numeric_columns(marketing) # Stores name of numeric columns
numeric_marketing = marketing[, numeric_col_names] # Creates dataframe with only numeric columns


# There are some variables with a somewhat strong correlation, but they might be
# improved upon variable transformation.

correlation_matrix = cor(numeric_marketing)
corrplot(correlation_matrix, method='ellipse', order='AOE')

# Removing three outliers that are making variable Year_Birth highly skewed.
# These are only three observations on a large dataset. The relationship between
# all variables have been analyzed before and after removing these and as suspected
# it did not change anything for any other variables. I decided to perform this
# remotion at the beginning instead of analyzing all variable's so as to keep the
# code clean.

hist(marketing$Year_Birth)
marketing = marketing[marketing$Year_Birth >= 1940,]
hist(marketing$Year_Birth)

# Creating a function to plot histograms for a group of variables.

plot_hist <- function(df)
{
  col_names = colnames(df) # Creating list of column names
  
  for (i in 1:ncol(df)) # Iterates through all columns of the dataframe
  {
    hist(df[,i], main=col_names[i]) # Plots a histogram for the variable
  }
}

# As suspected, many of the variables are right skewed and should be transformed.
# A log + 1 transformation will be performed for those variables, considering that
# the immense majority of them have null values.

plot_hist(numeric_marketing)

# Applying log + 1 transformations to variables.

marketing$logp1_Income <- log(marketing$Income + 1)
marketing$logp1_MntFishProducts <- log(marketing$MntFishProducts + 1)
marketing$logp1_MntFruits <- log(marketing$MntFruits + 1)
marketing$logp1_MntGoldProds <- log(marketing$MntGoldProds + 1)
marketing$logp1_MntMeatProducts <- log(marketing$MntMeatProducts + 1)
marketing$logp1_MntSweetProducts <- log(marketing$MntSweetProducts + 1)
marketing$logp1_MntWines <- log(marketing$MntWines + 1) # Something a little subtler than log would be better
marketing$logp1_NumCatalogPurchases <- log(marketing$NumCatalogPurchases + 1) # Something a little stronger than log would be better
marketing$logp1_NumDealsPurchases <- log(marketing$NumDealsPurchases + 1)
marketing$logp1_NumStorePurchases <- log(marketing$NumStorePurchases + 1)
marketing$logp1_NumWebPurchases <- log(marketing$NumWebPurchases + 1)
marketing$logp1_NumWebVisitsMonth <- log(marketing$NumWebVisitsMonth + 1)

# Deleting pre log variables

marketing <- marketing[, c(-4, -(8:18))]

# Correlation for log transformed variables improved significantly as can be seen
# on the correlation matrix.

numeric_col_names = numeric_columns(marketing) # Stores name of numeric columns
numeric_marketing = marketing[, numeric_col_names] # Creates dataframe with only numeric columns
correlation_matrix = cor(numeric_marketing)
corrplot(correlation_matrix, method='ellipse', order='AOE')

# Transformed variables have a much less skewed and apparently normal distribution.

plot_hist(numeric_marketing)

# Adds up all campaigns accepted by row.

marketing$Tot_Accepted = marketing$AcceptedCmp1 + marketing$AcceptedCmp2 + marketing$AcceptedCmp3 +
                         marketing$AcceptedCmp4 + marketing$AcceptedCmp5 + marketing$Response

# Checks the distribution for the new variable. Tried log transforming, but could
# not unskew the distribution.
hist(marketing$Tot_Accepted)

# Adds zero to new variable if Marital Status equals single, one if it equals
# together and two if it equals married.

for (i in 1:nrow(marketing))
{
  if (marketing$Marital_Status[i] == 'Single' || marketing$Marital_Status[i] == 'Alone')
  {
    marketing$Marital_Status[i] = 0
  } else if (marketing$Marital_Status[i] == 'Together')
  {
    marketing$Marital_Status[i] = 1
  } else if (marketing$Marital_Status[i] == 'Married')
  {
    marketing$Marital_Status[i] = 2
  } else if (marketing$Marital_Status[i] == 'Divorced')
  {
    marketing$Marital_Status[i] = 3
  } else if (marketing$Marital_Status[i] == 'Widow')
  {
    marketing$Marital_Status[i] = 4
  }
}

# Making Marital Status variable numeric and removing NAs created due to the presence
# of values that did not belong to that column ("Absurd" and "YOLO")
marketing$Marital_Status = as.numeric(marketing$Marital_Status)
marketing = na.omit(marketing)

# Checks the distribution for the new variable.
hist(marketing$Marital_Status)

# After some research I found out that this dataset is of an unknown international
# precedence, in which case graduate refers to the equivalent of an undergraduate
# level in the US; Second cycle is the equivalent to graduate level and Basic
# refers to high school level. Therefore, Basic will receive a 0, graduate will
# receive a one and all the other levels of education will receive a 2, meaning
# graduate level (Masters, PhD and 2n Cycle).

for (i in 1:nrow(marketing))
{
  if (marketing$Education[i] == 'Basic')
  {
    marketing$Education[i] = 0
  } else if (marketing$Education[i] == 'Graduation')
  {
    marketing$Education[i] = 1
  } else
  {
    marketing$Education[i] = 2
  }
}

# Making Education variable numeric.
marketing$Education = as.numeric(marketing$Education)

# Checks the distribution for the new variable.
hist(marketing$Education)


write.csv(marketing, 'final_project.csv')

