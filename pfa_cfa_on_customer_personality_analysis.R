# Loading libraries
library(psych)
library(corrplot)

# Loading dataframe and taking a look at it
marketing = read.csv('final_project.csv')[, -1]
head(marketing)

# Dropping education and marital status, as I can't make them numeric variables,
# and replacing all 'accepted' variables with a 'Total Accepted' variables.

marketing = marketing[, -c(2:3, 7:11, 13)]

pc1 = prcomp(marketing, scale=T)
summary(pc1)

matrices_list = list()

# Then found the number of rows and columns of the dataframe
num_rows = nrow(marketing)
num_cols = ncol(marketing)

# Used a for loop to create thirty random matrices and store them in the list
for (i in 1:30)
{
  m = matrix(rnorm(num_cols*num_rows, 0, 1), ncol = num_cols)
  matrices_list[[i]] = m
}

# Calculated the mean of each corresponding element of each matrix.
mm = Reduce("+",matrices_list)/length(matrices_list)

# Casted it to a dataframe
mmdf = data.frame(mm)

# Plotted the scree plots and ab lines
par(mfrow=c(1, 2))

plot(pc1, ylim=c(0, 7.5), main='Principal Factors')
abline(1, 0, col='red')

prand = prcomp(mmdf, scale=T)
plot(prand, ylim=c(0, 7.5), main='Random Data')
abline(1, 0, col='red')

# Running parallel analysis
par(mfrow=c(1, 1))
parallel_pfa = fa.parallel(marketing, n.iter=30)

# Based on knee, abline, parallel analysis and cumulative proportion it seems
# like four principal components should be used. Trying it with principal function

pfa1 = principal(marketing, nfactors=4, rot='none')
print(pfa1$loadings, cutoff=.4, sort=T)

# Trying varimax

pfa2 = principal(marketing, nfactors=4, rot='varimax')
print(pfa2$loadings, cutoff=.4, sort=T)

# Comparing results with CFA

cfa = factanal(marketing, 4)
print(cfa$loadings, cutoff=.4, sort=T)

# Checking it against correlation matrix

par(mfrow=c(1,1))
correlation_matrix = cor(marketing)
corrplot(correlation_matrix, method='ellipse', order='AOE')

