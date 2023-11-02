#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^racism-county-homeownership-inequity^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Reading csv file of Racism - county - income inequity
data1 <- read.csv("/Users/vijaykumarreddybommireddy/Downloads/racism-county-homeownership-inequity/racism-county-homeownership-inequity.csv")

# View the first few rows of the dataset
head(data)





#@@@@@@@@@@@@@@@@@@@@@@@@@Exploratory data analysis and visualizatiion@@@@@@@@@@@@@@@@@@@@@.

# Get summary statistics
summary(data)

# Check the structure of the dataset
str(data)

# Calculate the number of null values in each column
null_counts <- sapply(data1, function(x) sum(is.na(x)))
print(null_counts)

#Replace null values with mean of respective column
install.packages("mice")

library(mice)
imp <- mice(data1, method = "mean")
cdata1 <- complete(imp)


# Correlation matrix
# Assuming we only want to include numeric columns in the correlation analysis so we changin
numeric_columns <- cdata1[, sapply(cdata1, is.numeric)]
cor_matrix <- cor(numeric_columns)
cor_matrix

install.packages("corrplot")
# Load required libraries
library(ggplot2)
library(corrplot)

# Create a correlation plot
my_color_palette <- colorRampPalette(c("blue", "white", "red"))(50)
corrplot(cor_matrix,method = "color", col = my_color_palette)


#1

# Boxplot of homeownership inequity ratios
library(ggplot2)
ggplot(cdata1, aes(x = statefips, y = ho_ratio_wanh_ba)) +
  geom_boxplot() +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Black alone",
       x = "State FIPS Code", y = "Inequity Ratio")

#This code filters the cdata1 dataset to only include rows where ho_ratio_wanh_ba is less than or equal to 40
cdata1 <- cdata1[cdata1$ho_ratio_wanh_ba <= 40, ]


# bargraph of homeownership inequity ratios
ggplot(cdata1, aes(x = factor(statefips), y = ho_ratio_wanh_ba)) +
  geom_bar(stat = "identity") +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Black alone",
       x = "State FIPS Code", y = "Inequity Ratio")




#2.

# Boxplot of homeownership inequity ratios
library(ggplot2)
ggplot(cdata1, aes(x = statefips, y = ho_ratio_wanh_aa)) +
  geom_boxplot() +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Asian alone",
       x = "State FIPS Code", y = "Inequity Ratio")
#This code filters the cdata1 dataset to only include rows where ho_ratio_wanh_aa is less than or equal to 30
cdata1 <- cdata1[cdata1$ho_ratio_wanh_ba <= 30, ]

# bargraph of homeownership inequity ratios
ggplot(cdata1, aes(x = factor(statefips), y = ho_ratio_wanh_aa)) +
  geom_bar(stat = "identity") +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Asian alone",
       x = "State FIPS Code", y = "Inequity Ratio")



#3

# Boxplot of homeownership inequity ratios
library(ggplot2)
ggplot(cdata1, aes(x = statefips, y = ho_ratio_wanh_h)) +
  geom_boxplot() +
  labs(title = "Homeownership Inequity Ratio: White alone vs.Hispanic or latino",
       x = "State FIPS Code", y = "Inequity Ratio")
#This code filters the cdata1 dataset to only include rows where ho_ratio_wanh_h is less than or equal to 20
cdata1 <- cdata1[cdata1$ho_ratio_wanh_ba <= 20, ]

# bargraph of homeownership inequity ratios
ggplot(cdata1, aes(x = factor(statefips), y = ho_ratio_wanh_h)) +
  geom_bar(stat = "identity") +
  labs(title = "Homeownership Inequity Ratio: White alone vs.Hispanic or latino",
       x = "State FIPS Code", y = "Inequity Ratio")




# Scatterplot of Homeownership inequity ratios between White alone/ Asian alone and White alone/ Black alone
ggplot(cdata1, aes(x = ho_ratio_wanh_ba, y = ho_ratio_wanh_aa)) +
geom_point() +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Asian alone",
       x = "Inequity Ratio (White vs. Black)", y = "Inequity Ratio (White vs. Asian)")

# Scatterplot of Homeownership inequity ratios between White alone/ Black  and White alone/ Hispanic or latino
ggplot(cdata1, aes(x = ho_ratio_wanh_ba, y = ho_ratio_wanh_h)) +
  geom_point() +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Asian alone",
       x = "Inequity Ratio (White vs. Black)", y = "Inequity Ratio (White vs.Hispanic or latino)")

# Scatterplot of Homeownership inequity ratios between White alone/ Asian alon and White alone/ Hispanic or latino
ggplot(cdata1, aes(x = ho_ratio_wanh_aa, y = ho_ratio_wanh_h)) +
  geom_point() +
  labs(title = "Homeownership Inequity Ratio: White alone vs. Asian alone",
       x = "Inequity Ratio (White vs. Asian)", y = "Inequity Ratio (White vs.Hispanic or latino)")






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@Statistical analysis@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

summary(cdata1$ho_ratio_wanh_ba)
summary(cdata1$ho_ratio_wanh_aa)
summary(cdata1$ho_ratio_wanh_h)

#Comparing Means using Two Sample t-test
t.test(cdata1$ho_ratio_wanh_ba, cdata1$ho_ratio_wanh_aa)
t.test(cdata1$ho_ratio_wanh_ba, cdata1$ho_ratio_wanh_h)
t.test(cdata1$ho_ratio_wanh_aa, cdata1$ho_ratio_wanh_h)







#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^Racism-county-residential-segregation-index-of-dissimilarity^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Reading csv file of Racism - county - income inequity
data2 <- read.csv("/Users/vijaykumarreddybommireddy/Downloads/racism-county-residential-segregation-index-of-dissimilarity/racism-county-residential-segregation-index-of-dissimilarity.csv")

# View the first few rows of the dataset
head(data2)


#@@@@@@@@@@@@@@@@@@@@@@@@@Exploratory data analysis and visualizatiion@@@@@@@@@@@@@@@@@@@@@.
# Get summary statistics
summary(data2)

# Check the structure of the dataset
str(data2)

# Calculate the number of null values in each column
null_counts <- sapply(data2, function(x) sum(is.na(x)))
print(null_counts)

#Replace null values with mean of respective column
imp <- mice(data2, method = "mean")
cdata2 <- complete(imp)

# Correlation matrix
# Assuming we only want to include numeric columns in the correlation analysis so we changin
numeric_columns <- cdata2[, sapply(cdata2, is.numeric)]
cor_matrix <- cor(numeric_columns)
cor_matrix


# Create a correlation plot
my_color_palette <- colorRampPalette(c("blue", "white", "red"))(50)
corrplot(cor_matrix,method = "color", col = my_color_palette)



# Boxplot of dissimilarity inequity ratios
ggplot(cdata2, aes(x = statefips, y = d_wa_ba)) +
  geom_boxplot() +
  labs(title = "Dissimilarity data",
       x = "State FIPS Code", y = "Index of dissimilaritybetween White alone andBlack alone")

# Boxplot of dissimilarity inequity ratios
ggplot(cdata2, aes(x =as.factor(statefips), y = d_wa_aa)) +
  geom_boxplot() +
  labs(title = "Dissimilarity data",
       x = "State FIPS Code", y = "Index of dissimilaritybetween White alone andAsian alone")
























