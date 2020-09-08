# import the CSV file
pcd <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)

# examine the structure of the pcd data frame
str(pcd)

# drop the id feature
pcd <- pcd[-1]

#drop the null values
pcd<- pcd[complete.cases(pcd[ , 3:9]),]

# table of diagnosis
table(pcd$diagnosis_result)

# recode diagnosis as a factor
pcd$diagnosis_result <- factor(pcd$diagnosis_result, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
pcd$diagnosis_result
# table or proportions with more informative labels
round(prop.table(table(pcd$diagnosis_result)) * 100, digits = 1)

# summarize three numeric features
summary(pcd[c("radius", "area", "smoothness")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the pcd data
pcd_n <- as.data.frame(lapply(pcd[2:9], normalize))

# confirm that normalization worked
summary(pcd_n$area)

# create training and test data
pcd_train <- pcd_n[1:80, ]
pcd_test <- pcd_n[81:95, ]

# create labels for training and test data
pcd_train_labels <- pcd[1:80, 1]
pcd_test_labels <- pcd[81:95, 1]

## Step 3: Training a model on the data ----

# load the "class" library
#install.packages("class")
library(class)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test,
                      cl = pcd_train_labels, k=10)
pcd_test_pred
## Step 4: Evaluating model performance ----

# load the "gmodels" library
#install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = pcd_test_labels, y = pcd_test_pred,
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
pcd_z <- as.data.frame(scale(pcd[-1]))

# confirm that the transformation was applied correctly
summary(pcd_z$area)

# create training and test datasets
pcd_train <- pcd_z[1:80, ]
pcd_test <- pcd_z[81:95, ]

# re-classify test cases
pcd_test_pred <- knn(train = pcd_train, test = pcd_test,
                      cl = pcd_train_labels, k=10)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = pcd_test_labels, y = pcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
pcd_train <- pcd_n[1:80, ]
pcd_test <- pcd_n[81:95, ]

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=1)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=5)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=11)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=15)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=21)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=27)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=10)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=4)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)

pcd_test_pred <- knn(train = pcd_train, test = pcd_test, cl = pcd_train_labels, k=25)
CrossTable(x = pcd_test_labels, y = pcd_test_pred, prop.chisq=FALSE)