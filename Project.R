# Loan Default and Loss Severity Project
# 4Q2017

library(tools) # Packaged used for computing MD5 checksum

my.path <- 'C:/Users/sdj98/Documents/Predict 454/Project/'
infile.train <- paste(my.path, 'train_v2.csv', sep='')
infile.test <- paste(my.path, 'test_v2.csv', sep='')

###############################################################################
##
## Import Training Data
##
###############################################################################
# Calculate MD5 checksum to verify file integrity
as.vector(md5sum(infile.train))
paste('checked at', date())
# "checked at Sun Sep 24 11:19:28 2017"
# "97fc0d96049621933a30f6afa24b8882"

train <- read.csv(infile.train,header=TRUE)

###############################################################################
##
## Import Testing Data
##
###############################################################################
# Calculate MD5 checksum to verify file integrity
as.vector(md5sum(infile.test))
paste('checked at', date())
# "checked at Sun Sep 24 14:51:00 2017"
# "91e1fb86e4df131d1a1f2eed9621fde6

test <- read.csv(infile.test,header=TRUE)

###############################################################################
##
## Data Preparation
##
###############################################################################

test$loss <- NA # Test data does not have loss target variable
combined <- rbind(train,test)

# Verify dimension of datasets
dim(train)
dim(test)
dim(combined) # training set rows 1:105471; testing set rows 105472:316415

# According to Q&A page, variables f776, f777, and f778 are categorical
#   https://www.kaggle.com/c/loan-default-prediction/discussion/6862

combined$f776 <- as.factor(combined$f776)
combined$f777 <- as.factor(combined$f777)
combined$f778 <- as.factor(combined$f778)

summary(combined$f776)
summary(combined$f777)
summary(combined$f778) # may be too many levels to deal with for some modeling methods

# According to Q&A page, variables f2, f4, and f5 can be seen as categorical
#   these variables refer to the size cluster of loans, maturity ranges, and qualitative ranking on ease of transaction
#   https://www.kaggle.com/c/loan-default-prediction/discussion/6862

combined$f2 <- as.factor(combined$f2)
combined$f4 <- as.factor(combined$f4)
combined$f5 <- as.factor(combined$f5)

summary(combined$f2)
summary(combined$f4) # may be too many levels to deal with for some modeling methods
summary(combined$f5)

# Check for Missing Values
names(which(sapply(combined,anyNA))) # Variables with NAs
colSums(is.na(combined)) # Number of NAs by variable
colMeans(is.na(combined)) # Percentage of NAs by variable

# Create boolean indicator variables for missing values starting at column 772
flag.combined <- combined
flag.combined[ , paste0( "M_",names(flag.combined)[-1])] <- lapply(flag.combined[-1], function(x) as.numeric(is.na(x)))
flag.combined[,772:1541] <- lapply(flag.combined[,772:1541],as.factor)
names(flag.combined)

# Remove boolean indicator variables for missing values if column has no missing values
drops <- names(which(sapply(flag.combined[,772:1541],function(x) sum(x != 0) == 0)))
flag.combined <- flag.combined[ , !(names(flag.combined) %in% drops)]
str(flag.combined, list.len=ncol(flag.combined))

# Remove the loss column to prevent imputing target values and remove the missing loss flag variable
loss <- flag.combined$loss
drops <- c('loss', 'M_loss')
flag.combined <- flag.combined[ , !(names(flag.combined) %in% drops)]

imputed.data.final <- flag.combined

imputed.data.final$f5[is.na(imputed.data.final$f5)] <- tail(names(sort(table(imputed.data.final$f5))), 1)

imputed.data.final$f2 <- as.numeric(imputed.data.final$f2)
imputed.data.final$f4 <- as.numeric(imputed.data.final$f4)
imputed.data.final$f5 <- as.numeric(imputed.data.final$f5)
imputed.data.final$f776 <- as.numeric(imputed.data.final$f776)
imputed.data.final$f777 <- as.numeric(imputed.data.final$f777)
imputed.data.final$f778 <- as.numeric(imputed.data.final$f778)

imputed.data.final[,1:770] <- vapply(imputed.data.final[,1:770], 
                             function(x) replace(x, is.na(x), median(x,na.rm=TRUE)), FUN.VALUE=numeric(nrow(imputed.data.final)))

# Check for Missing Values
names(which(sapply(imputed.data.final,anyNA))) # Variables with NAs
# only variable with NA value is loss for test data

imputed.data.final$loss <- loss
imputed.train <- imputed.data.final[1:105471,]
imputed.test <- imputed.data.final[105472:316415,]

names(which(sapply(imputed.train,anyNA))) # Variables with NAs
names(which(sapply(imputed.test,anyNA))) # Variables with NAs

# Write CSV in R
setwd(my.path )
write.csv(imputed.train, file = "imputed_train.csv",row.names=FALSE)
write.csv(imputed.test, file = "imputed_test.csv",row.names=FALSE)
