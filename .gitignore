# Reza Asad
# Digit Recognition

# Extract Data
x <- read.table("digit_recognition.dat", header = FALSE,
  col.names = paste("f", 1:76, sep = ""))

# Response Variable
digits = rep(0:9, each = nrow(x)/10, length.out = nrow(x));
#-----------------------------KNN-DIGIT-RECOGNITION------------------------
# The goal is to predict the digits using k nearest neighbours method.
# I find the best k using cross validation. 

library(class)
set.seed(4)

# y Should be a Factor for Classification
y <- factor(digits)

# Using 5 Fold Cross Validation to Choose k
n <- nrow(dat);
k <- 5
ii <- rep(1:k, each=floor(n/k))
if( length(ii) < n ){
	ii <- c(ii, 1:(n - length(ii)))
}
ii <- sample(ii);

max.k <- 10;
misclass <- vector('numeric', length = max.k);
for (j in 1:max.k){
	cv.err <- 0;
	for(i in 1:k){
		digits.knn <- knn(x[ii != k,], x[ii == k,], y[ii!=k], k = j,
	 		l = 0, prob = FALSE, use.all = TRUE)
		cv.err <- cv.err + sum(digits.knn!=y[ii == k]);
	}
	misclass[j] <- cv.err;
}
misclass <- misclass/n;

# Best k Value
kValue = which.min(misclass)

#Misclassification Error Using Best k Value
digits.knn <- knn(x,x,cl = y, k = kValue, l = 0, prob = FALSE, 
	use.all = TRUE);
print(misclass[kValue])

# Misclassification Matrix
table(y,digits.knn) 


