model_data <- function(data, future_value = 999){

col_num<-as.numeric(ncol(data))
#include all future data with previous data
data <- as.matrix(data)
dimnames(data) <- NULL
data[,2:col_num] <- normalize(data[,2:col_num])
# Future (upcoming) rows are flagged by the results sentinel in column 1 (the
# target), which normalize() leaves untouched. Detect them explicitly rather than
# assuming the sentinel is the matrix-wide maximum and the rows are contiguous.
future_rows <- which(data[,1] == future_value)
if (length(future_rows) == 0)
  stop("model_data: no future rows (column 1 == ", future_value, ") found")
# separate future data
future_matrix <- data[future_rows, 2:col_num]
full_future_matrix <- data[future_rows, 1:col_num]
#remove future data from training set
data <- data[-future_rows, ]
full_data_matrix <- data[,2:col_num]
full_data_target <- to_categorical(data[,1])
# organize training set
set.seed(321)
ind<-sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
training <- data[ind==1, 2:col_num]
test <- data[ind==2 , 2:col_num]
trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
test_var<-as.numeric(nrow(testLabels))
test_dim<-as.numeric(ncol(testLabels))

dataframe_list <- list(training = training, trainLabels = trainLabels,test = test,testLabels = testLabels, test_var = test_var, testtarget = testtarget, test_dim = test_dim, future_matrix = future_matrix, full_future_matrix = full_future_matrix,full_data_matrix = full_data_matrix,full_data_target=full_data_target, data = data)

}

