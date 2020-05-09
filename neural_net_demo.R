library(ISLR)
library(neuralnet)
library(caTools)

# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

# Use scale() and convert the resulting matrix to a data frame

# Convert Private column from Yes/No to 1/0
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))
Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)

# Convert to formula
f <- as.formula(f)

# Neural Net
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

table(test$Private,predicted.nn.values$net.result)

