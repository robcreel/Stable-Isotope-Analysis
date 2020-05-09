dataset <- read.csv('Churn_Modelling.csv')
dataset <- dataset[4:14]

dataset$Geography <- as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

library(h2o)

h2o.init(nthreads = -1) # always use -1 if you are running on a basic computer


classifier = h2o.deeplearning(y = 'Exited', # y is the name of the Dependent variable
                              # we need to make an H2o dataframe because we have a plain dataframe
                              training_frame = as.h2o(training_set), 
                              activation = 'Rectifier', # this is activation function
                              hidden = c(6,6), # as a vector the number of hidden layers and vectors
                              # chosing the nodes rule of thumb is to use variables 10 + 1 / 2 = 5.5 ;) we'll
                              # use 6 (note the 2 is for the 2 groups)
                              epochs = 100, # how many times the dataset will be iterated
                              train_samples_per_iteration = -2) # -2 is autotune :-)

prob_pred = h2o.predict(classifier, newdata = as.h2o(test_set[-11])) # -11 is the index of dependent variable. Again H2O needs a h2o dataframe

# the 0.5 is a threshold
# y_pred = ifelse(prod_pred > 0.5, 1, 0) # this can be written simpler as
y_pred = (prob_pred > 0.5)

y_pred = as.vector(y_pred)
cm = table(test_set[, 11], y_pred) # 11 is the index of the dependent variable

