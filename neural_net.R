# library(ISLR)
# library(neuralnet) (nnet)
library(tidyverse)
library(lubridate)
# library(caret)
library(caTools)
library(h2o)

data_file = "mega.csv"
data_file %>% read_csv() %>% 
  mutate(TIMESTAMP = ymd_hm(TIMESTAMP)) %>% 
  drop_na() %>% 
  # mutate(Location = Name) %>% 
  select(Delta_18O, Delta_D, Location = Name) -> dataset #TIMESTAMP,  

# dataset <- read.csv('Churn_Modelling.csv')
# dataset <- dataset[4:14]

# dataset$Geography <- as.numeric(factor(dataset$Geography,
#                                        levels = c('France', 'Spain', 'Germany'),
#                                        labels = c(1, 2, 3)))
# dataset$Gender = as.numeric(factor(dataset$Gender,
#                                    levels = c('Female', 'Male'),
#                                    labels = c(1, 2)))
loc_levels = dataset$Location %>% unique()
dataset$Location = as.numeric(factor(dataset$Location, 
                                     levels = loc_levels,
                                     labels = 1:length(loc_levels)))

set.seed(123)
split = sample.split(dataset$Location, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])


h2o.init(nthreads = -1) # always use -1 if you are running on a basic computer


classifier = h2o.deeplearning(y = 'Location', # y is the name of the Dependent variable
                              # we need to make an H2o dataframe because we have a plain dataframe
                              training_frame = as.h2o(training_set), 
                              activation = 'Tanh', # this is activation function
                              hidden = c(6,6), # as a vector the number of hidden layers and vectors
                              # chosing the nodes rule of thumb is to use variables 10 + 1 / 2 = 5.5 ;) we'll
                              # use 6 (note the 2 is for the 2 groups)
                              epochs = 100, # how many times the dataset will be iterated
                              train_samples_per_iteration = -2) # -2 is autotune :-)

prob_pred = h2o.predict(classifier, newdata = as.h2o(training_set[-3])) # -3 is the index of dependent variable. Again H2O needs a h2o dataframe

# the 0.5 is a threshold
# y_pred = ifelse(prod_pred > 0.5, 1, 0) # this can be written simpler as
y_pred = (prob_pred > 0.5)

y_pred = as.vector(y_pred)
cm = table(training_set$Location , y_pred) # 3 is the index of the dependent variable

model <- h2o.gbm(x = 1:2, y = 3, training_frame = as.h2o(dataset))
h2o.confusionMatrix(model, as.h2o(dataset))


h2o.confusionMatrix(classifier, as.h2o(dataset))

# Possible vis
h2o.partialPlot(object = classifier, data = as.h2o(dataset))
  

h2o.shutdown()
