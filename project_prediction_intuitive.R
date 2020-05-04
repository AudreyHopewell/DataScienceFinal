library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
set.seed(92397)

# analysis 1: using "intuitive category" data
# breaking up the "intuitive category" data into testing and training sets
n <- nrow(movies.cat)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
cat.train <- movies.cat[train,]
cat.test <- movies.cat[test,]

# creating the task, resampling, and tuning strategies
classification <- makeClassifTask(id="category task", data = cat.train, target = "category")
resampling <- makeResampleDesc(method = "CV", iters = 6)
tuning <- makeTuneControlRandom(maxit = 10)

# setting up the prediction algorithms
trees <- makeLearner("classif.rpart", predict.type = "response")
logregression <- makeLearner("classif.glmnet",predict.type = "response")
neuralnet <- makeLearner("classif.nnet",predict.type = "response")
naivebayes <- makeLearner("classif.naiveBayes",predict.type = "response")
kNN <- makeLearner("classif.kknn",predict.type = "response")
SVM <- makeLearner("classif.svm",predict.type = "response")

# creating hyperparameters 
tree_param <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = .001, upper =.2)
)

logit_param <- makeParamSet(
  makeNumericParam("lambda", lower = 0, upper = 3),
  makeNumericParam("alpha", lower = 0, upper = 1)
)

neural_param <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = .1, upper = .5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000)
)

kNN_param <- makeParamSet(
  makeIntegerParam("k", lower = 1,upper = 30)
)

SVM_param <- makeParamSet(
  makeDiscreteParam("kernel", values = "radial"),
  makeDiscreteParam("cost", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10)),
  makeDiscreteParam("gamma", values = c(2^-2, 2^-1, 2^0, 2^1, 2^2, 2^10))
)

# tuning the models

tuned_trees <- tuneParams(learner = trees,
                          task = classification,
                          resampling = resampling,
                          par.set = tree_param,
                          control = tuning,
                          show.info = TRUE
)
#[Tune] Result: minsplit=23; minbucket=6; cp=0.0386 : mmce.test.mean=0.5292388

tuned_logit <- tuneParams(learner = logregression,
                          task = classification,
                          resampling = resampling,
                          par.set = logit_param,
                          control = tuning,
                          show.info = TRUE
)
#[Tune] Result: lambda=0.181; alpha=0.384 : mmce.test.mean=0.5743843

tuned_neural <- tuneParams(learner = neuralnet,
                           task = classification,
                           resampling = resampling,
                           par.set = neural_param,
                           control = tuning,
                           show.info = TRUE
)
#[Tune] Result: size=10; decay=0.292; maxit=1000 : mmce.test.mean=0.4810625

tuned_kNN <- tuneParams(learner = kNN,
                        task = classification,
                        resampling = resampling,
                        par.set = kNN_param,
                        control = tuning,
                        show.info = TRUE
)    
#[Tune] Result: k=28 : mmce.test.mean=0.5043920


tuned_SVM <- tuneParams(learner = SVM,
                        task = classification,
                        resampling = resampling,
                        par.set = SVM_param,
                        control = tuning,
                        show.info = TRUE
)
#[Tune] Result: kernel=radial; cost=1; gamma=1 : mmce.test.mean=0.5713480


# applying optimal parameters to the models
trees <- setHyperPars(learner = trees, par.vals = tuned_trees$x)
logregression <- setHyperPars(learner = logregression, par.vals = tuned_logit$x)
neuralnet <- setHyperPars(learner = neuralnet, par.vals = tuned_neural$x)
kNN <- setHyperPars(learner = kNN, par.vals = tuned_kNN$x)
SVM <- setHyperPars(learner = SVM, par.vals = tuned_SVM$x)

# training the models
final_trees <- train(learner = trees, task = classification)
final_logregression <- train(learner = logregression, task = classification)
final_neuralnet <- train(learner = neuralnet, task = classification)
final_naivebayes <- train(learner = naivebayes, task = classification)
final_kNN <- train(learner = kNN, task = classification)
final_SVM <- train(learner = SVM, task = classification)

# generating predictions
predict_trees <- predict(final_trees, newdata = cat.test)
predict_logregression <- predict(final_logregression, newdata = cat.test)
predict_neuralnet <- predict(final_neuralnet, newdata = cat.test)
predict_naivebayes <- predict(final_naivebayes, newdata = cat.test)
predict_kNN <- predict(final_kNN, newdata = cat.test)
predict_SVM <- predict(final_SVM, newdata = cat.test)

# evaluating performance
print(performance(predict_trees, measures = mmce))
print(performance(predict_logregression, measures = mmce))
print(performance(predict_neuralnet, measures = mmce))
print(performance(predict_naivebayes, measures = mmce))
print(performance(predict_kNN, measures = mmce))
print(performance(predict_SVM, measures = mmce))



