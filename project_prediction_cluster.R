library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
set.seed(92397)

# analysis 2: using k-means clusters data
# breaking up the data into testing and training sets
n <- nrow(movies.cat)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
cluster.train <- movies.cluster[train,]
cluster.test <- movies.cluster[test,]

# creating the a new task
classification.cluster <- makeClassifTask(id="cluster task", data = cluster.train, 
                                          target = "cluster")
resampling <- makeResampleDesc(method = "CV", iters = 6)
tuning <- makeTuneControlRandom(maxit = 10)

# tuning the cluster models

tuned_trees2 <- tuneParams(learner = trees,
                           task = classification.cluster,
                           resampling = resampling,
                           par.set = tree_param,
                           control = tuning,
                           show.info = TRUE
)
#[Tune] Result: minsplit=26; minbucket=7; cp=0.00717 : mmce.test.mean=0.2690068


tuned_logit2 <- tuneParams(learner = logregression,
                           task = classification.cluster,
                           resampling = resampling,
                           par.set = logit_param,
                           control = tuning,
                           show.info = TRUE
)
#[Tune] Result: lambda=0.0458; alpha=0.595 : mmce.test.mean=0.2838563


tuned_neural2 <- tuneParams(learner = neuralnet,
                            task = classification.cluster,
                            resampling = resampling,
                            par.set = neural_param,
                            control = tuning,
                            show.info = TRUE
)
#[Tune] Result: size=8; decay=0.428; maxit=1000 : mmce.test.mean=0.2559891

tuned_kNN2 <- tuneParams(learner = kNN,
                         task = classification.cluster,
                         resampling = resampling,
                         par.set = kNN_param,
                         control = tuning,
                         show.info = TRUE
)    
#[Tune] Result: k=17 : mmce.test.mean=0.2762789


tuned_SVM2 <- tuneParams(learner = SVM,
                         task = classification.cluster,
                         resampling = resampling,
                         par.set = SVM_param,
                         control = tuning,
                         show.info = TRUE
)
#[Tune] Result: kernel=radial; cost=1; gamma=4 : mmce.test.mean=0.2953655

# applying optimal parameters to the models
trees2 <- setHyperPars(learner = trees, par.vals = tuned_trees2$x)
logregression2 <- setHyperPars(learner = logregression, par.vals = tuned_logit2$x)
neuralnet2 <- setHyperPars(learner = neuralnet, par.vals = tuned_neural2$x)
kNN2 <- setHyperPars(learner = kNN, par.vals = tuned_kNN2$x)
SVM2 <- setHyperPars(learner = SVM, par.vals = tuned_SVM2$x)


# training the models
final_trees2 <- train(learner = trees, task = classification.cluster)
final_logregression2 <- train(learner = logregression, task = classification.cluster)
final_neuralnet2 <- train(learner = neuralnet, task = classification.cluster)
final_naivebayes2 <- train(learner = naivebayes, task = classification.cluster)
final_kNN2 <- train(learner = kNN, task = classification.cluster)
final_SVM2 <- train(learner = SVM, task = classification.cluster)


# generating predictions
predict_trees2 <- predict(final_trees2, newdata = cluster.test)
predict_logregression2 <- predict(final_logregression2, newdata = cluster.test)
predict_neuralnet2 <- predict(final_neuralnet2, newdata = cluster.test)
predict_naivebayes2 <- predict(final_naivebayes2, newdata = cluster.test)
predict_kNN2 <- predict(final_kNN2, newdata = cluster.test)
predict_SVM2 <- predict(final_SVM2, newdata = cluster.test)


# evaluating performance
print(performance(predict_trees2, measures = mmce))
print(performance(predict_logregression2, measures = mmce))
print(performance(predict_neuralnet2, measures = mmce))
print(performance(predict_naivebayes2, measures = mmce))
print(performance(predict_kNN2, measures = mmce))
print(performance(predict_SVM2, measures = mmce))