library("nsga2R")
library("caret")
library("ggplot2")
library("mco")


fit_svr = function(dat, target, predictors, tuneLength=5){
    ctrl = caret::trainControl(
        method = "repeatedcv",
        repeats = 5,
    )
    print(paste(target, "~", predictors, sep=" "))
    
    svrFit = caret::train(
        as.formula(paste(target, "~", predictors, sep=" ")),
        data = dat,
        method = 'svmRadial',
        tuneLength = tuneLength,
        trControl = ctrl,
        metric = "MAE",
    )
    return(svrFit)
}

svr_fun = function(ind){
    ind = t(data.frame(ind))
    prediction_1 = predict(svr_1,ind)
    prediction_2= predict(svr_2,ind)
    result = c(prediction_1, prediction_2)
    return(result)
}


data = read.csv("/Users/b1017579/Documents/Projects/Fish/fish-stock-optimiser/data/raw/fish0.01.csv")
first_reward = 'safety'
second_reward = 'yield'

predictors = 'k1 + k2'

svr_1 = fit_svr(data, first_reward, predictors, tuneLength = 15)$finalModel
svr_2 = fit_svr(data, second_reward, predictors, tuneLength = 15)$finalModel

pareto_front = mco::nsga2(fn = svr_fun, idim = 2, odim = 2, lower.bounds = rep(0, 2), upper.bounds = rep(1.4, 2), popsize = 1000, generations = 100, cprob = 0.9)

ggplot(data=data.frame(pareto_front$value), aes(x=X1, y=X2)) + geom_point() + xlab(first_reward) + ylab(second_reward)
