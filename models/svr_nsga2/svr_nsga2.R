library("nsga2R")
library("caret")
library("ggplot2")
library("mco")

data = read.csv("/Users/b1017579/Documents/Projects/Fish/fish-stock-optimiser/data/raw/fish0.01.csv")

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
    prediction_safety = predict(svr_safety,ind)
    prediction_kobe= predict(svr_kobe,ind)
    result = c(prediction_safety, prediction_kobe)
    return(result)
}

svr_safety = fit_svr(data, "safety", "k1 + k2", 5)$finalModel
svr_kobe = fit_svr(data, "yield", "k1 + k2", 5)$finalModel

pareto_front = mco::nsga2(fn = svr_fun, idim = 2, odim = 2, lower.bounds = rep(0, 2), upper.bounds = rep(1.4, 2), popsize = 400, generations = 100, cprob = 0.9)
print(pareto_front)

ggplot(data=data.frame(pareto_front$value), aes(x=X1, y=X2)) + geom_point() + xlab("safety") + ylab("kobe")
