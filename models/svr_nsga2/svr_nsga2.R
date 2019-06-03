library("nsga2R")
library("caret")
library("ggplot2")
library("mco")

data = read.csv("/Users/b1017579/Documents/Projects/Fish/fish-stock-optimiser/data/raw/fish0.01.csv")

fit_svr = function(dat, target, predictors){
    ctrl = caret::trainControl(
        method = "repeatedcv",
        repeats = 3,
    )
    print(paste(target, "~", predictors, sep=" "))
    
    svrFit = caret::train(
        as.formula(paste(target, "~", predictors, sep=" ")),
        # safety ~ k1 + k2,
        data = dat,
        method = 'svmRadial',
        tuneLength = 5,
        trControl = ctrl,
        metric = "Rsquared",
        # C = 0.1,
        # sigma = 0.1,
    )
    return(svrFit)
}



svr_fun = function(ind){
    ind = t(data.frame(ind))
    print(paste("ind: ", ind))
   prediction_safety = predict(svr_safety,ind)
   prediction_kobe= predict(svr_kobe,ind)
   result = c(prediction_safety, prediction_kobe)
   print(paste("result:",result))
   return(result)
}

svr_safety = fit_svr(data, "safety", "k1 + k2")$finalModel
svr_kobe = fit_svr(data, "kobe", "k1 + k2")$finalModel

pareto_front = nsga2(fn = svr_fun, idim = 2, odim = 2, lower.bounds = 0, upper.bounds = 1.4, popsize = 400, generations = 100, cprob = 0.9)
print(pareto_front)
