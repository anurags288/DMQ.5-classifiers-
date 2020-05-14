#CLASSIFICATION USING NAIVE BAYES
    library(klaR)
    library(e1071)
    library(rpart)
    library(rpart.plot)
    library(carat)
	library(caret)
        
    #training set 75%
    
    s<-sample(150,113)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    model<-naiveBayes(Species~.,data=iris_train)
    pred<-predict(model,iris_test)
    conf<-caret::confusionMatrix(pred,iris_test$Species)$table
    accuracy1<-((sum(diag(conf)))/sum(conf))*100
    
    #training set 66.6%(2/3)
        
    s<-sample(150,100)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    model<-naiveBayes(Species~.,data=iris_train)
    pred<-predict(model,iris_test)
    conf<-confusionMatrix(pred,iris_test$Species)$table
    accuracy2<-((sum(diag(conf)))/sum(conf))*100
    
    if(accuracy1>accuracy2)
    {
        cat("Training set of 75% records is better")
    }
    else
    {
        cat("Training set of 66.6% records is better")
    }
    
    #holdout method
    
    s<-sample(150,75)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    model<-naiveBayes(Species~.,data=iris_train)
    pred<-predict(model,iris_test)
    conf<-confusionMatrix(pred,iris_test$Species)$table
    acchld<-((sum(diag(conf)))/sum(conf))*100
    print("Accuracy of holdout method is ")
	acchld
        
    #Random subsampling
    
    i<-75
    j<-1
    acc<-c()
    for(i in 75:100)
    {
        s<-sample(150,i)
        iris_train<-iris[s,]
        iris_test<-iris[-s,]
        model<-naiveBayes(Species~.,data=iris_train)
        pred<-predict(model,iris_test)
        conf<-confusionMatrix(pred,iris_test$Species)$table
        acc[j]<-c((sum(diag(conf))/sum(conf))*100,acc)
        j=j+1
    }
    accrs<-mean(acc)
    cat("Accuracy of random subsampling is ",accrs)

    #Cross validation method
    x=iris[,-5]
    y=iris$Species
    model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
    cons<-table(predict(model$finalModel,x)$class,y)
    accv<-(sum(diag(cons))/sum(cons))*100
        
    #comparison of holdout,random subsampling and  cross validation
    greatest<-max(acchld,accrs,accv)
    if(greatest==acchld)
    {
        cat("Holdout method does best classification")
    }
    else if(greatest==accrs)
    {
        cat("Random subsampling method does best classification")
    }
	else
    {
        cat("cross validation method does best classification")
    }
    
    #data scaled to standard format
    data_std <- function(x) 
    { 
        (x-mean(x))/sd(x) 
    }

    sapply(iris[,-5],data_std)
    s<-sample(150,90)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    model<-naiveBayes(Species~.,data=iris_train)
    pred<-predict(model,iris_test)
    conf<-confusionMatrix(pred,iris_test$Species)$table
    accsd<-((sum(diag(conf)))/sum(conf))*100
    cat("Accuracy of standardise datset is",accsd,"%")






#knearest classifier
    library(rpart)
    library(rpart.plot)
    library(caret)
    library(e1071)
    library(class)

    data_norm<-function(x)
    {
        (x-mean(x))/(sd(x))
    }
    iris_norm<-sapply(iris[,-5],data_norm)
    
    #training set 75%

    iris_train<-iris_norm[1:113,]
    iris_test<-iris_norm[114:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:113,5],k=13)
    con<-table(iris_pred,iris[114:150,5])
    accuracy<-((sum(diag(con)))/sum(con))*100
    cat("Accuracy with 75% training set is ",accuracy,"%")
        
    #training set 66.6%

    iris_train<-iris_norm[1:100,]
    iris_test<-iris_norm[101:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:100,5],k=13)
    con<-table(iris_pred,iris[101:150,5])
    accu66<-((sum(diag(con)))/sum(con))*100
    cat("Accuracy with 75% training set is ",accu66,"%")
        
    #holdout method

    iris_train<-iris_norm[1:75,]
    iris_test<-iris_norm[76:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:75,5],k=13)
    con<-table(iris_pred,iris[76:150,5])
    acchld<-((sum(diag(con)))/sum(con))*100
    cat("Accuracy with hold out is ",acchld,"%")
    
    
    #random subsampling
    #training sets are 80

    iris_train<-iris_norm[1:80,]
    iris_test<-iris_norm[81:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:80,5],k=13)
    con<-table(iris_pred,iris[81:150,5])
    accu1<-((sum(diag(con)))/sum(con))*100
    
    
    #training sets are 90
    
    iris_train<-iris_norm[1:90,]
    iris_test<-iris_norm[91:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:90,5],k=13)
    con<-table(iris_pred,iris[91:150,5])
    accu2<-((sum(diag(con)))/sum(con))*100
    
    accrs<-max(accu1,accu2)
    cat("Accuracy with random subsampling is",accrs)
    
    
    #cross validation
    
    x=iris[,-5]
    y=as.factor(iris$Species)
    res<-knn.cv(x,y,1:length(y))
    con<-table(res,y)
    accucs<-((sum(diag(con)))/sum(con))*100
    cat("Accuracy with cross validation is",accrs)
    
    #comparison of holdout,random subsampling and  cross validation

    greatest<-max(acchld,accrs,accucs)
    if(greatest==acchld)
    {
        print("Holdout method does best classification")
    }
    else if(greatest==accrs)
    {
        print("Random subsampling method does best classification")
    }
    else
    {
        cat("cross validation method does best classification")
    }
    
    #data scaled to standard format

    data_std <- function(x) 
    {
        (x-mean(x))/sd(x)
    }
    sapply(iris[,-5],data_std)
    
    iris_train<-iris_norm[1:100,]
    iris_test<-iris_norm[101:150,]
    iris_pred<-knn(iris_train,iris_test,iris[1:100,5],k=13)
    con<-table(iris_pred,iris[101:150,5])
    accusd<-((sum(diag(con)))/sum(con))*100
    cat("Accuracy of sd is",accusd)


    #decisiontreeclassifier
    library(rpart)
    library(e1071)
    library(rpart.plot)
    library(caret)
    
    #75% training set

    s<-sample(150,113)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    dtm<-rpart(Species~.,iris_train,method="class")
    rpart.plot(dtm)
    p<-predict(dtm,iris_test,type="class")
    cn<-confusionMatrix(iris_test[,5],p)$table
    print(cn)
    accu<-(sum(diag(cn))/sum(cn))*100
    cat("The accuracy with 75% training data is ",accu,"%")
    
    #66.6% taining set
    
    s<-sample(150,100)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    dtm<-rpart(Species~.,iris_train,method="class")
    rpart.plot(dtm)
    p<-predict(dtm,iris_test,type="class")
    cn<-confusionMatrix(iris_test[,5],p)$table
    print(cn)
    accu6<-(sum(diag(cn))/sum(cn))*100
    cat("The accuracy with 66.6% training data is ",accu6,"%")
        
    #Holdout method

    s<-sample(150,75)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    dtm<-rpart(Species~.,iris_train,method="class")
    rpart.plot(dtm)
    p<-predict(dtm,iris_test,type="class")
    cn<-confusionMatrix(iris_test[,5],p)$table
    acchld<-(sum(diag(cn))/sum(cn))*100
    cat("The accuracy with hold out method is ",acchld,"%")
    
    #Random subsampling
    
    i<-75
    j<-1
    acc<-c()
    for(i in 75:100)
    {
        s<-sample(150,i)
        iris_train<-iris[s,]
        iris_test<-iris[-s,]
        dtm<-rpart(Species~.,iris_train,method="class")
        rpart.plot(dtm)
        p<-predict(dtm,iris_test,type="class")
        cn<-confusionMatrix(iris_test[,5],p)$table
        acc[j]<-c((sum(diag(cn))/sum(cn))*100,acc)
        j=j+1
    }
    acrs<-mean(acc)
    
    cat("The accuracy with random subsampling method is ",acrs,"%")
    
    #cross validation method

    library(plyr)
    library(rpart)
    set.seed(123)
    form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
    folds <- split(iris, cut(sample(1:nrow(iris)),10))
    errs <- rep(NA, length(folds))
    i<-1
    for (i in 1:length(folds))
    {
        test <- ldply(folds[i], data.frame)
        train <- ldply(folds[-i], data.frame)
        tmp.model <- rpart(form , train, method = "class")
        tmp.predict <- predict(tmp.model, newdata = test, type = "class")
        conf.mat <- table(test$Species, tmp.predict)
        errs[i] <-1- sum(diag(conf.mat))/sum(conf.mat)
    }
    accv<-(1-mean(errs))*100
    cat("The average accutracy using k-fold cross validation is",accv,"%")
    
    #comparison of holdout,random subsampling and cross validation

    greatest<-max(acchld,acrs,accv)
    if(greatest==acchld)
    {
        cat("Holdout method does best classification")
    }
    else if(greatest==acrs)
    {
        cat("Random subsampling method does best classification")
    }
    else
    {
        cat("cross validation method does best classification")
    }
    
    #standard normal form

    data_std <- function(x) 
    {
        (x-mean(x))/sd(x)
    }
    sapply(iris[,-5],data_std)
    s<-sample(150,90)
    iris_train<-iris[s,]
    iris_test<-iris[-s,]
    dtm<-rpart(Species~.,iris_train,method="class")
    rpart.plot(dtm)
    p<-predict(dtm,iris_test,type="class")
    cn<-confusionMatrix(iris_test[,5],p)$table
    accsd<-(sum(diag(cn))/sum(cn))*100
    cat("The accuracy in standardised iris data is ",accsd,"%")


