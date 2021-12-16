library(caret)

#데이터 읽기
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)
#훈련, 테스트 데이터 분류(6:4)
ucla_sample <- createDataPartition(y = ucla$admit, p = 0.6, list = F)
train_data <- ucla[ucla_sample,]
test_data <- ucla[-ucla_sample,]


#결정트리 모델링
r = train(admit~., data = train_data, method = "rpart")
print(r)

#결정트리 테스트데이터 예측
table(predict(r, test_data), test_data$admit)

#결정트리 혼동행렬 분석
caret::confusionMatrix(predict(r, test_data), test_data$admit)


#랜덤포레스트(50)모델링
f = train(admit~., data = train_data, method = "rf", ntree=50)
print(f)

#랜덤포레스트(50) 테스트데이터 예측
table(predict(f, test_data), test_data$admit)

#랜덤포레스트(50) 혼동행렬 분석
caret::confusionMatrix(predict(f, test_data), test_data$admit)


#랜덤포레스트(100)모델링
f2 = train(admit~., data = train_data, method = "rf", ntree=100)
print(f2)

#랜덤포레스트(100) 테스트데이터 예측
table(predict(f2, test_data), test_data$admit)

#랜덤포레스트(100) 혼동행렬 분석
caret::confusionMatrix(predict(f2, test_data), test_data$admit)


#k-nn 모델링
k = train(admit~., data = train_data, method = "knn")
print(k)

#k-nn 테스트데이터 예측
table(predict(k, test_data), test_data$admit)

#k-nn 혼동행렬 분석
caret::confusionMatrix(predict(k, test_data), test_data$admit)


#SVM(radial basis) 모델링
sr = train(admit~., data = train_data, method = "svmRadial")
print(sr)

#SVM(radial basis) 테스트데이터 예측
table(predict(sr, test_data), test_data$admit)

#SVM(radial basis) 혼동행렬 분석
caret::confusionMatrix(predict(sr, test_data), test_data$admit)


#SVM(polynimials) 모델링
sp = train(admit~., data = train_data, method = "svmPoly")
print(sp)

#SVM(polynimials) 테스트데이터 예측
table(predict(sp, test_data), test_data$admit)

#SVM(polynimials) 혼동행렬 분석
caret::confusionMatrix(predict(sp, test_data), test_data$admit)
