# ____________________ ZADANIE 5
data <- read.table("/home/rafcio/Desktop/Studia/Podstawy statystycznego modelowania/data/file_3.txt", header = TRUE)

require(MASS)
lda.classifier <- lda(owner ~ income + lotsize, data=data)
print(lda.classifier)
summary(lda.classifier)

model <- lm(owner ~ ., data=data)
summary(model)

LD1 <- predict(lda.classifier)$x[, 1]
sum(LD1 * (data$owner == 1)) / sum(data$owner == 1)
sum(LD1 * (data$owner == 2)) / sum(data$owner == 2)

lda.classifier$svd

test_set <- data[, 1:2]
data.predict <- predict(lda.classifier, test_set)
data.predict$class
data[, 3]

test_set <- data[, 1:2]
data.predict <- predict(lda.classifier, test_set)
data.predict$posterior

data.classify <- data.predict$class
data.classperc <- sum(data.classify == data[, 3])/length(data[, 3])
data.classperc

table(Original=data$owner, Predicted=data.classify, dnn=c("Prawdziwe","Przewidziane"))

# ____________________ ZADANIE 5
wine <- read.csv("/home/rafcio/Desktop/Studia/Podstawy statystycznego modelowania/data/file_4.txt", header = FALSE)

lda.wine <- lda(wine$V1 ~ ., data=wine)
print(lda.wine)

model <- lm(wine$V1 ~ ., data=wine)
summary(model)

LD1 <- predict(lda.wine)$x[, 1]
LD2 <- predict(lda.wine)$x[, 2]

sum(LD1 * (wine$V1 == 1)) / sum(wine$V1 == 1)
sum(LD2 * (wine$V1 == 1)) / sum(wine$V1 == 1)

sum(LD1 * (wine$V1 == 2)) / sum(wine$V1 == 2)
sum(LD2 * (wine$V1 == 2)) / sum(wine$V1 == 2)

sum(LD1 * (wine$V1 == 3)) / sum(wine$V1 == 3)
sum(LD2 * (wine$V1 == 3)) / sum(wine$V1 == 3)

test.wine <- wine[, 2:14]
wine.predict <- predict(lda.wine, test.wine)
wine.predict$posterior[20,]

wine.classify <- wine.predict$class
wine.classperc <- sum(wine.classify == wine[, 1])/length(wine[, 1])
wine.classperc

table(Original=wine$V1, Predicted=wine.classify)
