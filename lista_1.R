printers <- read.table("/home/rafcio/Desktop/Studia/Podstawy statystycznego modelowania/data/file_1.txt")
colnames(printers) <- c("conservation.time", "copy.machines.number")

attach(printers)

# ______________________________________________________________________  CWICZENIA __________________________________________________________________________

plot(conservation.time, copy.machines.number)
hist(conservation.time)

model <- lm(copy.machines.number~conservation.time)
summary(model)

# copy.machines.number = 0.8142 + 2.95 * (conservation.time)

# Intercept - wyraz wolny - punkt przecięcia prostej z osią O, taka będzie wartość danych przy zerowych wartościach pozostałych zmiennych
# conservation.time - o tyle wzrośnie wartość Y jeśli X zmieni się o jedną jednostkę

hist(resid(model))

plot(conservation.time, copy.machines.number)
abline(model)

qqnorm(resid(model))
# Wartości na OX to wartości kwantyli teorerycznych z rozkładu normalnego,
# Wartości na OY to wartości kwantyli z rozkładu residuów modelu
# Jeżeli punkty na wykresie układają się w linię prostą to rozkład jest normalny. Ten wykres nie przedstawia rozkładu normalnego.

printers.sample <- data.frame(X=1:100)
colnames(printers.sample) <- c("conservation.time")
pp <- predict(model, int="p", newdata=printers.sample)
pc <- predict(model, int="c", newdata=printers.sample)
plot(conservation.time, copy.machines.number)
abline(model)
matlines(printers.sample$conservation.time, pc, lty=c(1, 2, 2), col="red")
matlines(printers.sample$conservation.time, pp, lty=c(1, 3, 3), col="blue")

cor(printers)
cor.test(conservation.time, copy.machines.number)


# ______________________________________________________________________ ZADANIA __________________________________________________________________________
# ____________________ ZADANIE 1
library(ISwR)
data <- as.data.frame(kfm)
attach(data)
data$sex <- ifelse(sex=="boy", 1, 0)
# R tworzy sztuczne zmienne, więc gdy mamy do czynienia ze zmiennymi kategorycznymi, to zamieniamy je na liczby. W tym wypadku chłopiec=1, dziewczynka=0.


par(mex=0.5) 
pairs(data, gap = 0, cex.labels = 0.8)
cor(data)
sort(cor(x=data), decreasing = F)[8:12]
  # Niektóre zmienne wykazują między sobą prawdopodobną zależność liniową (np. waga-wzrost) lecz generalnie nie widać zależności liniowej pomiędzy większością zmiennych.
# Warto zwrócić uwagę, że występują dość mocno skorelowane ze sobą zmienne, np. dl.milk-weight lub mat.height-mat.weight

kfm_model <- lm(dl.milk~sex+weight+ml.suppl+mat.weight+mat.height)
summary(kfm_model)
# Największe p-value posiada mat.weight (0.79), więc zostaje wyeliminowana 

kfm_model_step_elimination <- lm(dl.milk~sex+weight+ml.suppl+mat.height)
summary(kfm_model_step_elimination)
# Największe p-value posiada sex (0.12), więc zostaje wyeliminowana

kfm_model_step_elimination <- lm(dl.milk~weight+ml.suppl+mat.height)
summary(kfm_model_step_elimination)
# Największe p-value posiada ml.suppl (0.07), więc zostaje wyeliminowana

kfm_model_step_elimination <- lm(dl.milk~weight+mat.height)
summary(kfm_model_step_elimination)
# Wszystkie zmienne są istotne przynajmniej na poziomie a=0.95
# dl.milk=-11.92 + 1.43 * weight + 0.07 * mat.height
# Ilość spożycia mleka zależy zarówno od wagi jak i wzrostu dziecka, ale na jego spożycie wpływa bardziej waga dziecka niż jego wzrost.
# Wraz ze wzrostem wagi i wzrostu dziecka, wzrasta jego dzienne spożycie mleka.

# ____________________ ZADANIE 2
library(datasets)
data(iris)

setosa.df <- iris[Species=="setosa", ]
versicolor.df <- iris[Species=="versicolor", ]
virginica.df <- iris[Species=="virginica", ]

hist(setosa.df$Sepal.Length, main="Histogram długości kielicha dla gatunku setosa", xlab="Długość kielicha", ylab="Ilość wystąpień")
hist(setosa.df$Sepal.Width, main="Histogram szerokości kielicha dla gatunku setosa", xlab="Szerokość kielicha", ylab="Ilość wystąpień")

hist(versicolor.df$Sepal.Length, main="Histogram długości kielicha dla gatunku versicolor", xlab="Długość kielicha", ylab="Ilość wystąpień")
hist(versicolor.df$Sepal.Width, main="Histogram szerokości kielicha dla gatunku versicolor", xlab="Szerokość kielicha", ylab="Ilość wystąpień")

hist(virginica.df$Sepal.Length, main="Histogram długości kielicha dla gatunku virginica", xlab="Długość kielicha", ylab="Ilość wystąpień")
hist(virginica.df$Sepal.Width, main="Histogram szerokości kielicha dla gatunku virginica", xlab="Szerokość kielicha", ylab="Ilość wystąpień")


setosa.df$Species <- 0
par(mex=0.5) 
pairs(setosa.df[, 1:2], gap = 0, cex.labels = 0.8)
par(mex=0.5) 
pairs(setosa.df[, 3:4], gap = 0, cex.labels = 0.8)
cor(x=setosa.df)
cor.test(setosa.df$Sepal.Length, setosa.df$Petal.Length)
 sort(cor(x=setosa.df), decreasing = T)[6:10] # Zwraca najbardziej skorelowane wartości poprzez sortowanie dataframe


versicolor.df$Species <- 1
par(mex=0.5) 
pairs(versicolor.df[, 1:2], gap = 0, cex.labels = 0.8)
par(mex=0.5) 
pairs(versicolor.df[, 3:4], gap = 0, cex.labels = 0.8)
cor(x=versicolor.df)
cor.test(versicolor.df$Sepal.Length, versicolor.df$Petal.Length)
sort(cor(x=versicolor.df), decreasing = T)[6:10]


virginica.df$Species <- 2
par(mex=0.5) 
pairs(virginica.df[, 1:2], gap = 0, cex.labels = 0.8)
par(mex=0.5) 
pairs(virginica.df[, 3:4], gap = 0, cex.labels = 0.8)
cor(x=virginica.df)
cor.test(virginica.df$Sepal.Length, virginica.df$Petal.Length)
sort(cor(x=virginica.df), decreasing = T)[6:10]


# Długość i szerokość kielicha dla gatunku setosa wykazują zależność liniową, natomiast długość i szerokość płatka dla tego gatunku już takiej zależności nie wykazują.
# Dla gatunku versicolor zależność liniową wykazują długość i szerokość płatka, a długość i szerokość kielicha nie wykazują takiej zależności.
# Gatunek virginica nie wykazuje zależności liniowej dla długiśći i szerokości płatka ani kielicha.

# Największy współczynnik korelacji długości kielicha z długością płatka wykazuje gatunek virginica i wynosi on ~ 0.864. Korelacja tych współczynników dla gatunku setosa
# jest nieistotna statystycznie dla a=0.95, natomiast dla gatunku versicolor i virginica korelacja jest istotna.
# Dla gatunku setosa najbardziej skorelowane są długość i szerokość kielicha
# Dla gatunku versicolor najbardziej skorelowane są długość płatka i szerokość płatka
# Dla gatunku virginica najbardziej skorelowane są długości płatka i kielicha


setosa.model <- lm(setosa.df$Petal.Length ~ setosa.df$Petal.Width + setosa.df$Sepal.Length + setosa.df$Sepal.Width)
summary(setosa.model)

setosa.correction <- lm(setosa.df$Petal.Length ~ setosa.df$Petal.Width + setosa.df$Sepal.Length)
summary(setosa.correction)

setosa.correction <- lm(setosa.df$Petal.Length ~ setosa.df$Petal.Width)
summary(setosa.correction)

# Petal.length = 1.33 + 0.55 * Petal.Width
# Wzrost szerokości płatka o jedną jednostkę powoduje wzrost długości płatka o 0.55 jednostki

