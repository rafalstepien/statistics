# _________________________________________ ĆWICZENIA _____________________________________________
data <- stackloss
data_corrected <- data[c(-21), ]

model <- lm(data$stack.loss~data$Air.Flow)
summary(model)

model_correction <- lm(data_corrected$stack.loss~data_corrected$Air.Flow)
summary(model_correction)

plot(data$Air.Flow, data$stack.loss, main = "Zaleznosc liniowa Air.Flow ~ stack.loss", col=ifelse(data$Air.Flow==70, "red", "blue"))
abline(lm(data_corrected$stack.loss~data_corrected$Air.Flow), col="blue")
abline(lm(data$stack.loss~data$Air.Flow), col="red")

reszty.stud <- rstudent(model)
outliers <- reszty.stud[abs(reszty.stud) > 2]

library(car)
outlierTest(model)

influencePlot(model, id.method="identify", id.n = 1, id.cex = 1,  id.col = palette()[1] , main = "Wykres wpływów" ,sub ="Rozmiar okręgów
jest proporcjonalny do odległości Cooka")

influence.measures(model)

corrected_data <- data[c(-21, -2, -4), ]
corrected_model <- lm(corrected_data$stack.loss~corrected_data$Air.Flow)
summary(corrected_model)

plot(corrected_data$Air.Flow, corrected_data$stack.loss)
abline(lm(corrected_data$stack.loss~corrected_data$Air.Flow))
abline(lm(data$stack.loss~data$Air.Flow), col="red")

# _________________________________________ ZADANIA _____________________________________________
# ____________________ ZADANIE 3
data <- stackloss

model <- lm(data$stack.loss~data$Water.Temp)
summary(model)

plot(data$Water.Temp, data$stack.loss, 
     main = "Zależność efektywności rośliny od temperatury wody w cewkach", 
     col = ifelse((data$stack.loss == 15 & data$Water.Temp == 23) | 
                    (data$stack.loss == 37 & data$Water.Temp == 25) |
                    (data$stack.loss == 42), "red", "blue"), 
     xlab = "Temperatura wody", 
     ylab = "efektywność rośliny",
     pch = ifelse((data$stack.loss == 15 & data$Water.Temp == 23) | 
                    (data$stack.loss == 37 & data$Water.Temp == 25) |
                    (data$stack.loss == 42), 19, 20),)

abline(lm(data$stack.loss~data$Water.Temp), col="red")

data_corrected <- data[c(-1, -3, -9), ]

plot(data_corrected$Water.Temp, data_corrected$stack.loss, 
     main = "Zależność efektywności rośliny od temperatury wody w cewkach", 
     col = "blue", 
     xlab = "Temperatura wody", 
     ylab = "efektywność rośliny",
     pch = 20)
abline(lm(data_corrected$stack.loss~data_corrected$Water.Temp), col="green")

summary(lm(data$stack.loss~data$Water.Temp))
summary(lm(data_corrected$stack.loss~data_corrected$Water.Temp))

reszty.st <-rstudent(model)
outliers <- reszty.st[abs(reszty.st) > 2]

outlierTest(model)

influencePlot(model, id.method="identify", id.n = 1, id.cex = 1,  id.col = palette()[1] , main = "Wykres wpływów")

influence.measures(model)

data_without_influence <- data[c(-1, -2), ]
model <- lm(data_without_influence$stack.loss ~ data_without_influence$Water.Temp)
summary(model)

plot(data_without_influence$Water.Temp, data_without_influence$stack.loss, 
     main = "Zależność efektywności rośliny od temperatury wody w cewkach", 
     col = "blue", 
     xlab = "Temperatura wody", 
     ylab = "efektywność rośliny",
     pch = 20)
abline(lm(data_without_influence$stack.loss ~ data_without_influence$Water.Temp), col="red")

# ____________________ ZADANIE 4
data <- read.table("/home/rafcio/Desktop/Studia/Podstawy statystycznego modelowania/data/file_2.txt", header = F)
colnames(data) <- c("Speed","Concentration")

plot(data$Speed, data$Concentration,
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",
     )

par(mfrow=c(1, 1))
plot(log(data$Speed), data$Concentration,
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",)
lm.out1 <- lm(log(data$Speed) ~ data$Concentration)
plot(lm.out1$fitted, lm.out1$resid)

plot(data$Speed, log(data$Concentration),
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",)
lm.out2 <- lm(data$Speed ~ log(data$Concentration))
plot(lm.out2$fitted, lm.out2$resid)

plot(log(data$Speed), log(data$Concentration),
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",)
lm.out3 <- lm(log(data$Speed) ~ log(data$Concentration))
plot(lm.out3$fitted, lm.out3$resid)

# DRUGI STOPIEN
plot(data$Concentration, data$Speed,
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",
)

model <- lm(data$Speed ~ data$Concentration + I(data$Concentration^2))
summary(model)
curve(10.69371 + 2.77768 * x - 0.09782 * x^2, add=T)

# SZOSTY STOPIEN
model <- lm(data$Speed ~ data$Concentration + I(data$Concentration^2) + I(data$Concentration^3) + I(data$Concentration^4) + I(data$Concentration^5))
summary(model)

plot(data$Concentration, data$Speed,
     main = "Zależność między prędkością a koncentracją",
     col = "blue",
     pch = 20,
     xlab = "Prędkość",
     ylab = "Koncentraja",
)
curve(-40.71269 + 102.89802*x - 56.15630*x^2 + 12.43751*x^3 - 1.18296*x^4 + 0.04028*x^5, add=T)