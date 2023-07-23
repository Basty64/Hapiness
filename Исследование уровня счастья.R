setwd("_")

library(openxlsx)
library(ggcorrplot)
library(randomForest)

Data <- read.xlsx("Исследование_Полные_данные.xlsx", sheet = 1)

colnames(Data)[3] <- "Среднегодовой.доход.тыс."
colnames(Data)[4] <- "Объем.потребленного.алкоголя.в.год.л."
colnames(Data)[7] <- "Доля.от.дохода.семьи.которая.тратится.на.продовольствие"
colnames(Data)[9] <- "Издержки.сообщества.на.окружающую.среду.млн."
colnames(Data)[10] <- "Охват.беспроводной.связи.в.сообществе"
colnames(Data)[11] <- "Количество.смертей.от.вирусных.и.респираторных.заболеваний.в.сообществе.тыс.человек"

Data_Learning_All <- Data[Data$Ощущаемое.счастье != "Неизвестно", ]
Data_Forecast<- Data[Data$Ощущаемое.счастье == "Неизвестно", ]

for(i in 1:nrow(Data_Learning_All))
{
  Data_Learning_All$Ощущаемое.счастье[i] <- switch(Data_Learning_All$Ощущаемое.счастье[i],"Hopeless" = 1, "Depressed" = 2, "Suffering" = 3,
                 "Strugglng" = 4,"Coping" = 5,"Just ok" = 6,"Doing well" = 7,"Blooming" = 8,"Thriving" = 9,"Prospering" = 10)
}

sample <- sample(c( TRUE , FALSE ), nrow(Data_Learning_All), replace= TRUE , prob=c( 0.7 , 0.3 ))

Data_Learning <- Data_Learning_All[sample, ]
Data_Test <- Data_Learning_All[!sample, ]

Data_Learning$Ощущаемое.счастье <- as.numeric(Data_Learning$Ощущаемое.счастье)
matrix_condition <- cor(Data_Learning[ ,c(13:26)])
ggcorrplot(matrix_condition, colors = c("black","white","red","darkred"))

ncol(matrix_condition)

koef_kor_frame <- data.frame(matrix_condition[ ,c(14)])
colnames(koef_kor_frame)=c("Коэффициент корреляции с индексом счастья")

matrix_reason_individual <- cor(Data_Learning[ ,c(3:12,13,16,18,21,24)])
ggcorrplot(matrix_reason_individual, colors = c("black","white","red","darkred"))


#Оценка благополучия (13)
factors1 <- names(Data_Learning[ ,c(8,9,10,11,12)])
formula_text1 <- paste0("Оценка.благополучия", " ~ ", paste0(factors1, collapse = "+"))
MyFormula1 <- as.formula(formula_text1)
LM_welfare <-lm( data = Data_Learning, formula = MyFormula1)
summary(LM_welfare)

#Свобода граждан cамостоятельно принимать жизненно важные решения (16)
factors2 <- names(Data_Learning[ ,c(3,4,6,7,8,9,11,12)])
formula_text2 <- paste0("Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения", " ~ ", paste0(factors2, collapse = "+"))
MyFormula2 <- as.formula(formula_text2)
LM_solution <-lm( data = Data_Learning, formula = MyFormula2)
summary(LM_solution)

#Индекс отношения к коррупции (18)
factors3 <- names(Data_Learning[ ,c(3,4,5,6,7)])
formula_text3 <- paste0("Индекс.отношения.к.коррупции", " ~ ", paste0(factors3, collapse = "+"))
MyFormula3 <- as.formula(formula_text3)
LM_Corruption <-lm( data = Data_Learning, formula = MyFormula3)
summary(LM_Corruption)

#Индекс страха социальных конфликтов (21)
factors4 <- names(Data_Learning[ ,c(3,4,6,7,8,9,11,12)])
formula_text4 <- paste0("Индекс.страха.социальных.конфликтов", " ~ ", paste0(factors4, collapse = "+"))
MyFormula4 <- as.formula(formula_text4)
LM_fear <-lm( data = Data_Learning, formula = MyFormula4)
summary(LM_fear)

#Чувство технологического прогресса (24)
factors5 <- names(Data_Learning[ ,c(3,4,6,7,8,9,11,12)])
formula_text5 <- paste0("Чувство.технологического.прогресса", " ~ ", paste0(factors5, collapse = "+"))
MyFormula5 <- as.formula(formula_text5)
LM_technology <-lm( data = Data_Learning, formula = MyFormula5)
summary(LM_technology)


#Оценка.благополучия LM_welfare
#Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения LM_solution
#Индекс.отношения.к.коррупции  LM_Corruption
#Индекс.страха.социальных.конфликтов LM_fear
#Чувство.технологического.прогресса LM_technology


prediction <- predict(object = LM_welfare, newdata = Data_Test)
#Data_Test$Оценка.благополучия <- prediction

Forecast_Test1 <- data.frame("Исходное"= Data_Test$Оценка.благополучия, "Спрогнозированное" = round(prediction))
k <- 0 
for (i in 1:length(Data_Test$Оценка.благополучия)){
  if(Forecast_Test1$Исходное[i]==Forecast_Test1$Спрогнозированное[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)

prediction <- predict(object = LM_solution, newdata = Data_Test)
#Data_Test$Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения <- prediction

Forecast_Test2 <- data.frame("Исходное"= Data_Test$Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения, "Спрогнозированное" = round(prediction))
k <- 0 
for (i in 1:length(Data_Test$Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения)){
  if(Forecast_Test2$Исходное[i]==Forecast_Test2$Спрогнозированное[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)

prediction <- predict(object = LM_Corruption, newdata = Data_Test)

Forecast_Test3 <- data.frame("Исходное"= Data_Test$Индекс.отношения.к.коррупции, "Спрогнозированное" = round(prediction))
k <- 0 
for (i in 1:length(Data_Test$Индекс.отношения.к.коррупции)){
  if(Forecast_Test3$Исходное[i]==Forecast_Test3$Спрогнозированное[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)

prediction <- predict(object = LM_fear, newdata = Data_Test)

Forecast_Test4 <- data.frame("Исходное"= Data_Test$Индекс.страха.социальных.конфликтов, "Спрогнозированное" = round(prediction))
k <- 0 
for (i in 1:length(Data_Test$Индекс.страха.социальных.конфликтов)){
  if(Forecast_Test4$Исходное[i]==Forecast_Test4$Спрогнозированное[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)

prediction <- predict(object = LM_technology, newdata = Data_Test)

Forecast_Test5 <- data.frame("Исходное"= Data_Test$Чувство.технологического.прогресса, "Спрогнозированное" = round(prediction))
k <- 0 
for (i in 1:length(Data_Test$Чувство.технологического.прогресса)){
  if(Forecast_Test5$Исходное[i]==Forecast_Test5$Спрогнозированное[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)



to_10 <- function(x)
{
  if(x > 10)
  {
    x <- 10
  }
  return(x)
}
to_1 <- function(x)
{
  if(x < 1)
  {
    x <- 1
  }
  return(x)
}

factors <- names(Data_Learning[ ,c(13,16,18,21,24)])
formula_text <- paste0("Ощущаемое.счастье", " ~ ", paste0(factors, collapse = "+")) #переменная для формулы
MyFormula <- as.formula(formula_text)#теперь надо преобразовать строку в тип формулы
Model <- lm(data = Data_Learning[c(13,16,18,21,24,26)], formula = MyFormula) 
summary(Model) 


prediction <- predict(object = Model, newdata = Data_Test)
Data_Test$predictions <- round(prediction)
Data_Test$predictions <-(Data_Test[ ,27])
Data_Test$predictions <-(Data_Test[ ,27])
# 41%

k <- 0 
for (i in 1:length(Data_Test$Ощущаемое.счастье)){
  if(Data_Test$predictions[i]==Data_Test$Ощущаемое.счастье[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)


MyFormula <- "Ощущаемое.счастье ~ Оценка.благополучия+Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения+Индекс.отношения.к.коррупции+
Индекс.страха.социальных.конфликтов+Чувство.технологического.прогресса"
for (i in 4:10){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[13], "^", i, ")",sep="")}
for (i in 9:10){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[16], "^", i, ")",sep="")
}
for (i in 5:6){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[18], "^", i, ")",sep="")}

for (i in 5:10){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[21], "^", i, ")",sep="")
}
for (i in 6:8){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[24], "^", i, ")",sep="")}
#------------------------------------------------------------------------------------------------------------------
for (i in 1:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[16],")^", i, ")",sep="")}
for (i in 1:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[21],")^", i, ")",sep="")
}
for (i in 1:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[18],")^", i, ")",sep="")
}
for (i in 1:2){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[24],")^", i, ")",sep="")
}
for (i in 3:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[21],")^", i, ")",sep="")
}
for (i in 1:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[24],")^", i, ")",sep="")
}

#-------------------------------------------------------------------------------------------------------------------------------
for (i in 1:4){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[16],"*",names(Data_Learning)[18],")^", i, ")",sep="")
}
for (i in 1:3){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[21],"*",names(Data_Learning)[18],")^", i, ")",sep="")
}
for (i in 1:4){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[24],"*",names(Data_Learning)[18],")^", i, ")",sep="")
}
for (i in 1:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[18],"*",names(Data_Learning)[25],")^", i, ")",sep="")
}
#----------------------------------------------------------------------------------------------------------------------------------------
for (i in 6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[16],"*",names(Data_Learning)[18],"*",names(Data_Learning)[24],")^", i, ")",sep="")
}

for (i in 1){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[18],"*",names(Data_Learning)[24],"*",names(Data_Learning)[21],"*",names(Data_Learning)[13],")^", i, ")",sep="")
}
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[13],"^", i, "))",sep="")}

for (i in 3:4){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[18],"^", i, "))",sep="")
}
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"*",names(Data_Learning)[21],"^", i, "))",sep="")}

for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[24],"*",names(Data_Learning)[13],"^", i, "))",sep="")
}
for (i in 4:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[21],"*",names(Data_Learning)[24],"^", i, "))",sep="")
}
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[13],"^", i, "))",sep="")
}
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[24],"^", i, "))",sep="")
}
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[21],"^", i, "))",sep="")
}
for (i in 3:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[18],"*",names(Data_Learning)[16],"^", i, "))",sep="")
}
for (i in 5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[21],"^", i, "))",sep="")}


for (i in 2:5){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"*",names(Data_Learning)[24],"^", i, "))",sep="")
}

#------------------------------------------------------------------------------------------------------------------------------
for (i in 2:3){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[16], "^(1/", i, "))",sep="")
}
for (i in 2:3){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[16], "^(1/", i, "))",sep="")
}
for (i in 2:3){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[21], "^(1/", i, "))",sep="")
}
for (i in 2){
  MyFormula <- paste (MyFormula, "+I(", names(Data_Learning)[24], "^(1/", i, "))",sep="")}
for (i in 3:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"^", i, "*",names(Data_Learning)[24],"^", i, "))",sep="")}

for (i in 3:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[16],"^", i, "*",names(Data_Learning)[21],"^", i, "))",sep="")
}
for (i in 3:6){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[13],"^", i, "*",names(Data_Learning)[18],"^", i, "))",sep="")
}
for (i in 3:4){
  MyFormula <- paste (MyFormula, "+I((" ,names(Data_Learning)[21],"^", i, "*",names(Data_Learning)[24],"^", i, "))",sep="")}

Model <- lm(data = Data_Learning[c(13,16,18,21,24,26)], formula = MyFormula) 
summary(Model) 


prediction <- predict(object = Model, newdata = Data_Test)
Data_Test$predictions <- round(prediction)
Data_Test$predictions <-(sapply(Data_Test[ ,27],to_10))
Data_Test$predictions <-(sapply(Data_Test[ ,27],to_1))

k <- 0 
for (i in 1:length(Data_Test$Ощущаемое.счастье)){
  if(Data_Test$predictions[i]==Data_Test$Ощущаемое.счастье[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)*100





#Прогноз на основе модели случайного леса
model <- randomForest(formula = Ощущаемое.счастье ~ .,data = Data_Learning[ ,c(13,16,18,21,24,26)])
which.min(model$mse)
varImpPlot(model)

prediction <- predict(object = model, newdata = Data_Test)
Data_Test$predictions <- round(prediction)
Data_Test$predictions <-(sapply(Data_Test[ ,27],to_10))
Data_Test$predictions <-(sapply(Data_Test[ ,27],to_1))

k <- 0 
for (i in 1:length(Data_Test$Ощущаемое.счастье)){
  if(Data_Test$predictions[i]==Data_Test$Ощущаемое.счастье[i]){
    k <- k+1
  }
}
k/nrow(Data_Test)*100


#Оценка.благополучия LM_welfare
#Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения LM_solution
#Индекс.отношения.к.коррупции  LM_Corruption
#Индекс.страха.социальных.конфликтов LM_fear
#Чувство.технологического.прогресса LM_technology


prediction <- predict(object = LM_welfare, newdata = Data_Forecast)
Data_Forecast$Оценка.благополучия <- prediction

prediction <- predict(object = LM_solution, newdata = Data_Forecast)
Data_Forecast$Свобода.граждан.самостоятельно.принимать.жизненно.важные.решения <- prediction

prediction <- predict(object = LM_Corruption, newdata = Data_Forecast)
Data_Forecast$Индекс.отношения.к.коррупции<- prediction

prediction <- predict(object = LM_fear, newdata = Data_Forecast)
Data_Forecast$Индекс.страха.социальных.конфликтов <- prediction

prediction <- predict(object = LM_technology, newdata = Data_Forecast)
Data_Forecast$Чувство.технологического.прогресса <- prediction

prediction <- predict(object = model, newdata = Data_Forecast)
Data_Forecast$predictions <- round(prediction)

Data_Forecast$predictions <-(sapply(Data_Forecast[ ,27],to_10))
Data_Forecast$predictions <-(sapply(Data_Forecast[ ,27],to_1))





for(i in 1:nrow(Data_Forecast))
{
  if(Data_Forecast$predictions[i] == 1)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Hopeless"
  }
  else if(Data_Forecast$predictions[i] == 2)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Depressed"
  }
  else if(Data_Forecast$predictions[i] == 3)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Suffering"
  }
  else if(Data_Forecast$predictions[i] == 4)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Strugglng"
  }
  else if(Data_Forecast$predictions[i] == 5)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Coping"
  }
  else if(Data_Forecast$predictions[i] == 6)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Just ok"
  }
  else if(Data_Forecast$predictions[i] == 7)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Doing well"
  }
  else if(Data_Forecast$predictions[i] == 8)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Blooming"
  }
  else if(Data_Forecast$predictions[i] == 9)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Thriving"
  }
  else if(Data_Forecast$predictions[i] == 10)
  {
    Data_Forecast$Ощущаемое.счастье[i] <- "Prospering"
  }
}
write.xlsx(Data_Forecast, "Вар55_Forecast1.xlsx")

write.xlsx(Data_Forecast, "Вар55_Forecast2.xlsx")
