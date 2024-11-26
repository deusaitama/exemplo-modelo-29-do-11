
install.packages("caret")
install.packages("class")
install.packages("e1071")
install.packages("MASS")
install.packages("rattle")
install.packages("rpart")

library(caret)
library(class)
library(e1071)
library(MASS)
library(rattle)
library(rpart)

cancer2<-Exercício_3_Dados_cancer2

#preparação dos dados

cancer2<-cancer2[-1]


# Função de normalização

minmax<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Separando atributos normalizados

cancer2_atributos<-as.data.frame(lapply(cancer2[2:8],minmax))

#Separando em rotulos

cancer2_rotulos<-factor(cancer.2$Grupo,levels=c("tem câncer","não tem câncer"))


# Separando em treino e teste

set.seed(1234)
sorteio<-sample(1:362,290)

cancer2_atributos_treino<-cancer2_atributos[sorteio,]
cancer2_atributos_teste<-cancer2_atributos[-sorteio,]
cancer2_rotulos_treino<-cancer2_rotulos[sorteio]
cancer2_rotulos_teste<-cancer2_rotulos[-sorteio]

# 2 - KNN

set.seed(1234)
knn_predicoes<-knn(train = cancer2_atributos_treino,
                   test = cancer2_atributos_teste,
                   cl = cancer2_rotulos_treino,
                   k=30)

confusionMatrix(knn_predicoes,cancer2_rotulos_teste)

#3-Naive Bayes

set.seed(1234)
naive_bayes<-naiveBayes(cancer2_atributos_treino,cancer2_rotulos_treino)
naive_bayes_predicoes<-predict(naive_bayes,cancer2_atributos_teste)

confusionMatrix(naive_bayes_predicoes,cancer2_rotulos_teste)

# 4 - Arvore de decisão

cancer2$Grupo<-factor(cancer2$Grupo,levels = c("tem câncer","não tem câncer"))

cancer2_treino<-cancer2[sorteio,]
cancer2_teste_atributos<-cancer2[-sorteio,2:8]
cancer2_teste_rotulos<-cancer2$Grupo[-sorteio]


arvore<-rpart(Grupo~.,cancer2_treino,method = "class")

arvore_predicoes<-predict(arvore,cancer2_teste,type="class")
confusionMatrix(arvore_predicoes,cancer2_teste_rotulos)

varImp(arvore)















