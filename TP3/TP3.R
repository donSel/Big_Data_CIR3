# DS
# proporiété cov var et esperence lien entre tout ça, prédire beta chapeau, utiliser le bon modèle selon 
# que le modèle soit discret ou non, quelques commandes en R

# -- [Exercice 1] --

# - Question 1

x1 <- anesthesie$X1
y <- anesthesie$Y
plot(x1, y)

model = glm(y~x1, family=binomial)
summary(model)



















