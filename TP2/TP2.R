# -- [Exercice 1] --

# - Question 1

vX <- c(420, 380, 350, 400, 440, 380, 450, 420) #volume de vente
vY <- c(5.5, 6, 6.5, 6, 5, 6.5, 4.5, 5) #prix du produit
plot(vX, vY, xlab = "volume de vente", ylab = "prix du produit", main = "Graphique du volume de vente en selon le prix du produit")
# On peut envisager un lien entre la variable X et Y car on observe que plus le prix est élevé, plus les ventes baissent.
# La relation entre les deux variables semble être linéaire, on peut envisager la réalisation une régression linéaire

# - Question 2    

# creatig matrix X
mX <- cbind( rep(1,8), vX)
# creating Beta matrix
Beta0 <- 0
Beta <- 1
vBeta <- (c(Beta0, Beta1))
mBeta <- cbind(vBeta)
# creating Y matrix
mY <- cbind(vY)
View(mY)

# - Question 3

# creating transposed matrix of Y
mY <- rbind(vY)
tmY <- t(mY)
# computing b
b <- solve(t(mX) %*% mX, t(mX) %*% tmY) # solve(A,B) donne Ax = B où x = A^-1 * B
# b représente l'estimateur des MCO (moindres carrés ordinaires) du vecteur Beta inconnu 

# - Question 4

# computing b1
b1 <- cov(vX, vY) / var(vX)
# computing b0
b0 <- mean(vY) - b1 * mean(vX)
round(b1 - b[2], digits = 3)
round(b0 - b[1], digits = 3) 
# on trouve les mêmes valeurs pour les estimations

# mean, cor = coefficient de corrélation 
reg <- lm(vY ~ vX)
coef(reg)

# - Question 5

abline(b0, b1) # ou curve(b0 + b1 * x, add=TRUE)
plot(vX, vY)

# - Question 6

# calcul du coeficient de détermination
# r2 = 1-(sum(y_i-^y_i))2/sum(y_i--y_i)2)
R2 <- cor(vX, vY)^2 # valable que dans le cas s'une régression linéaire cor(x,y)=cov(x,y)/sigx*sigy
R2
# ki=1 (nombre de variable explicative), n=8 (taille de l'ensemble): R2ajust=R2-(k(1-R2))/(n-1-k)
R2ajuste <- R2-(1-R2)/(length(x)-1-1)
R2ajuste
# on trouve un Rajuste de 0.85 ce qui est important
# le modèle est donc bon 

# - Question 7

summary(reg)
anova(reg)


# -- [Exercice 2] --

# - Question 1

y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6) 
x1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
x2 <- c(4, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

# régression linéaire en fonction de l'épaisseur
model1 <- lm(y~x1)
# régression linéaire en fonction de la densité
model2 <- lm(y~x2)

summary(model1)
anova(model1)

# - Question 1/2

summary(model2)
anova(model2)

# - Question 3

model3 <- lm(y~x1+x2)
summary(model3)
anova(model3)

# - Question 4

# Somme des variations : 1420.62 -> somme des variations de x1, x2 et des résiduts
# model1, 980.64/1420.67 = 0.6903
# model2, 643.57/1420.67 = 0.453
# model3, 1204.86/1420.67 = 0.8481

# le model3 semble donc être le meilleur étant donné que le poids de l'erreur résiduelle dans la seomme des variations
# est la mois importante compraré aux autres modèles

# - Question 5

# carré moyen résiduel = residuals de la colonne mean sq obtenu avec anova
# ecart-type des résidus = residual standard error de la commande sumary

# - Question 7

# La p-value de la statistique de Fischer est largement inférieure à 0.05. Donc nous sommes dans la
# zone de rejet de l'hypothèse nulle H0. On refuse donc H0 et on accepte l'hypothèse H1

# - Question 9

# SumSquare(X1, X2) - SumSquare(X1) = 1204.86 - 980.63 = 224.3

# - Question 10

epaisseurX1 <- c(4, 3, 4)
densiteX2 <- c(3.8, 3.4, 2.9)
tab <- data.frame(Epaisseur_X1 = epaisseurX1, Densité_X2 = densiteX2)
predict(model3, newdata = data.frame(Epaisseur_X1 = c(4, 3, 4), Densité_X2 = c(3.8, 3.4, 2.9)))
predict(model3, epaisseurX1 = 4, densitéX2 = 3.8)
predict(model3, epaisseurX1 = 3, densitéX2 = 3.4)
predict(model3, epaisseurX1 = 4, densitéX2 = 2.9)

# truc qui marche
predict(model3, newdata=data.frame(x1=4,x2=3.8))
predict(model3, newdata=data.frame(x1=3,x2=3.4))
predict(model3, newdata=data.frame(x1=4,x2=2.9))
