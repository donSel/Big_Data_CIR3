2+4
36/5
var <- 3 * sqrt(4) + pi
print(var)
mystere <- var/0
print(mystere)

help(source)
getwd()
setwd("C:/Users/micka/Documents/ISEN/CIR_3/Big_Data")
getwd()
source("file.R")

tmp <- 7 / 2
resultat <- floor(tmp)
reste <- 7 - 2*resultat
sprintf("resultat value : %d", resultat)
sprintf("reste value : %d", reste)

# help(floor)
# tmp2<-9.9
# plancher<-floor(tmp2)
# print(plancher)
# plafond<-ceiling(tmp2)
# print(plafond)

resultat <- 7 %/% 2
reste <- 7 %% 2
sprintf("resultat value : %.2f", resultat)
sprintf("reste value : %.2f", reste)
abs(-3)
floor(-3.7)  
sqrt(floor(3 * pi))

x <- 10
y <- 20
x > y
x <- 3.14
y <- pi
logiq <- x == y
print(logiq)

x <- T
y <- F
v1 <- x && y
v2 <- x || y
v3 <- v2 && !v1
print(v3)

x <- T
y <- F
z <- F
v1 <- y && z || x
v2 <- y && (z || x)
print(v1)
print(v2)
v3 <- v1 && v2
print(v3)

txt <- "42"
nbr <- as.integer(txt)
is.numeric(nbr)

mot <- "petite"
text1 <- paste("une", mot, "phrase")
text2 <- paste(text1, "compte", nchar(text1), "lettres")
print(text1)
print(text2)

tmp <- 3 / 0
nsp <- NA
resultat <- paste(tmp, tmp+1, tmp+nsp)

vecteur1 <- c(1, 3, 5, 7, 9)
vecteur2 <- seq(from=0, to=10, by=2)
vecteur3 <- 0:10
vecteur4 <- rep(1:2, 5)
print(vecteur3)
vecteur1[1]
vecteur1[0]
summary(vecteur1)

vecteur <- rnorm(10)
for (i in 1:length(vecteur))
print(vecteur[i])

v1 <- runif(10)
v2 <- rpois(10, 1)
v3 <- rnorm(10)
matrice <- rbind(v1, v2, v3)
dim(matrice)
matricet <- t(matrice)
dim(matricet)

v1 <- c(175, 182, 165, 187, 158)
v2 <- c(19, 18, 21, 22, 20)
tableau <- data.frame(taille=v1,age=v2)
names(tableau)
print(tableau$taille)
summary(tableau)
write.table(tableau, "sortie.csv", sep=";")

install.packages("maps")
library(maps)
map("france")

datas <- rnorm(20)
barplot(datas)
hist(datas, nclass=4)

plot(seq(0,2*pi,by=0.01), sin(seq(0,2*pi,by=0.01)),type="l")

# EXERCICE 1

#1
data("iris") # ou data(iris)
class(iris)
var(iris$Petal.width)

view(iris)
mean(iris$Sepal.Length[iris$Species=="setosa"])

#2
view(iris)

#3
mean(iris$Sepal.Length)

#4
SL <- (iris$Sepal.Length - mean(iris$Sepal.Length)) / sd(iris$Sepal.Length)
SW <- (iris$Sepal.Width - mean(iris$Sepal.Width)) / sd(iris$Sepal.Width)

#5
plot(SL,SW, col=iris$Species)

# EXERCICE 2

ms=numeric(10000);
p=0.75; x=seq(-4,4,0.025);
for (j in(1:50)){
k=j*j; for (i in (1:10000)){
sig=sqrt(p*(1-p)/k); mu=p; ms[i]=(mean(rbinom(k,1,p))-mu)/sig }
hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("normal curve over
histogram, n = %d",k))
curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")}

# Loi exponentielle
lambda=1
ms=numeric(10000);
p=0.75; x=seq(-4,4,0.025);
for (j in(1:50)){
   k=j*j; for (i in (1:10000)){
       sig=1/(sqrt(k)*lambda); mu=1/lambda; ms[i]=(mean(rexp(k,lambda))-mu)/sig }
   hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("normal curve over
histogram, n = %d",k))
curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")}

# Loi de Cauchy impossible : son espérence/variance est infinie