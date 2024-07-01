##########################################
##### Distribuição Uniforme discreta #####
##########################################

###########
# Exemplo 1: Lançamento de uma dado
###########

n <- 6
x <- seq(1, 6)
px <- rep(1/n, n)

Fx <- cumsum(px)

# Tabela de distribuição de probabilidades

M <- matrix(nrow=n, ncol=3)
M[,1] <- x
M[,2] <- px 
M[,3] <- Fx
colnames(M) <- c("x", "p(x)", "F(x)")

# Gráfico de p(x)

par(mai=c(1,1,0.3,0.3))
plot(x, px, ylim=c(0, 1), type="h", ylab="p(x)")
points(x, px, pch=19)

# Gráfico de F(x)

plot(x, Fx, pch=19, ylim=c(0,1), ylab="F(x)", xlim=c(min(x)-1, max(x)+1))    
points(x[-1], Fx[-n], pch=1)
points(min(x), 0, pch=1)

for(i in 1:n){
  points(x=x[i+0:1] - 0.04, y=Fx[c(i,i)], type="l")  
}
points(x=c(min(x)-1, min(x) - 0.04), y=c(0,0), type="l", lwd=1.5)  
points(x=c(max(x) - 0.04, max(x)+1), y=c(1,1), type="l")  

# Percentil

q <- 0.75
Mq <- M[M[,3] >= q, 1]
x.q <- min(Mq)
x.q

# Valor esperado e variância

  EX <- (n+1)/2
VarX <- round(((n-1)*(n+1))/12, 4)
EX
VarX

###########
# Exemplo 2: 50 alunos na sala
###########

n <- 25
x <- seq(1, n)
px <- rep(1/n, n)

Fx <- cumsum(px)

# Tabela de distribuição de probabilidades

M <- matrix(nrow=n, ncol=3)
M[,1] <- x
M[,2] <- px 
M[,3] <- Fx
colnames(M) <- c("x", "p(x)", "F(x)")

# Gráfico de p(x)

par(mai=c(1,1,0.3,0.3))
plot(x, px, ylim=c(0, 1), type="h", ylab="p(x)")
points(x, px, pch=19)

# Gráfico de F(x)

plot(x, Fx, pch=19, ylim=c(0,1), ylab="F(x)", xlim=c(min(x)-1, max(x)+1))    
points(x[-1], Fx[-n], pch=1)
points(min(x), 0, pch=1)

for(i in 1:n){
  points(x=x[i+0:1] - 0.04, y=Fx[c(i,i)], type="l")  
}
points(x=c(min(x)-1, min(x) - 0.04), y=c(0,0), type="l", lwd=1.5)  
points(x=c(max(x) - 0.04, max(x)+1), y=c(1,1), type="l")  

# Percentil

q <- 0.75
Mq <- M[M[,3] >= q, 1]
x.q <- min(Mq)
x.q

# Valor esperado e variância

EX <- (n+1)/2
VarX <- ((n-1)*(n+1))/12
EX
VarX

##########################################
##### Distribuição de Bernoulli ##########
##########################################

# Exemplo 1: 

n <- 2
x <- c(0, 1)
px <- c(3/4, 1/4)
Fx <- cumsum(px)

# Tabela de distribuição de probabilidades

M <- matrix(nrow=n, ncol=3)
M[,1] <- x
M[,2] <- px 
M[,3] <- Fx
colnames(M) <- c("x", "p(x)", "F(x)")

# Gráfico de p(x)

par(mai=c(1,1,0.3,0.3))
plot(x, px, ylim=c(0, 1), xlim=c(-0.1, 1.1), type="h", ylab="p(x)")
points(x, px, pch=19)

# Gráfico de F(x)

plot(x, Fx, pch=19, ylim=c(0,1), ylab="F(x)", xlim=c(min(x)-1, max(x)+1))    
points(x[-1], Fx[-n], pch=1)
points(min(x), 0, pch=1)

for(i in 1:n){
  points(x=x[i+0:1], y=Fx[c(i,i)], type="l")  
}
points(x=c(min(x)-1, min(x) - 0.02), y=c(0,0), type="l", lwd=1.5)  
points(x=c(max(x), max(x)+1), y=c(1,1), type="l")  

# Percentil

q <- 0.8
Mq <- M[M[,3] >= q, 1]
x.q <- min(Mq)
x.q

# Valor esperado e variância

EX <- px[2]
VarX <- prod(px)
EX
VarX  

##########################################
##### Distribuição Binomial ##############
##########################################

###########
# Exemplo 1
###########

n <- 12
p <- 0.6

# P(X=7|n, p)

dbinom(7, n, p)

# P(x=10|n, p)
dbinom(10, n, p)

# Tabela de distribuição de probabilidades

x <- seq(0, n)
px <- dbinom(x, n, p)
Fx <- cumsum(px)

M <- matrix(nrow=n+1, ncol=3)
M[,1] <- x
M[,2] <- px 
M[,3] <- Fx
colnames(M) <- c("x", "p(x)", "F(x)")

# Gráfico de p(x)

par(mai=c(1,1,0.3,0.3))
plot(x, px, ylim=c(0, 0.25), type="h", ylab="p(x)")
points(x, px, pch=19)

# Gráfico de F(x)

plot(x, Fx, pch=19, ylim=c(0,1), ylab="F(x)", xlim=c(min(x)-1, max(x)+1))    
points(x[-1], Fx[-n], pch=1)
points(min(x), 0, pch=1)

for(i in 1:n){
  points(x=x[i+0:1], y=Fx[c(i,i)], type="l")  
}
points(x=c(min(x)-1, min(x) - 0.02), y=c(0,0), type="l", lwd=1.5)  
points(x=c(max(x), max(x)+1), y=c(1,1), type="l")  

# F(2) = P(Y <= 2)

pbinom(2, n, p)

# Quartis

q <- c(0.25, 0.50, 0.75)
qbinom(q, n, p)

# Valor esperado, variância e desvio-padrão

EX <- n*p
VarX <- n*p*(1-p)
dpX <- sqrt(VarX)
EX
VarX  
dpX

# Valores aleatórios

k <- 100000
xa <- rbinom(k, n, p)

m.xa <- numeric()
v.xa <- numeric()
for(i in 1:10000){
xa <- rbinom(i, n, p)
m.xa[i] <- mean(xa)
v.xa[i] <- var(xa)
}

plot(m.xa, type="l")

plot(v.xa, type="l")