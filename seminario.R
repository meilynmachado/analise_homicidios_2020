'''
Universidade Federal do Amazonas
Bacharelado em Estatítica

Disciplina: Modelos Lineares Generalizados
Professora: Themis Leão Abensur
Aluna: Meilyn Barbosa - 22052393

'''
setwd("C:/Users/HP/OneDrive/Documentos/Estudos/MLG/Seminario")

# Carregando Pacotes ------------------------------------------------------
library(tidyverse)
library(MASS)
if(!require("gghighlight"))install.packages("gghighlight")

# Leitura de dados --------------------------------------------------------

# y1x1 <- cbind(rep(c("Branca"), 118),rep(c("0-14"),118))
# y1x2 <- cbind(rep(c("Branca"), 4398),rep(c("15-29"),4398))
# y1x3 <- cbind(rep(c("Branca"), 4984),rep(c("30-59"),4984))
# y1x4 <- cbind(rep(c("Branca"), 790),rep(c("60+"),790))
# 
# y2x1 <- cbind(rep(c("Preta"), 38),rep(c("0-14"),38))
# y2x2 <- cbind(rep(c("Preta"), 2304),rep(c("15-29"),2304))
# y2x3 <- cbind(rep(c("Preta"), 1568),rep(c("30-59"),1568))
# y2x4 <- cbind(rep(c("Preta"), 117),rep(c("60+"),117))
# 
# y3x1 <- cbind(rep(c("Parda"), 409),rep(c("0-14"),409))
# y3x2 <- cbind(rep(c("Parda"), 18590),rep(c("15-29"),18590))
# y3x3 <- cbind(rep(c("Parda"), 13711),rep(c("30-59"),13711))
# y3x4 <- cbind(rep(c("Parda"), 1076),rep(c("60+"),1076))

#homicidios <- data.frame(rbind(y1x1,y1x2,y1x3,y1x4,y2x1,y2x2,y2x3,y2x4,y3x1,y3x2,
#                               y3x3,y3x4))
# names(homicidios)[1] <- "cor_raca"
# names(homicidios)[2] <- "faixa_etaria"
# 
# write.csv2(homicidios,"homicidios_2020.csv")

homicidios <- read.csv2("homicidios_2020.csv",header = T, stringsAsFactors = T)

dados <- read.csv2("C:/Users/HP/OneDrive/Documentos/Estudos/MLG/Seminario/homicidios.csv", header = T)

dados$n_homicidios <- as.numeric(dados$n_homicidios)
dados$faixa_etaria <- factor(dados$faixa_etaria) # Referencia: 0-14
dados$cor_raca <- factor(dados$cor_raca) # Referencia: Branca


# Analise Descritiva ------------------------------------------------------

palette <- c("#6EE66C", "#7B2661", "#839BFD", "#3B570A")

# Frequencia de homicídios por raça
homicidios %>% ggplot()+geom_bar(aes(x = cor_raca, fill = cor_raca),alpha = 0.7)+
  theme(text = element_text(size = 15))+labs(x = "Raça", y = "Frequência", fill = "Raça")+
  scale_fill_manual(values = palette)

# Frequencia de homicidios por faixa etária
homicidios %>% ggplot()+geom_bar(aes(x = faixa_etaria, fill = faixa_etaria),alpha = 0.7)+
  theme(text = element_text(size = 15))+labs(x = "Faixa Etária", y = "Frequência",fill = "Faixa Etária")+
  scale_fill_manual(values = palette)

dados %>% ggplot()+geom_boxplot(aes(y = n_homicidios, x = faixa_etaria, fill = faixa_etaria),alpha = 0.7)+
  theme(text = element_text(size = 15))+
  labs(y = "Número de Homicídios", x = "Faixa Etária", fill = "Faixa Etária")+
  scale_fill_manual(values = palette)

dados %>% ggplot()+geom_boxplot(aes(y = n_homicidios, x = cor_raca, fill = cor_raca),alpha = 0.7)+
  theme(text = element_text(size = 15))+
  labs(y = "Número de Homicídios", x = "Raça", fill = "Raça")+
  scale_fill_manual(values = palette)

summary(dados$n_homicidios)

# Ajuste do Modelo 1 --------------------------------------------------------

fit1 <- glm(n_homicidios~faixa_etaria+cor_raca, data = dados, family = poisson(link = "log"))

summary(fit1)

# Verificando se há superdispersão
X1 <- model.matrix(fit1)
n <- nrow(X1)
p <- ncol(X1)
phi1 <- sum(residuals(fit1, type="pearson")^2)/(n-p); phi1

## Analise de diagnostico -------------------------------------------------
eta <- fit1$fit
w1 <- fit1$weights
W1 <- diag(w1)
W11 <- sqrt(W1)
H1 <- W11%*%X1%*%solve(t(X1)%*%W1%*%X1)%*%t(X1)%*%W11
h1 <- diag(H1)
rd1 <- resid(fit1, type="deviance")
td1 <- rd1*sqrt(phi1/(1-h1))
rp1 <- resid(fit1, type="pearson")
rp1 <- sqrt(phi1)*rp1
ts1 <- rp1/sqrt(1-h1)
ma1 <- max(td1)
mi1 <- min(td1)
mas1 <- max(ts1)
mis1 <- min(ts1)
LD1 <- h1*(ts1^2)/(1-h1)


## graficos de diagnostico 1 -----------------------------------------------

plot(h1, xlab = "indices", ylab = "alavancagem", ylim = c(0,1), pch=16)
abline(2*p/n, 0, lty=2)
#identify(h1)
dev.off()

plot(td1, xlab = "indices", ylab="residuos do desvio", ylim = c(mi1-1, ma1+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(td1)
dev.off()

plot(LD1, xlab = "indices", ylab = "afastamento da verossimilhanca", pch=16)
identify(LD1)
dev.off()


## graficos de diagnostico 2 -----------------------------------------------

shapiro.test(td1) # há evidencia de normalidade, p-valor: 0,81

## funcao de autocorrelacao e densidade dos residuos do desvio
par(mfrow = c(1,2))
acf(td1, ylab = "Correlação residual", xlab = "Defasagem", main = "")
plot(density(td1), xlab = "Resíduos do desvio", ylab = "Densidade", main = "", col = "cadetblue", lwd = 2)
dev.off()

# a distribuição dos resíduos apresenta bimodalidade 

# função envelope do MLG Poisson ----------------------------------
envelope.poisson <- function(form=form,Fam=Fam,k=k,alfa=alfa){
  alfa1 <- ceiling(k*alfa)
  alfa2 <- ceiling(k*(1-alfa))
  glm1 <- glm(formula=form,family=Fam,maxit=50)
  X <- model.matrix(glm1)
  w <- glm1$weights
  W <- diag(w)
  H <- sqrt(W)%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%sqrt(W)
  h <- diag(H)
  n <- nrow(X)
  rd <- resid(glm1,type="deviance")  
  phi <- 1
  td <- rd*sqrt(phi/(1-h))
  re <- matrix(0,n,k)
  for(i in 1:k){
    nresp <- rpois(n,fitted(glm1))
    fit <- glm(nresp~-1+X,family=Fam,maxit=50)
    w <- fit$weights
    W <- diag(w)
    H <- sqrt(W)%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%sqrt(W)
    h <- diag(H)
    re[,i] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))
  }
  e1 <- numeric(n)
  e2 <- numeric(n)
  for(i in 1:n){
    eo <- sort(re[i,])
    e1[i] <- eo[alfa1]
    e2[i] <- eo[alfa2]
  }
  xb <- apply(re,1,mean)
  faixa <- range(td,e1,e2)
  par(pty="s")
  qqnorm(e1,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1)
  par(new=TRUE)
  qqnorm(e2,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1)
  par(new=TRUE)
  qqnorm(xb,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=2)
  par(new=TRUE)
  qqnorm(td,xlab="Percentis da N(0,1)",ylab="Resíduos do desvio", main = "", ylim=faixa)
}
# 
attach(dados)
form <- n_homicidios ~ faixa_etaria + cor_raca
Fam <- poisson(link = "log")
envelope.poisson(form = form, Fam = Fam, k = 100, alfa = 0.05)
dev.off()


# Ajuste do modelo 2 ------------------------------------------------------

fit <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados) # modelo saturado
summary(fit2) # sob h1

fit0 <- glm.nb(n_homicidios~cor_raca, data = dados)
summary(fit0) # sob h0



# Análise de Diagnóstico do Modelo 2 --------------------------------------

# Razão de verossimilhanças
D2 <- deviance(fit2); D2
D0 <- deviance(fit0); D0
RV <- (D0 - D2); RV
df <- df.residual(fit0) - df.residual(fit2); df
pvRV <- pchisq(RV, df, lower.tail = FALSE); pvRV # valor-p maior que 0.05

# apontando a escolha do modelo sob H0, porém ficarei com o modelo saturado

# interpretação da taxa média 

beta <- coef(fit2); beta # coeficientes estimados do MLG
taxa <- exp(beta); taxa     # taxas estimadas

# Medidas de Diagnóstico 

X2 <- model.matrix(fit2)
n <- nrow(X2)
p <- ncol(X2)
eta2 <- fit2$fit
w2 <- fit2$weights
W2 <- diag(w2)
W22 <- sqrt(W2)
H2 <- W22%*%X2%*%solve(t(X2)%*%W2%*%X2)%*%t(X2)%*%W22
h2 <- diag(H2)
rd2 <- resid(fit2, type="deviance")
phi2 <- fit2$theta
td2 <- rd2*sqrt(phi2/(1-h2))
rp2 <- sqrt(phi2)*resid(fit2, type="pearson")
ts2 <- rp2/sqrt(1-h2)
ma2 <- max(td2)
mi2 <- min(td2)
mas2 <- max(ts2)

mis2 <- min(ts2)
LD2 <- h2*(ts2^2)/(1-h2)

# Graficos de Diagnóstico 1 
# Investigacao de outliers  
indice <-  1:dim(dados)[1]
df_diag <- data.frame(indice, h2, td2, LD2)

# Grafico para detectar pontos de alavanca
ggplot(df_diag, aes(x = indice, y = h2))+
  theme(text = element_text(size = 15))+
  geom_point(col = palette[3], size = 2.5)+
  scale_y_continuous(limits = c(0,1))+ 
  geom_hline(yintercept = 2*p/n, col = palette[4], lty = 2)+
  gghighlight(h2>(2*p/n),label_key = indice,unhighlighted_colour=palette[3])+
  labs(x = "Indice das observações", y = "Alavancagem")


# Grafico para verificar variancia constante (outliers)

ggplot(df_diag, aes(x = indice, y = td2))+
  theme(text = element_text(size = 15))+
  geom_point(col = palette[3], size = 2.5)+
  geom_hline(yintercept = 2, col = palette[4], lty = 2)+
  geom_hline(yintercept = -2, col = palette[4], lty = 2)+
  gghighlight(td2>2,label_key = indice,unhighlighted_colour=palette[3])+
  gghighlight(td2< -2,label_key = indice,unhighlighted_colour=palette[3])+
  labs(x = "Indice das observações", y = "Resíduos do Desvio")

which(td2 > 2 | td2 < -2)
dados[c(1,2,4,6,8,9,10,12),]

# Grafico para identificar afastamento da verossimilhanca (pontos influentes)
plot(LD2, xlab = "Índices", ylab = "Afastamento da verossimilhanca", pch = 19, col = palette[4])
identify(LD2)
dev.off()

ggplot(df_diag, aes(x = indice, y = LD2))+
  theme(text = element_text(size = 15))+
  geom_point(col = palette[3], size = 2.5)+
  geom_hline(yintercept = 100, col = palette[4], lty = 2)+
  gghighlight(LD2>100,label_key = indice,unhighlighted_colour=palette[3])+
  labs(x = "Indice das observações", y = "Afastamento da Verossimilhança")

# Graficos de Diagnóstico 2 
# Verificação do Teorema de Gauss Markov 

### Teste de normalidade dos residuos
shapiro.test(td2) # evidência de normalidade dos resíduos

## funcao de autocorrelacao e densidade dos residuos do desvio
par(mfrow = c(1,2))
acf(td2, ylab = "Correlação residual", xlab = "Defasagem", main = "")
plot(density(td2), xlab = "Resíduos do desvio", ylab = "Densidade", main = "", lwd = 2, col = palette[2])
dev.off()

# função envelope do Modelo Binomial Negativo ---------------------------------
fit.model <- fit2
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
fi <- fit.model$theta
e <- matrix(0,n,100)
#
for(i in 1:100){
  resp <- rnegbin(n, fitted(fit.model),fi)
  fit <- glm.nb(resp ~ X)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentis da N(0,1)", ylab="Resíduos do desvio", main = "", ylim=faixa, pch=16, col = palette[3])
par(new=T)
#
qqnorm(e1,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1, lwd = 1.5, col = palette[2])
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1, lwd = 1.5, col = palette[2])
par(new=T)
qqnorm(med,axes=F,xlab="", ylab="", main = "", type="l",ylim=faixa,lty=2, col = palette[2])
#------------------------------------------------------------------------
dev.off()

# Ajuste do modelo BN sem os outliers 1 e 14 

fit1 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(1))
fit2 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(2))
fit4 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(4))
fit6 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(6))
fit8 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(8))
fit9 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(9))
fit10 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(10))
fit12 <- glm.nb(n_homicidios~faixa_etaria+cor_raca, data = dados, subset=-c(12))
#########################
# Analise Confirmatoria #
#########################

### Variacao percentual
VP1 <- 100*abs((coef(fit1)-coef(fit))/coef(fit)); VP1
VP2 <- 100*abs((coef(fit2)-coef(fit))/coef(fit)); VP2
VP4 <- 100*abs((coef(fit4)-coef(fit))/coef(fit)); VP4
VP6 <- 100*abs((coef(fit6)-coef(fit))/coef(fit)); VP6
VP8 <- 100*abs((coef(fit8)-coef(fit))/coef(fit)); VP8
VP9 <- 100*abs((coef(fit9)-coef(fit))/coef(fit)); VP9
VP10 <- 100*abs((coef(fit10)-coef(fit))/coef(fit)); VP10
VP12 <- 100*abs((coef(fit12)-coef(fit))/coef(fit)); VP12

# Ajuste do modelo 3 ------------------------------------------------------

fit3 <- glm.nb(n_homicidios~faixa_etaria, data = dados, link = "log")

summary(fit3)

# Análise de Diagnóstico do Modelo 2 --------------------------------------

# Razão de verossimilhanças
D3 <- deviance(fit3); D3
D0 <- deviance(fit0); D0
RV <- (D0 - D3); RV
df <- df.residual(fit0) - df.residual(fit3); df
pvRV <- pchisq(RV, df, lower.tail = FALSE); pvRV # valor-p maior que 0.05

# apontando a escolha do modelo sob H0, porém ficarei com o modelo saturado fit3

# interpretação da taxa média 

beta <- coef(fit3); beta # coeficientes estimados do MLG
taxa <- exp(beta); taxa     # taxas estimadas

# Medidas de Diagnóstico 

X3 <- model.matrix(fit3)
n <- nrow(X3)
p <- ncol(X3)
eta3 <- fit3$fit
w3 <- fit3$weights
W3 <- diag(w3)
W32 <- sqrt(W3)
H3 <- W32%*%X3%*%solve(t(X3)%*%W3%*%X3)%*%t(X3)%*%W32
h3 <- diag(H3)
rd3 <- resid(fit3, type="deviance")
phi3 <- fit3$theta
td3 <- rd3*sqrt(phi3/(1-h3))
rp3 <- sqrt(phi3)*resid(fit3, type="pearson")
ts3 <- rp3/sqrt(1-h3)
ma3 <- max(td3)
mi3 <- min(td3)
mas3 <- max(ts3)
mis3 <- min(ts3)
LD3 <- h3*(ts3^2)/(1-h3)

# Graficos de Diagnóstico 1 
# Investigacoo de outliers  

plot(h3, xlab = "Índices", ylab = "Alavancagem", ylim = c(0,1), pch = 16, col = palette[1])
abline(2*p/n,0,lty=2, col = palette[2])
identify(h3)
dev.off()

plot(td3, xlab = "Índices", ylab = "Residuos do desvio", ylim = c(mis3-1, mas3+1), col = palette[3], pch = 19)
abline(2, 0, lty=2, col = palette[4])

abline(-2, 0, lty=2, col = palette[4])
identify(td3)
dev.off()

plot(LD3, xlab = "Índices", ylab = "Afastamento da verossimilhanca", pch = 19, col = palette[4])
identify(LD3)
dev.off()

# Graficos de Diagnóstico 2 
# Verificação do Teorema de Gauss Markov 

### Teste de normalidade dos residuos
shapiro.test(td3) # evidência de normalidade dos resíduos

## funcao de autocorrelacao e densidade dos residuos do desvio
par(mfrow = c(1,2))
acf(td3, ylab = "Correlacao residual", xlab = "Defasagem", main = "")
plot(density(td3), xlab = "Resíduos do desvio", ylab = "Densidade", main = "", lwd = 2, col = palette[2])
dev.off()

# função envelope do Modelo Binomial Negativo ---------------------------------
fit.model <- fit3
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
fi <- fit.model$theta
e <- matrix(0,n,100)
#
for(i in 1:100){
  resp <- rnegbin(n, fitted(fit.model),fi)
  fit <- glm.nb(resp ~ X)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentis da N(0,1)", ylab="Resíduos do desvio", main = "", ylim=faixa, pch=16, col = palette[3])
par(new=T)
#
qqnorm(e1,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1, lwd = 1.5, col = palette[2])
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", main = "", type="l",ylim=faixa,lty=1, lwd = 1.5, col = palette[2])
par(new=T)
qqnorm(med,axes=F,xlab="", ylab="", main = "", type="l",ylim=faixa,lty=2, col = palette[2])
