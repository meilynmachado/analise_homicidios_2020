'''
Universidade Federal do Amazonas
Bacharelado em Estatítica

Disciplina: Modelos Lineares Generalizados
Professora: Themis Leão Abensur
Aluna: Meilun Barbosa - 22052393

'''


# Carregando Pacotes ------------------------------------------------------



# Leitura de dados --------------------------------------------------------
y1x1 <- cbind(rep(c("Branca"), 118),rep(c("0-14"),118))
y1x2 <- cbind(rep(c("Branca"), 4398),rep(c("15-29"),4398))
y1x3 <- cbind(rep(c("Branca"), 4984),rep(c("30-59"),4984))
y1x4 <- cbind(rep(c("Branca"), 790),rep(c("60+"),790))

y2x1 <- cbind(rep(c("Preta"), 38),rep(c("0-14"),38))
y2x2 <- cbind(rep(c("Preta"), 2304),rep(c("15-29"),2304))
y2x3 <- cbind(rep(c("Preta"), 1568),rep(c("30-59"),1568))
y2x4 <- cbind(rep(c("Preta"), 117),rep(c("60+"),117))

y3x1 <- cbind(rep(c("Parda"), 409),rep(c("0-14"),409))
y3x2 <- cbind(rep(c("Parda"), 18590),rep(c("15-29"),18590))
y3x3 <- cbind(rep(c("Parda"), 13711),rep(c("30-59"),13711))
y3x4 <- cbind(rep(c("Parda"), 1076),rep(c("60+"),1076))

y4x1 <- cbind(rep(c("Preta ou Parda"), 447),rep(c("0-14"),447))
y4x2 <- cbind(rep(c("Preta ou Parda"), 20894),rep(c("15-29"),20894))
y4x3 <- cbind(rep(c("Preta ou Parda"), 15279),rep(c("30-59"),15279))
y4x4 <- cbind(rep(c("Preta ou Parda"), 1193),rep(c("60+"),1193))


homicidios <- data.frame(rbind(y1x1,y1x2,y1x3,y1x4,y2x1,y2x2,y2x3,y2x4,y3x1,y3x2,
                               y3x3,y3x4,y4x1,y4x2,y4x3,y4x4))
names(homicidios)[1] <- "cor_raca"
names(homicidios)[2] <- "faixa_etaria"
