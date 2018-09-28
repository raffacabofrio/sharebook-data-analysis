library(ggplot2)
library(here)

setwd(here::here())
source("functions/facilitador.densidade.R")



# pra pegar nome dos facilitadores.
livros <- read.csv("donation-lifecycle/doacoes.csv", sep = ";")

# pra pegar dia que pedido foi feito.
dias <- read.csv("donation-lifecycle/pedidos.csv", sep = ";")

# cria e arruma tudo numa única lista de pedidos.
pedidos = merge(livros, dias, by="book_id")
pedidos = data.frame( book_id = pedidos$book_id, title=pedidos$Livro, facilitador=pedidos$FACILITADOR, dia_pedido=pedidos$dia_pedido )


# -----------------------------------------
# densidade GERAL

geral <- data.frame(table(pedidos$dia_pedido)) 
total <- sum(geral$Freq)
geral$percent = round((geral$Freq/total) * 100)


# -----------------------------------------
# densidade por FACILITADOR

antero   <- facilitador.densidade(pedidos, "ANTERO")
carlezzo <- facilitador.densidade(pedidos, "CARLEZZO")
cussa    <- facilitador.densidade(pedidos, "CUSSA")
raffa    <- facilitador.densidade(pedidos, "RAFFA")
walter   <- facilitador.densidade(pedidos, "WALTER")



# -----------------------------------------
# CONSOLIDA TUDO NO GRÁFICO

colors  <- c("#005ef7", "#f70000", "#00f731")
colors2 <- c("#005ef7", "#00f731", "#a400f7", "#f70000")

ggplot() +
  geom_line(data = geral,    aes(x=Var1, y=percent, colour="GERAL",    group=1), size = 1.5) +
  geom_line(data = antero,   aes(x=Var1, y=percent, colour="ANTERO",   group=2), size = 1) +
  # geom_line(data = carlezzo, aes(x=Var1, y=percent, colour="CARLEZZO", group=3), size = 1) +
  scale_color_manual(name = "", values=colors) +
  ylab("PERCENTUAL %") +
  xlab("DIA DOS PEDIDOS")


ggplot() +
  geom_line(data = geral,  aes(x=Var1, y=percent, colour="GERAL",    group=1), size = 1.5) +
  geom_line(data = cussa,  aes(x=Var1, y=percent, colour="CUSSA",   group=2), size = 1) +
  geom_line(data = raffa,  aes(x=Var1, y=percent, colour="RAFFA", group=3), size = 1) +
  geom_line(data = walter, aes(x=Var1, y=percent, colour="WALTER", group=3), size = 1) +
  scale_color_manual(name = "", values=colors2) +
  ylab("PERCENTUAL %") +
  xlab("DIA DOS PEDIDOS")






