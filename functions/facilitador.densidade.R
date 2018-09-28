# -----------------------------------------
# Densidade de probabilidade de pedidos por facilitador.

facilitador.densidade <- function(pedidos, facilitador){
  
  pedidos.filtered <- pedidos[pedidos$facilitador==facilitador,]
  pedidos.densidade <- data.frame(table(pedidos.filtered$dia_pedido)) 
  total <- sum(pedidos.densidade$Freq)
  pedidos.densidade$percent = round((pedidos.densidade$Freq/total) * 100)
  
  return(pedidos.densidade)
  
}