n = 100
modelos = c('cumulative_logit','cumulative_probit','cumulative_log_log', 'adjacent_logit','continuation_ratio_logit_1','continuation_ratio_logit_2')

alphas = c(0.2, 0.6) # 5,6
#alphas = c(0.3, 0.9) # 3,4
#alphas = c(0.4, 0.8) #1,2
#betas = 0.6


betas = 0.3
x <- rnorm(n)

#modelo_principal = "cumulative_logit"
#modelo_principal = "cumulative_probit"
modelo_principal = "cumulative_log_log"
#modelo_principal = "adjacent_logit"
#modelo_principal= "continuation_ratio_logit_1"
#modelo_principal = "continuation_ratio_logit_2"
#cumulative_logit
#cumulative_log_log
#adjacent_logit
#continuation_ratio_logit_1

resultados = matrix(0, n, 6)
j = 1

while (j < 100+1) {
  dados = banco_de_dados(modelo = modelo_principal, alphas, betas, x)
  matriz_estimado = seis_colunas(modelo_principal, alphas,betas, x)  
  #print(matriz_estimado)
  for (i in 1:6){
    resultados[j,i] = comparacao_de_valores(modelo_principal, modelos[i], alphas, betas, c(matriz_estimado[i,1:2]),matriz_estimado[i,3], x)
  }
  j = j+1
}

resultados

#help(boxplot)
#boxplot(resultados)
#apply(resultados, 2,mean)
#apply(resultados, 2,sd)
#boxplot(resultados[,c(1,3,6)])

#write.table(resultados, file = "resultados_logit_6.csv", sep = "\t", na = "", quote = FALSE)
#write.table(resultados, file = "resultados_probit_6.csv", sep = "\t", na = "", quote = FALSE)
#write.table(resultados, file = "resultados_log_6.csv", sep = "\t", na = "", quote = FALSE)
#write.table(resultados, file = "resultados_adjacent_6.csv", sep = "\t", na = "", quote = FALSE)
#write.table(resultados, file = "resultados_ratio1_6.csv", sep = "\t", na = "", quote = FALSE)
#write.table(resultados, file = "resultados_ratio2_6.csv", sep = "\t", na = "", quote = FALSE)

#colnames(df) <- c('Logito','Probito','Log-log','Adjacente','Razão I','Razão II')
