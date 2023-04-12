# Pacotes
require(VGAM)



#Função 1
prob_ordinal = function(modelo, alphas,betas, x){
  if(is.vector(x)==T) x <- as.matrix(x)
  betas <- as.vector(betas)
  n = dim(x)[1] 
  c = length(alphas) 
  preditor = as.matrix(rep(1,n))%*%t(alphas) + t(as.matrix(rep(1,c))%*%t(x%*%betas))
  exp_pred <- exp(preditor)
  pr_ac = matrix(0, n, (c+1))
  probs = matrix(0, n, (c+1))
  
  if (modelo == "cumulative_logit"){
    pr_ac[,1]= exp_pred[,1]/(1+exp_pred[,1])
    probs[,1] = pr_ac[,1]  
    for(i in 2:c){
      pr_ac[,i]= exp_pred[,i]/(1+exp_pred[,i])
      probs[,i] = pr_ac[,i]-pr_ac[,(i-1)] 
    }
    probs[,(c+1)] = 1 - pr_ac[,c]
    
  }else if (modelo == "cumulative_probit"){
    pr_ac[,1]= pnorm(preditor)[,1]
    probs[,1] = pr_ac[,1]  
    for(i in 2:c){
      probs[,i] = pnorm(preditor)[,i] - probs[,i-1]
    }
    probs[,(c+1)] = 1 - probs[,c]  
    
  }else if (modelo == "cumulative_log_log"){
    pr_ac[,1]= 1 - (exp(-1*exp_pred[,1]))
    probs[,1] = pr_ac[,1]  
    for(i in 2:c){
      pr_ac[,i]=  1 - (exp(-1*exp_pred[,i]))
      probs[,i] = pr_ac[,i]-pr_ac[,(i-1)] 
    }
    probs[,(c+1)] = 1 - pr_ac[,c]
    
  }else if (modelo == "adjacent_logit"){
    multiplicacao = NULL
    multiplicacao[1] = 1
    for(i in 2:(c)){
      multiplicacao[i] = 1 + multiplicacao[i-1] * (exp_pred[i])}
    probs[,1] = 1/(sum(multiplicacao) )
    for(i in 2:c){
      probs[,i] = multiplicacao[i]/sum(multiplicacao) }
    soma = 0
    for(i in 1:(c-1)){
      soma = soma + probs[i] }
    probs[,(c+1)] = 1 - soma
    
  }else if (modelo == "continuation_ratio_logit_1"){
    denominador = (1+exp_pred[c])
    probs[,(c+1)] = 1/denominador 
    i =  (c) 
    numerador = 1
    while (i > 1){
      denominador = denominador *(1+exp_pred[i-1])   
      numerador = numerador * exp_pred[i]
      probs[,i] = numerador/denominador 
      i = i-1
    }
    probs[,1] = 1 - rowSums(probs) 
    
  }else if (modelo == "continuation_ratio_logit_2"){
    denominador = 1
    for(i in 1:(c)){
      denominador = denominador *(1+exp_pred[i]) 
      probs[,i] = (exp_pred[i])/denominador }
    probs[,(c+1)] = 1 - rowSums(probs) 
    
    
  }
  return(probs)
}



#Função 2
banco_de_dados = function(modelo, alphas,betas, x){
  probs = prob_ordinal(modelo = modelo , alphas = alphas, betas = betas, x = x)
  c = length(alphas) 
  y = numeric(n)
  for(i in 1:n){
    y[i] = sample(1:(c+1), size=1, replace = T,prob= probs[i,])
  }
  resposta = matrix(0,n,c+1)
  for(i in 1: n)   resposta[i,y[i]] = 1
  dados <- data.frame(y = resposta, x = x)
  return(dados)
}



#Função 3
seis_colunas = function(modelo, alphas,betas, x){
  y.1 = dados[,1]
  y.2 = dados[,2]
  y.3 = dados[,3]
  x = dados[,4]
  resposta = c(y.1,y.2,y.3)
  
  matriz_estimado = matrix(0, 6, 3)
  
  #cumulative_logit
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x, family=cumulative(reverse=FALSE, parallel = TRUE,link = logitlink), maxit = 100,  data=dados)
  ####print(summary(fit.adj))
  
  matriz_estimado[1,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[1,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[1,3] = coefficients(summary(fit.adj))[3,1]
  
  #cumulative_probit
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x,               family=cumulative(reverse=FALSE, parallel = TRUE,link = probitlink), maxit = 100,  data=dados)
  ####print(summary(fit.adj))
  matriz_estimado[2,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[2,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[2,3] = coefficients(summary(fit.adj))[3,1]
  
  #cumulative_log_log
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x,                   family=cumulative(reverse=FALSE, parallel = TRUE,link = clogloglink), maxit = 100,  data=dados)
  ####print(summary(fit.adj))
  matriz_estimado[3,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[3,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[3,3] = coefficients(summary(fit.adj))[3,1]
  
  #adjacent_logit
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x, family=acat(reverse=TRUE, parallel=TRUE), data=dados)
  ###print(summary(fit.adj))
  matriz_estimado[4,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[4,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[4,3] = coefficients(summary(fit.adj))[3,1]
  
  #continuation_ratio_logit_1
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x, family=cratio(reverse=TRUE, parallel=TRUE), data=dados)
  ###print(summary(fit.adj))
  matriz_estimado[5,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[5,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[5,3] = coefficients(summary(fit.adj))[3,1]
  
  #continuation_ratio_logit_2
  fit.adj <- vglm(cbind(y.1, y.2, y.3) ~ x,family=sratio(reverse=FALSE, parallel=TRUE), data=dados)
  ###print(summary(fit.adj))
  matriz_estimado[6,1] = coefficients(summary(fit.adj))[1,1]
  matriz_estimado[6,2] = coefficients(summary(fit.adj))[2,1]
  matriz_estimado[6,3] = coefficients(summary(fit.adj))[3,1]
  
  return(matriz_estimado)
}



#Função 4
comparacao_de_valores = function(modelo_real,modelo_estimado, alphas_real, beta_real, alphas_est, beta_est, x){
  matriz_real = prob_ordinal(modelo_real,alphas_real, beta_real, x)
  matriz_est = prob_ordinal(modelo_estimado, alphas_est, beta_est, x)
  erro = mean((matriz_real - matriz_est)**2)  
  return(erro)
}
