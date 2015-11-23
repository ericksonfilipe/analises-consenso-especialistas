library(reshape)

require(ggplot2) #install.packages("ggplot2") #para gráficos de IC
require(plyr) #install.packages("plyr") #agrupamento, nos gráficos de IC
require(moments) #install.packages("moments") #para o cálculo de Skewness e Kurtosis
require(nortest) #install.packages("nortest") #para os testes Anderson Darling e Shapiro-Wilk
require(lawstat) #install.packages("lawstat") #para o teste de Levene

setwd("/home/erickson/mestrado/analises-consenso-especialistas")

theme_set(theme_bw())

#FUNÇÕES

#função que coloca em um só arquivo os dados sumarizados por abordagem
gera.sumario <- function(variavel.resposta, dados) {
  arquivo = file(paste("Sumário dos dados de",variavel.resposta,".txt"), "w")
  writeLines(paste("","Votação","Vocabulário","Commit","Linha", sep="\t"), arquivo)
  x1=subset(dados, abordagem=="Votação")[,variavel.resposta]
  x2=subset(dados, abordagem=="Vocabulário")[,variavel.resposta]
  x3=subset(dados, abordagem=="Commit")[,variavel.resposta]
  x4=subset(dados, abordagem=="Linha")[,variavel.resposta]
  writeLines(paste("Média",mean(x1),mean(x2),mean(x3),mean(x4),sep="\t"),arquivo)
  writeLines(paste("Mediana",median(x1),median(x2),median(x3),median(x4),sep="\t"),arquivo)
  writeLines(paste("Mínimo",min(x1),min(x2),min(x3),min(x4),sep="\t"),arquivo)
  writeLines(paste("Máximo",max(x1),max(x2),max(x3),max(x4),sep="\t"),arquivo)
  writeLines(paste("1º quartil",quantile(x1,0.25),quantile(x2,0.25),quantile(x3,0.25),quantile(x4,0.25),sep="\t"),arquivo)
  writeLines(paste("3º quartil",quantile(x1,0.75),quantile(x2,0.75),quantile(x3,0.75),quantile(x4,0.75),sep="\t"),arquivo)
  writeLines(paste("Desvio Padrão",sd(x1),sd(x2),sd(x3),sd(x4),sep="\t"),arquivo)
  writeLines(paste("IQR",IQR(x1),IQR(x2),IQR(x3),IQR(x4),sep="\t"),arquivo)
  close(arquivo)
}

#função que retorna lista de cores a ser utilizada em determinada ocasião para boxplots/histogramas, dependendo da quantidade desejada
cores <- function(qtd.cores){
  lista.de.cores=c( #7 cores principais, uma por alggoritmo
    "deeppink1", "deeppink2", "deeppink3", "deeppink4", #verde
    "green1", "green2",  "green3", "green4", #amarelo
    "red1",	"red2",	"red3",	"red4", #vermelho
    "steelblue1", "steelblue2",	"steelblue3", "steelblue" #azul
  )
  if (qtd.cores == 16) cores.a.utilizar = lista.de.cores
  else {
    cores.a.utilizar = c()
    i = 1
    if (qtd.cores == 8) for (indice.cor in c(1,2,5,6,9,10,13,14)) { cores.a.utilizar[i] = lista.de.cores[indice.cor]; i=i+1 }
    else if (qtd.cores == 4) for (indice.cor in c(1,5,9,13)) { cores.a.utilizar[i] = lista.de.cores[indice.cor]; i=i+1 }
    else if (qtd.cores == 5) for (indice.cor in c(2,6,10,14,16)) { cores.a.utilizar[i] = lista.de.cores[indice.cor]; i=i+1 }
  }
  return(cores.a.utilizar)
}

#função que calcula o ylim ideal para padronizar escala de diversos boxplots
calcula.altura.ideal <- function(dados, variavel.resposta) {
  ymin=.Machine$integer.max
  ymax=0
  for (a in levels(dados$abordagem)) {
    amostra=subset(dados, abordagem==a)[,variavel.resposta]
    if (min(summary(amostra)) < ymin) ymin = min(summary(amostra))
    if (max(summary(amostra)) > ymax) ymax = max(summary(amostra))
  }
  return(c(ymin,ymax))
}

#função que organiza melhor os nomes no eixo y dos gráficos
gera.nomes <- function(dados, fator) {
  nomes=c()
  i=1
  for (a in levels(dados$abordagem)) {
    for (p in levels(dados[,fator])) {
      nomes[i]=paste(a,":",p)
      i=i+1
    }
  }
  return(nomes)
}

#função que gera gráficos a partir dos dados residuais
gera.graficos.residuais <- function(variavel.resposta, dados) {
  residuos = lm(dados[,variavel.resposta] ~ abordagem, dados)$residuals
  png(paste("graficos.residuais", variavel.resposta, ".png"), width=500, height=550)
  layout(matrix(c(1,2,3,3,4,5), 3, 2, byrow = TRUE))
  qqnorm(residuos, main=paste("Normal Q-Q Plot dos resíduos de",variavel.resposta), col="saddlebrown"); qqline(residuos, col = "red")
  hist(residuos,main=paste("Histograma dos resíduos de",variavel.resposta), col="beige")
  boxplot(residuos, horizontal=TRUE,main=paste("Box Plot dos resíduos de",variavel.resposta), col="beige")
  lag.plot(residuos,layout=c(1,1),main=paste("Lag Plot dos resíduos de",variavel.resposta), col="saddlebrown")
  plot(dados[,variavel.resposta], residuos, xlab=variavel.resposta, main=paste("Scatter Plot - resíduos x",variavel.resposta), col="saddlebrown")
  dev.off()
}

#função que calcula intervalo de confiança para média
ic_media <- function(x, alpha) {
  x_barra = mean(x) #media amostral
  s = sd(x) #desvio padrão
  n = length(x) #tamanho da amostra
  if (n >= 30) {
    #usa-se normal
    z = qnorm(1-(alpha/2))
    erro = z*s/sqrt(n) 
  } else {
    #usa-se t student, com n-1 graus de liberdade
    t = qt(1-(alpha/2), df = n-1)
    erro = t*s/sqrt(n) 
  }
  c1 = x_barra-erro
  c2 = x_barra+erro
  return(data.frame(c1, c2, y=x_barra))
}

#função que gera gráficos de intervalo de confiança com alpha igual a 5%  	---cores pros 8 algoritmos em IC
gera.grafico.ic <- function(nome_imagem, dados, atributo.a.comparar, atributo.a.analisar){
  png(paste(nome_imagem,".png", sep=""), width=620, height=480) #cortar 10 da esquerda,direita,acima e 5 abaixo
  print(ggplot(ddply(dados, atributo.a.comparar, function(df) ic_media(df[,atributo.a.analisar], alpha=.05)), aes_string(x=atributo.a.comparar)) 
        + geom_errorbar(aes(ymax=c2, ymin=c1), colour=cores(4), size=0.8) #definindo limites
        + geom_point(aes(y=y), colour=cores(4), size=3.5) #definindo ponto central
        + ylab(atributo.a.analisar) #especificando label do eixo y
        #+ ggtitle(nome_imagem)
        + theme_bw()
  )
  dev.off()
}

#função que escreve em arquivo os resultados de Zkurtosis, Zskewness, testes de Anderson Darling e de Shapiro-Wilk
testar_normalidade <- function(data){
  for (variavel.resposta in c("precision","recall","NDCG","fmeasure")) {
    arquivo.normalidade = file(paste("normalidade", variavel.resposta, sep="_"), "w")
    writeLines("Abordagem\tZkurtosis\tZskewness\tSW\tAD", arquivo.normalidade)
    for (a in c("Votação", "Linha", "Commit", "Vocabulário")) {
      dados.abordagem = subset(data, abordagem==a)[,variavel.resposta]
      k = kurtosis(dados.abordagem)/sqrt(24/length(dados.abordagem))
      s = skewness(dados.abordagem)/sqrt(6/length(dados.abordagem))
      print(mean(dados.abordagem))
      writeLines(paste(a, k, s, shapiro.test(dados.abordagem)$p.value, ad.test(dados.abordagem)$p.value, sep = "\t"), arquivo.normalidade)
    }
    close(arquivo.normalidade)
  }
}

#função que escreve em arquivo os resultados dos teste de Bartlett e de Levene
testar_homoscedasticidade = function(base, variavel.resposta, data, group, arquivo){
  writeLines(paste(base, variavel.resposta, bartlett.test(data, group)$p.value, bartlett.test(data, group)$statistic, levene.test(data, group, location="mean")$p.value, levene.test(data, group, location="mean")$statistic, sep = "\t"), arquivo)
}


#ANÁLISE

#carregando dados e deixando claro o que deve ser tratado como fator
dados = read.csv("acuracia_por_classe.csv",header=F,sep="|")
colnames(dados) = c("abordagem", "entidade", "precision", "recall", "fmeasure", "NDCG")

dados$abordagem = as.factor(dados$abordagem)							          # 4 abordagems

testar_normalidade(dados)

#gerando gráficos - comparando métricas por abordagem
gera.grafico.ic("ICs precision", dados, "abordagem", "precision")
gera.grafico.ic("ICs recall", dados, "abordagem", "recall")
gera.grafico.ic("ICs fmeasure", dados, "abordagem", "fmeasure")
gera.grafico.ic("ICs NDCG", dados, "abordagem", "NDCG")

#visão geral dos dados
png("Pair plot.png", width=1600, height=1600)
#plot(dados, col="saddlebrown")
plot(dados, col="saddlebrown")
dev.off()

#gerando boxplots e histogramas para as variáveis
for (variavel.resposta in c("precision","recall","NDCG","fmeasure")) {
  #gera sumário de dados
  gera.sumario(variavel.resposta, dados)
  
  #gera Boxplots
  png(paste("boxplots_", variavel.resposta, ".png", sep=""), width=620, height=480) #800,600
  par(mfrow=c(1,1),mar=c(2.5,8,0.5,1), oma=c(0,0,0,0))
  boxplot(dados[,variavel.resposta] ~ dados$abordagem, las=1, col=cores(4))
  #title(paste("Boxplots  - ",variavel.resposta), cex.main = 1.3, font.main = 2, outer=TRUE)
  dev.off()
}


#anova
arquivo.anova = file("anova_20-11-2015.txt", "w")
for (variavel.resposta in c("precision","recall","fmeasure", "NDCG")) {
  writeLines(paste("\n",variavel.resposta, sep = ""), arquivo.anova)
  fit = lm(dados[,variavel.resposta] ~ abordagem, dados) 
  a = anova(fit)

  writeLines(paste("Df", "Sum Sq", "F value", "F table", sep = "\t"), arquivo.anova)
  writeLines(paste(a[1,"Df"], a[1,"Sum Sq"], a[1,"F value"], qf(.95, a[1,"Df"], a[2,"Df"]), sep = "\t"), arquivo.anova)
}
close(arquivo.anova)


#Usando teste não paramétrico
for (variavel.resposta in c("precision","recall","fmeasure", "NDCG")) { 
  print(kruskal.test(dados[,variavel.resposta] ~ abordagem, dados))
}


#testes par a par com base nos resultados de normalidade e homoscedasticidade... (Mann–Whitney para não normais)
with(dados, pairwise.t.test(precision, abordagem, paired=F, var.equal=T)) #Normal e Homoscedástico
with(dados, pairwise.t.test(precision, abordagem, paired=F, var.equal=F)) #Normal e Heteroscedástico
# with(dados, pairwise.wilcox.test(tempo.de.resposta, abordagem, paired=F, var.equal=T)) #Não Normal e Homoscedástico
# with(dados, pairwise.wilcox.test(tempo.de.resposta, abordagem, paired=F, var.equal=F)) #Não Normal e Heteroscedástico

with(dados, pairwise.t.test(NDCG, abordagem, paired=F, var.equal=T)) #Normal e Homoscedástico
with(dados, pairwise.t.test(recall, abordagem, paired=F, var.equal=F)) #Normal e Heteroscedástico

# png("ndcg.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = NDCG, fill = abordagem)) + 
#   geom_bar(position = "dodge", stat = "identity") + coord_flip()
# dev.off()
# 
# png("ndcg_abordagem.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = NDCG, fill = abordagem)) +
#   geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
# dev.off()
# 
# png("precision.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = precision, fill = abordagem)) + 
#   geom_bar(position = "dodge", stat = "identity") + coord_flip()
# dev.off()
# 
# png("precision_abordagem.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = precision, fill = abordagem)) +
#   geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
# dev.off()
# 
# png("recall.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = recall, fill = abordagem)) + 
#   geom_bar(position = "dodge", stat = "identity") + coord_flip()
# dev.off()
# 
# png("recall_abordagem.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = recall, fill = abordagem)) +
#   geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
# dev.off()
# 
# png("f-measure.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = fmeasure, fill = abordagem)) + 
#   geom_bar(position = "dodge", stat = "identity") + coord_flip()
# dev.off()
# 
# png("f-measure_abordagem.png", width = 800, height = 800)
# ggplot(dados, aes(x = entidade, y = fmeasure, fill = abordagem)) +
#   geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
# dev.off()
# 
# dlply(dados, .(abordagem), function(df) summary(df))
# 
# data_melt <- melt(dados, id.vars=c("entidade", "abordagem"), measure.vars=c("precision", "recall", "fmeasure", "NDCG"), variable.name="metric")
# 
# png("metricas_por_abordagem.png", width = 1024, height = 768)
# ggplot(data_melt, aes(x=entidade, y=value, fill=variable)) +
#   geom_bar(position = "dodge", stat = "identity") + coord_flip() + facet_wrap(~abordagem, nrow=1)  
# dev.off()
# 
# png("metricas_por_entidade.png", width = 1024, height = 768)
# ggplot(data_melt, aes(x=variable, y=value, fill=abordagem)) +
#   geom_bar(position = "dodge", stat = "identity") + coord_flip()+ facet_wrap(~entidade, nrow=4, ncol=7)  
# dev.off()
# 
# png("metricas_boxplot.png", width = 1024, height = 768)
# ggplot(data_melt, aes(x=variable, y=value, fill=abordagem)) +
#   geom_boxplot() + coord_flip()
# dev.off()


