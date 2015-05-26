require(ggplot2)
library(plyr)
library(reshape)

setwd("/home/erickson/mestrado/analises-consenso-especialistas")

theme_set(theme_bw())

dados = read.csv("acuracia_por_classe.csv",header=F,sep="|")
colnames(dados) = c("abordagem", "entidade", "precision", "recall", "fmeasure", "NDCG")

png("ndcg.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = NDCG, fill = abordagem)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip()
dev.off()

png("ndcg_abordagem.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = NDCG, fill = abordagem)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
dev.off()

png("precision.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = precision, fill = abordagem)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip()
dev.off()

png("precision_abordagem.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = precision, fill = abordagem)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
dev.off()

png("recall.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = recall, fill = abordagem)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip()
dev.off()

png("recall_abordagem.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = recall, fill = abordagem)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
dev.off()

png("f-measure.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = fmeasure, fill = abordagem)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip()
dev.off()

png("f-measure_abordagem.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = fmeasure, fill = abordagem)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
dev.off()

dlply(dados, .(abordagem), function(df) summary(df))

data_melt <- melt(dados, id.vars=c("entidade", "abordagem"), measure.vars=c("precision", "recall", "fmeasure", "NDCG"), variable.name="metric")

png("metricas_por_abordagem.png", width = 1024, height = 768)
ggplot(data_melt, aes(x=entidade, y=value, fill=variable)) +
  geom_bar(position = "dodge", stat = "identity") + coord_flip() + facet_wrap(~abordagem, nrow=1)  
dev.off()

png("metricas_por_entidade.png", width = 1024, height = 768)
ggplot(data_melt, aes(x=variable, y=value, fill=abordagem)) +
  geom_bar(position = "dodge", stat = "identity") + coord_flip()+ facet_wrap(~entidade, nrow=4, ncol=7)  
dev.off()

png("metricas_boxplot.png", width = 1024, height = 768)
ggplot(data_melt, aes(x=variable, y=value, fill=abordagem)) +
  geom_boxplot() + coord_flip()
dev.off()


