require(ggplot2)

setwd("/home/erickson/mestrado/analises-consenso-especialistas")

dados = read.csv("acuracia_por_classe.csv",header=F,sep="|")
colnames(dados) = c("abordagem", "entidade", "precision", "recall", "f-measure", "NDCG")

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
ggplot(dados, aes(x = entidade, y = dados$"f-measure", fill = abordagem)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip()
dev.off()

png("f-measure_abordagem.png", width = 800, height = 800)
ggplot(dados, aes(x = entidade, y = dados$"f-measure", fill = abordagem)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ abordagem)
dev.off()