library("FD")
read.table("matriz_comunidade.tsv", header=T)->com
read.table("atributos_espécie.tsv", header=T)->traits
##ajuste das matrizes para realizar dbFD
zero_sum_rows <- rowSums(com) == 0
which(zero_sum_rows)
com_filtrado <- com[!zero_sum_rows, ]
rowSums(com_filtrado)
## dbFD
fd_result <- dbFD(traits, com_filtrado)
dbFD(traits, com_filtrado, corr="lingoes")->res
names(res)
res$FRic
res$nbsp
##plotagem FRic por nbsp
plot(res$nbsp,res$FRic,
     xlab = "Número de Espécies (nbsp)",
     ylab = "Riqueza Funcional (FRic)",
     main = "Relação entre Número de espécies e Riqueza Funcional")
text(res$nbsp, res$FRic, labels=rownames(com_filtrado), pos=4, cex=1)
##teste de regressão 
regressao<-lm(res$FRic~res$nbsp)
summary(regressao)
abline(regressao)
regressao
##teste de correlação
correlacao<-cor.test(res$nbsp,res$FRic)
print(correlacao)
##verificação de resíduos 
residuos<-regressao$residuals
qqnorm(residuos)
qqline(residuos, col="blue")
histogram(residuos)
summary(residuos)
teste_normalidade<-shapiro.test(residuos)
print(teste_normalidade)
