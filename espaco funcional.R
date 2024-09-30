##biocampo economia foliar DF
library("FD")
library("readxl")


##script prof


read.table("matriz_comunidade.tsv", header=T)->com
read.table("atributos_espécie.tsv", header=T)->traits
dbFD(traits,com)->res
res

### zero-sum abundances 


zero_sum_rows <- rowSums(com) == 0
which(zero_sum_rows)
com_filtered <- com[!zero_sum_rows, ]
rowSums(com_filtered)


fd_result <- dbFD(traits, com_filtered)
dbFD(traits, com_filtered, corr="lingoes")->res

names(res)
res$FRic
res$nbsp
plot(res$nbsp,res$FRic)

