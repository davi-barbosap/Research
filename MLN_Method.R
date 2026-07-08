nstall.packages("estmeansd")
devtools::install_github("stmcg/estmeansd")
library("estmeansd")

# MLN Method
res_mln <- mln.mean.sd(min.val =39, med.val = 40.143, max.val =41, n = 310825) 
res_mln