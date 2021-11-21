library(datasets)

data("swiss")


library("ggplot2")
library("ggrepel")

library(smacof)

head(swiss)

str(swiss)

swiss_scale = scale(swiss[,-1])
swiss_dist = dist(x = swiss_scale)
swiss_mds= mds(delta = swiss_dist , ndim = 2 , type = "ratio")
swiss_mds

 ggplot() + geom_point(data = as.data.frame(swiss_mds$conf) , mapping = aes(x = D1, y = D2), color = "blue", alpha = 0.5) + labs(title = "figure4: MDS configuration of swiss")

perf = substr(x = as.character(japan_perf[,1]), start = 1, stop = 3)

ggplot() + geom_point(data = as.data.frame(japan_mds$conf) , mapping = aes(x = D1, y = D2), color = "blue", alpha = 0.5) + geom_text_repel(data =data.frame(perf ,japan_mds$conf), mapping = aes(x=D1, y=D2 , label = perf)  ) + labs(title = "figure5:  MDS configuration of Japan Prefectures with labels")


perf = substr(x = as.character(swiss[,1]), start = 1, stop = 3)

ggplot() + geom_point(data = as.data.frame(swiss_mds$conf) , mapping = aes(x = D1, y = D2), color = "blue", alpha = 0.5) + geom_text_repel(data =data.frame(perf ,swiss_mds$conf), mapping = aes(x=D1, y=D2 , label = perf)  ) + labs(title = "figure5:  MDS configuration of swiss Prefectures with labels")


dhat_matrix = as.matrix(swiss_mds$dhat)
d_matrix = as.matrix(swiss_mds$confdist)
denominator = sum(dhat_matrix[upper.tri(dhat_matrix)]^2)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
nominator = sum((p_ij - d_ij)^2) 
normalized_stress = nominator/denominator

normalized_stress


