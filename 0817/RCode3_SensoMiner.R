require(SensoMineR)

data(chocolates)
decat(sensochoc, formul = "~Product+Panelist", firstvar = 5, graph = FALSE)

data(cocktail)

head(compo.cocktail);tail(compo.cocktail)
head(senso.cocktail);tail(senso.cocktail)
head(hedo.cocktail);tail(hedo.cocktail)
names(compo.cocktail)
names(senso.cocktail)
hedo.cocktail

myData=as.data.frame(cbind(compo.cocktail, senso.cocktail))
head(myData);tail(myData)

res.cpa = cpa(myData, hedo.cocktail)
## If you prefer a graph in black and white and with 3 clusters
res.cpa = cpa(myData, hedo.cocktail,col = gray((50:1)/50), nb.clusters = 3)


## Not run:
data(perfume_ideal)
head(perfume_ideal);tail(perfume_ideal)
res <- IdMap(perfume_ideal, col.p=2, col.j=1, col.lik=ncol(perfume_ideal), id.recogn="id_")
plot.IdMap(res, xlim=c(-7,10), ylim=c(-5,7), levels.contour=NULL, color=TRUE)
plot.IdMap(res, xlim=c(-7,10), ylim=c(-5,7), levels.contour=NULL, color=FALSE, inverse=TRUE)


#! For the IdMap
res.IdMap <- IdMap(perfume_ideal, col.p=2, col.j=1, col.lik=ncol(perfume_ideal),id.recogn="id_", cons.eq=FALSE)
plot.IdMap(res.IdMap, x=c(-7,10), y=c(-5,7), levels.contour=NULL, color=TRUE)
#! For the wIdMap
res.wIdMap <- IdMap(perfume_ideal, col.p=2, col.j=1, col.lik=ncol(perfume_ideal), id.recogn="id_", cons.eq=TRUE)
## End(Not run)


data(perfume_fcp)
head(perfume_fcp);tail(perfume_fcp)
res <- fcp(perfume_fcp, group = c(12,7,7,7,6,8))


data(perfume)
head(perfume);tail(perfume)
res<-WordCountAna(base=perfume,sep.word=";",graph=FALSE)
plot.WordCountAna(res,choix="prod")
plot.WordCountAna(res,choix="panel")
plot.WordCountAna(res,choix="dist")
plot.WordCountAna(res,choix="cons")
plot.WordCountAna(res,choix="cons",proba=0.1)
res.fast<-fast(perfume,sep.words=";")
res.consensual<-ConsensualWords(res.fast)


data(napping)
nappeplot(napping.don)
dev.new()
pmfa(napping.don)
pmfa(napping.words)

