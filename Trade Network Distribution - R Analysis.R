require(igraph)
imports<-read.csv("trade_imports_2018.csv")
cdata<-read.csv("country_metadata.csv")
nimports<-graph.data.frame(imports)
V(nimports)$ccode<-cdata$ISO.alpha3.code
V(nimports)$m49code<-cdata$M49.code
V(nimports)$region<-cdata$Region.1
V(nimports)$regiondetail<-cdata$Region.2
V(nimports)$continent<-cdata$Continent
V(nimports)$gdp<-cdata$GDP.per.capita
exports<-read.csv("trade_exports_2018.csv")
nexports<-graph.data.frame(exports)
V(nexports)$ccode<-cdata$ISO.alpha3.code
V(nexports)$m49code<-cdata$M49.code
V(nexports)$region<-cdata$Region.1
V(nexports)$regiondetail<-cdata$Region.2
V(nexports)$continent<-cdata$Continent
V(nexports)$gdp<-cdata$GDP.per.capita



summary(nimports)
## IGRAPH 51cdcc8 DN-- 211 614 -- 
##  + attr: name (v/c), ccode (v/n), m49code (v/n), region
## (v/n), regiondetail (v/n), continent (v/n), gdp (v/n),
## PercentOfImports (e/n)
summary(nexports)
## IGRAPH 51ed25e DN-- 211 603 -- 
##  + attr: name (v/c), ccode (v/n), m49code (v/n), region
## (v/n), regiondetail (v/n), continent (v/n), gdp (v/n),
## PercentOfExports (e/n)

## I generated a plot to make sure I have loaded a coherent network 
plot(nimports,edge.width=0.1,vertex.size=degree(nimports)/6,main='Imports',vertex.color=3, edge.arrow.size=0.01,vertex.label.cex=0.4)
plot(nexports,edge.width=0.1,vertex.size=degree(nexports)/6,main='Exports',vertex.color=3, edge.arrow.size=0.01,vertex.label=NA)
## looks good, I see some clustering (nodes prefering to connect to nodes with shared connections, 
## as well as some hubs (China labeled in Imports, large number of in nodes - so heavy importers?))


is_connected(nimports)
## [1] TRUE
is_connected(nexports)
## [1] TRUE

NetworkComparisonTable <- data.frame(c('Imports','Exports'),c(graph.density(nimports), graph.density(nexports)), c(average.path.length(nimports),average.path.length(nexports)),    c(transitivity(nimports), transitivity(nexports)), c(transitivity(nimports, type='global'), transitivity(nexports, type='global')))
colnames(NetworkComparisonTable) <- c('Network', 'Density', 'Average path length', 'Transitivity', 'Global Clustering')
## NetworkComparisonTable
## Network    Density Average path length Transitivity Global Clustering
## 1 Imports 0.01385692            2.278597   0.07378167        0.07378167
## 2 Exports 0.01360867            3.020854   0.08383234        0.08383234

install.packages("gridExtra")
library(gridExtra)
pdf("NetworkComparisonTable.pdf")
grid.table(NetworkComparisonTable)
dev.off()

graph.density(nimports)
## [1] 0.01385692

erdos<- sample_gnp(n=212, p=0.01385692)
par(mfrow=c(1,2), mar=c(0,1,2,1)) 
plot(sample_gnp(n=212, p=0.01385692), vertex.size=0.01, vertex.label=NA, main="First random network")
plot(sample_gnp(n=212, p=0.01385692), vertex.size=0.01, vertex.label=NA, main="Second random network")



## Way less clustering, and this is obvious in the graphs. In addition, the 
## ER graph has visible isolates. Given that we are working with trade relations, this type of network 
## component (a state without trading partners) is  not relaistic. 
## There is also less density seen in the ER grpah. The real world graph in addition to clustering  had "hubs" that 
## were nodes with a relatively high amount of connections.
## These are all known limitations of the ER graph when compared to real world models. 
## The Poission distribution  shows that too many nodes cluster around the mean degree and that the degree distibution
## decays too quickly (leading to a lack of hubs observed in real networks).


NetworkComparisonTable2 <- data.frame(c('Imports','ErdosImports'),c(mean(degree(nimports)), mean(degree(erdos))), c(average.path.length(nimports),average.path.length(erdos)),    c(transitivity(nimports), transitivity(erdos)))
colnames(NetworkComparisonTable2)<- c('Network', 'Mean Degree', 'Average path length', 'Transitivity')
NetworkComparisonTable2
## Network Mean Degree Average path length Transitivity
## 1      Imports    5.819905            2.278597  0.073781665
## 2 ErdosImports    2.905660            4.950741  0.003340757


## These metrics certainly validate our observations 
## about the limitations of the ER random network models and
## the differences in connectivity observed in the plots and discussed above.
## Transitivity is greater by far in the real network, reiterating the lack 
## of clustering we observed in the ER network compared to the plot of the real 
## network. In addition, the mean degree in a network with hubs will be much 
## higher than that of theER network (see Poission distribution) where the nodes cluster around the mean degree (in this case, a low 2.9 for the ER network)
## The presence of hubs means that much like the sdiffusion of a contagion or discussions of real networks lower path legnths, random
## nodes are likely to be connectedto hubs, making the average path legnth of a real network much lower than a radnom ER network 
## generation, hich can be clearly obsevred in both our networkplots and 
## our metrics. Clustering makes sense given regional ties mean 
## that there are both cultural and economic incentives forimport clustering. 

## For the export models, I will create a random graph and analze its metrics
graph.density(nexports)
## [1] 0.01360867
exerdos<- sample_gnp(n=212, p=0.01360867)
plot(exerdos,edge.width=0.1,vertex.size=degree(exerdos)/7,edge.arrow.size=0.01,vertex.label.cex=0.1, main="Export Erdos Random network")

NetworkComparisonTable3 <- data.frame(c('Exports','ErdosExports'),c(mean(degree(nexports)), mean(degree(exerdos))), c(average.path.length(nexports),average.path.length(exerdos)),    c(transitivity(nexports), transitivity(exerdos)))
colnames(NetworkComparisonTable3)<- c('Network', 'Mean Degree', 'Average path length', 'Transitivity')
NetworkComparisonTable3
## Network Mean Degree Average path length Transitivity
## 1      Exports    5.715640            3.020854   0.08383234
## 2 ErdosExports    2.849057            5.095514   0.01766784

## We see the same lack of clustering and hubs observed in the import random networks generations. 

inbar<- sample_pa(n=212, m=(5.819905/2), directed=T, out.pref=T)
plot(inbar,edge.width=0.1,vertex.size=degree(inbar),edge.arrow.size=0.01,vertex.label.cex=0.1, main="Import Barabasi Model")
exbar<- sample_pa(n=212, m=(5.715640/2), directed=T, out.pref=T)

NetworkComparisonTable4 <- data.frame(c('Imports', 'ErdosImports', 'BarabasiImports', 'Exports', 'ErdosExports','BarabasiExports'),c(mean(degree(nimports)), mean(degree(erdos)), mean(degree(inbar)), mean(degree(nexports)), mean(degree(exerdos)), mean(degree(exbar))), c(average.path.length(nimports), average.path.length(erdos), average.path.length(inbar), average.path.length(nexports),average.path.length(exerdos), average.path.length(exbar)),    c(transitivity(nimports), transitivity(erdos), transitivity(inbar), transitivity(nexports), transitivity(exerdos), transitivity(exbar)))
colnames(NetworkComparisonTable4)<- c('Network', 'Mean Degree', 'Average path length', 'Transitivity')

## NetworkComparisonTable4
## Network Mean Degree Average path length Transitivity
## 1         Imports    5.819905            2.278597  0.073781665
## 2    ErdosImports    2.905660            4.950741  0.003340757
## 3 BarabasiImports    3.971698            2.539912  0.029239766
## 4         Exports    5.715640            3.020854  0.083832335
## 5    ErdosExports    2.849057            5.095514  0.017667845
## 6 BarabasiExports    3.971698            2.737678  0.038557658

## The Barabasi model significantly improved transitivity due to the preferential attachment simulation closely mirroring
## the behavior of trade ties. However, the real network still has a lower average path length because trade networks prioritize
## connections to hubs (or nations connected to hubs) in their primary import/export edge list more than the models, giving the benefits of engaging in trade with a 
## high gdp country. 

network<-nimports+nexports
plot(network)
plot(network,edge.width=0.1,vertex.size=0.5,main='Combined Trade Network',vertex.color=3, edge.arrow.size=0.01,directed= TRUE, vertex.label=NA)
pr<- page.rank(nimports)$vector
bet<-betweenness(nimports)
deg<-degree(nimports,mode='in')
library(scales)
nimlayout<-layout.fruchterman.reingold(nimports)
plot(nimports, vertex.color=3, vertex.label=NA, edge.arrow.size=0.25, vertex.size=rescale(pr,c(1,20)),main='Imports - Node Size By Pagerank Centrality', edge.width=0.5, layout=nimlayout)
plot(nimports, vertex.color=3, vertex.label=NA, edge.arrow.size=0.25, vertex.size=rescale(bet,c(1,20)),main='Imports - Node Size By Betweenness Centrality', edge.width=0.5, layout=nimlayout)
plot(nimports, vertex.color=3, vertex.label=NA, edge.arrow.size=0.25, vertex.size=degree(nimports, mode='in')/8,main='Imports - Node Size By Degree Centrality', edge.width=0.5, layout=nimlayout)


summary(deg)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00    0.00    0.00    2.91    1.50  149.00 
summary(pr)
## Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0007109 0.0007109 0.0007109 0.0047393 0.0010986 0.2107504 
summary(bet)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.0     0.0     0.0    15.2     4.0   666.7 
cents <- data.frame(deg,pr,bet)
cor(cents)
## deg        pr       bet
## deg 1.0000000 0.8188196 0.9358727
## pr  0.8188196 1.0000000 0.8524973
## bet 0.9358727 0.8524973 1.0000000
plot(cents)

expr<- page.rank(nexports)$vector
exbet<-betweenness(nexports)
exdeg<-degree(nexports,mode='in')
nexlayout<-layout.fruchterman.reingold(nexports)
plot(nexports, vertex.color=3, vertex.label=NA, edge.arrow.size=0.25, vertex.size=rescale(expr,c(1,20)),main='Exports - Node Size By Pagerank Centrality', edge.width=0.5, layout=nexlayout)
plot(nexports, vertex.color=3, vertex.label=NA, edge.arrow.size=0.25, vertex.size=rescale(exbet,c(1,20)),main='Exports - Node Size By Betweenness Centrality', edge.width=0.5, layout=nexlayout)

library(igraphdata)
library(ape)
library(Matrix)



assortativity.nominal(nimports,V(nimports)$continent)
## [1] 0.3984324
assortativity.nominal(nexports,V(nexports)$continent)
## [1] 0.3561265
##Assortativity is a probability measurement and complete assortativity would have a value of 1. 
## Given this, it seems that there is very high association in import/export networks between 
## countries belonging to the same continent. This makes sensde, as costs favors regional sorting, while things like culture costs can influence demand for certain imports
## and relate to region. In a globalized world, the assortativity by continent is not total - the existent of network hubs observed earlier. Exports seem to have slightly less assortativity 
## exports, to the extent that a country engages in them, are more likely to travel further distances. 
## also attest to that. 

## Low energy arrangments for both networks collapse into 9 subcategories. This implies that 
## these networks are assorting into categories (maybe subregions?) smaller than continents. 
plot(nimports, vertex.color=V(nimports)$continent, vertex.label.cex=0.4, edge.arrow.size=0.005, vertex.size=rescale(deg,c(1,40)),main='Imports by Continent - Node Degree Centrality', edge.width=0.5, layout=nimlayout)
plot(sg,nimports,vertex.size=rescale(deg,c(1,40)), vertex.label.cex=0.6, edge.width = 0.5, edge.arrow.size=0.005,vertex.size=3,main='Import Spinglass - Nodes by Degree', layout = nimlayout)
plot(nexports, vertex.color=V(nexports)$continent, vertex.label.cex=0.53, edge.arrow.size=0.005, vertex.size=rescale(exdeg,c(1,40)),main='Exports by Continent - Node Degree Centrality', edge.width=0.5, layout=nexlayout)
plot(sg,nexports,vertex.size=rescale(exdeg,c(1,40)), vertex.label.cex=0.6, edge.width = 0.5, edge.arrow.size=0.005,vertex.size=3,main='Export Spinglass - Nodes by Degree', layout = nexlayout)
## Divisions seein in the sg import model- regional- actually, the strongest regional tie seen in this model seems hemispheric (US and Mexico compared to the grouping of several EU countries (Germany, Italy) with the Middle East and China in the lowest energy model. 
## Cultural ties - France and Spain are distinctly seprated out in the SG import model, possibly explainedby an export network within the francophone and Spanish speaking worlds )
## The United Kingdom and the US are sorted together in the spinglass export model, likely due to a special cultural and lingustic tie betweent he two nations
## The hemispheric and regional division persists in the sp export model.
## Overall, most countries in the spinglass model are sorted by their immediate region (with more divisions than continents) and the exceptions deal with the hubs - nations that are mostly EXPORTING a great deal  
## This validates the original observation form the assortativity value - that there were cultural and regional ties that influenced these networks in addition to continent, and that hubs would be exceptions to continental sorting due
## to the wide reach of their import and export activity. The spinglass models also shed light on how regional assortativity can actually run counter to continent - the import spinglass model groups many east african nations with the Middle East - geograpically plausible as neighbors but obviously
## from different continents. 

require(intergraph)
detach(package:igraph)
require(sna)

istatnet<-asNetwork(nimports)
estatnet<-asNetwork(nexports)
blockmodel(istatnet,ec=istatnet %v% "continent")

## Block 1     Block 2     Block 3     Block 4     Block 5     Block 6
## Block 1 0.0114478114 0.029818182 0.017316017 0.003977273 0.000000000 0.000000000
## Block 2 0.0003636364 0.043673469 0.009047619 0.008750000 0.001052632 0.000000000
## Block 3 0.0000000000 0.008571429 0.058652729 0.003720238 0.000000000 0.000000000
## Block 4 0.0000000000 0.015000000 0.013392857 0.049395161 0.000000000 0.002403846
## Block 5 0.0009569378 0.024210526 0.006265664 0.011513158 0.052631579 0.000000000
## Block 6 0.0000000000 0.016923077 0.007326007 0.031250000 0.000000000 0.070512821
blockmodel(estatnet,ec=estatnet %v% "continent")
## Block 1     Block 2     Block 3     Block 4      Block 5     Block 6
## Block 1 0.0111111111 0.023636364 0.021645022 0.007386364 0.0009569378 0.000000000
## Block 2 0.0018181818 0.035102041 0.012857143 0.013125000 0.0000000000 0.000000000
## Block 3 0.0004329004 0.003333333 0.062137050 0.005208333 0.0000000000 0.000000000
## Block 4 0.0017045455 0.003750000 0.015625000 0.053427419 0.0016447368 0.012019231
## Block 5 0.0009569378 0.030526316 0.008771930 0.014802632 0.0175438596 0.004048583
## Block 6 0.0013986014 0.018461538 0.009157509 0.028846154 0.0000000000 0.051282051

## What we see here in the blockmodels is that continents have elevated tie probabilities in regions that are not within their continent.
## This evidences the observation of preferential selection that IS based on region but NOT based on continent discussed above. 
## We also see the hub effect discussed earlier here, especially in that regions with nations that have high amounts of
## exports and imports overall are more likely to have ties across other blocks. 


detach(package:blockmodeling)
detach(package:igraph)
require(sna)
require(network)

imec<-equiv.clust(list(istatnet))
plot(imec,labels=network.vertex.names(istatnet))
exec<-equiv.clust(list(estatnet))
plot(exec, labels=network.vertex.names(estatnet))
imecbm<-blockmodel(istatnet,imec, k=6)
imecbm
execbm<-blockmodel(estatnet, exec, k=6)

## From this k=6 cut and the dendogram we cna get a better picture of what was driving the probability of a tie in the previous analysis. Continents with hubs are single-handedly shifting 
## the probability of a tie upward, as certain countries are responsible for an overwhelming amount of both export activity and the import activities on a national scale. The fact that all
## countries that are not these top hubs carry a 1 value means that for the majority of nations, their largest share of exports and import ties are with one of the nations designated as hubs. 
## This is different that regional preference, as a continent with a "hub" would simply appear to be preferred. However the small communities of the dendogram do finally break down into clusters that are very 
## well aligned with the idea of subregional preferences (see the import and export dendograms, for example, certain Latin American countries are grouped together). This regional selectivity means that
## Many countries have regional neighbors amoung their top trading partners, and a few will even have a regional partner as a top trading partner over a hub (although this trading partner is very likely to be connected to a hub 
## itself, see the network graphs, the degree centrality, the betweeness centrality, and the pagerank centrality grphs. The graphs show that proximity to a hub increases degree centrality, betweeness centrality, and pagerank centrality))

library(network)
library(sna)
library(ergm)
require(igraph)

sicdata<-read.csv("country_metadata_standin.csv")
V(nimports)$sigdp<-sicdata$GDP.per.capita


V(nimports)$gdplog<-log(V(nimports)$sigdp)


nnimports<-intergraph::asNetwork(nimports)
nnexports<-intergraph::asNetwork(nexports)
model<- ergm(nnimports ~ edges + nodeifactor("continent") + nodematch("continent") + nodeicov("gdplog") + mutual)
summary(model)

## an edge term is the equivalent of an intercet regression
## with out equation, we're saying that we're saying that we think (along with "edges") these variables should be 
## influencing their likelihood of relationships with others, and more generally are terms that help give rise to 
## the overall structure of the network.
## We looked at the nodeifactor for continent to give the probability of an incoming import tie 
## (that is, probability of ego country receiving and import from alter) based on continent. Nodeicov gdp breaks down the probability of an incoming
## trade tie (imports) based on GDP, a new insight enabled by the network metadata. nodematch continent is 
## a parameter to observe network homophily by continent. - nodematch is a straightforward measure of the network 
## homophily for- probability of assocition by continent. The effect of "mutual"- mutual is a consideration of recirpocity in 
## network ties. The expected effect would be an improvement in the ergm's precision (based on the AIC value). The reason why 
## reciprocity improves ergms is that real world networks are extremely high in reciprocal ties (along with other features like local clustering
## and heavy tailed distrubutions), that are hard to capture algorithmatically. 
## It is important to note that for two continents (5 and 6) my ergm did not produce significant coefficients for nodeicov based on the standard deviation. 
## This might be explained by the hub discussion from earlier questions. Many nations will have top import alters that are a large hub nation rather than a nation from
## their own continent. Also, as we discussed earlier regional interactions sometimes does nto completely overlap with continent category. 
## Nodeicov gdp produced a statistically significatiant coeeficient for ties - this is evidence that high importing "hubs", presumable with high gdp due
## to their increased number of ties are shaping the network structure. A high coefficienct here means nations with high gdps have a higher probability of
## having an import tie - so they have a lot of ties/ appearances as import originators in the original edgelists from the csv. 
## Odd Ratio generation

or<- exp(model$coef)
or

## We can see from the odds ratios that sharing a continent is still a major predictor for both the probability of a tie sharing continent (4.5x as likely -nodematch)
## For nodeifactor continent, the strong coefficients show that certain incoming import ties are a good predictor of what other ties those nodes will have, and therefore a 
## good explanation of how the overall network structure takes place. 
## To strengthen the model, I am going to add a nodematch for subregion. Similar to the blockmodel insights, this will account for local ties with more detail than continents. Since other steps like the blockmodel 
## and the dendogram have already shown subregion to be a significant network forming factor, I predict this addition will improve the ergm accuracy. 
## subregion to be a significant 

model2<- ergm(nnimports ~ edges + nodeifactor("continent") + nodematch("continent") + nodematch("region") + nodeicov("gdplog") + mutual)
summary(model2)

## We have slightly lowered the AIC and thus improved our ergm with this addition. 
## GOF test for model fit
sim<-simulate(model2)
goftest <- gof(model2 ~ idegree + odegree + esp + distanc
pdf("GOF.pdf")
plot(goftest)
dev.off()

## I ran a GOF test that actually shows a lot of the ergm's weakness to explain the actual network. The reason is that we are not 
## dealing with a real network that is perfectly analogous to social networks. Countries choosing import partners
## is a utilitarian network and the utilization of hubs matters much more. Thesemodels cannot capture the fact that it is important for even small
## nations to have a tie with large import hubs, nor can regional specificity capture the international nature of hub connections
## and a lot of our ergm terms were very focused on region. The unique features of this network has made replication dificult, but identification of 
## network formation attributes easy once a variety of different methods were employed. 