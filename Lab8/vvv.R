
#paquetes
library(SSP)
library(vegan)
library(ggplot2)
library(dplyr)

set.seed(42)
ave <- runif(30, min = 10, max = 40)
var <- sqrt(ave)

sim1 <- matrix(data = NA, nrow = 20, ncol = 30)
sim2 <- matrix(data = NA, nrow = 10, ncol = 30)

for (j in 1:30){
    sim1[,j] <- rnorm(20, mean = ave[j], sd=var[j])   
    sim2[,j] <- rnorm(10, mean = ave[j], sd=2*var[j])
    }

sim1 <- as.data.frame(sim1)
sim2 <- as.data.frame(sim2)
sim <- rbind(sim1, sim2)

sim.n <- decostand(sim, method = "standardize")

euc <- dist(sim.n)

#evaluación de dispersión
grupos <- c(rep("a",20), rep("b",10))
disp <- betadisper(euc, group = grupos)
boxplot(disp)
permutest(disp)

#Evaluación de diferencias en posición. Se debe retener la H0: no diferencias en grupos
anosim(euc, grouping = grupos)
adonis(euc~grupos)

#plot
mds1<-metaMDS(euc)

MDS1 <- as.data.frame(mds1$points)
MDS1$grp <- grupos

plot.MDS1 <- ggplot(data=MDS1 ,aes(x=MDS1, y=MDS2))+
  geom_point(aes(colour=grupos), size=3.5)+
  theme_bw(base_size=16) 



plot.MDS1


#PERMANOVA2
PERMANOVA2(sim.n, factor = grupos, distancia = "euclidean", nperm = 999)



#Muestreo de dos grupos de 12 elementos cada uno
a <- round(runif(12,0,1000))
b <- round(runif(12,0,1000))
dat.a <- dat[a,]
dat.b <- dat[b,]  
grupos <- c(rep("a", 12),rep("b", 12))
muestreo <- rbind(dat.a, dat.b)

#Aproximación multivariada usando Jaccard
bray <- vegdist(sqrt(muestreo), method="bray", binary = F)

#evaluación de dispersión
disp <- betadisper(bray, group = grupos)
boxplot(disp)
permutest(disp)

#Evaluación de diferencias en posición. Se debe retener la H0: no diferencias en grupos
anosim(bray, grouping = grupos)
adonis(bray~grupos)

#plot
mds1<-metaMDS(bray)

MDS1 <- as.data.frame(mds1$points)
MDS1$grp <- grupos

plot.MDS1 <- ggplot(data=MDS1 ,aes(x=MDS1, y=MDS2))+
  geom_point(aes(colour=grupos), size=3.5)+
  theme_bw(base_size=16) 



plot.MDS1


