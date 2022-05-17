
#paquetes
library(SSP)
library(vegan)
library(ggplot2)
library(dplyr)


ave <- runif(30, min = 10, max = 40)
var <- ave

sim1 <- matrix(data = NA, nrow = 20, ncol = 30)
sim2 <- matrix(data = NA, nrow = 10, ncol = 30)

for (j in 1:30){
    sim1[,j] <- rnorm(20, mean = ave[j], sd=var[j])   
    sim2[,j] <- rnorm(10, mean = ave[j], sd=1.5*var[j])
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
PERMANOVA2(sim.n, factor = grupos, distancia = "euclidean")




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



##Modifiquemos la estructura desde el Centroide
bray1.m <- as.matrix(bray)
A <- -0.5*(bray1.m^2)
nn <- dim(A)[1]
ones <- matrix(1,nn)
I <- diag(1,nn)
Gower <- (I-1/nn*ones %*% (t(ones))) %*% A %*% (I-1/nn*ones %*% (t(ones)))
EG <- eigen(Gower)
vectors <- sweep(EG$vectors, 2, sqrt(abs(EG$values)), FUN = "*")
scoresPCO <- as.data.frame(vectors)

centroide <- scoresPCO %>% 
              summarise_all(mean) %>% 
              mutate(grupos = "C")


dat.b2 <- log(dat.b[1:6,]+1)
dat.b3 <- dat.b[7:12,]
dat.b4<-rbind(dat.b2, dat.b3)
muestreo2 <- rbind(sqrt(dat.a), dat.b4)
bray2 <- vegdist(muestreo2, method="bray", binary = F)

#evaluación de dispersión
disp2 <- betadisper(bray2, group = grupos)
boxplot(disp2)
permutest(disp2, permutations = 9999)

#Evaluación de diferencias en posición. Se debe retener la H0: no diferencias en grupos
anosim(bray2, grouping = grupos)
adonis(bray2~grupos)

mds2<-metaMDS(bray2)

MDS2<-as.data.frame(mds2$points)
MDS2$grp<-grupos

plot.MDS2<-ggplot(data=MDS2 ,aes(x=MDS1, y=MDS2))+
  geom_point(aes(colour=grupos), size=3.5)+
  theme_bw(base_size=16)

plot.MDS2

#PERMANOVA2
PERMANOVA2(muestreo2, factor = grupos2)
