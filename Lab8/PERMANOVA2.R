SS <- function (d) {
  n <- dim(as.matrix(d))[1]
  ss <- sum(d^2)/n
  return(ss)
} ## Sum of Squares using Theorem of Huygen

v = function (d) {
  n <- dim(as.matrix(d))[1]
  ss <- sum(d^2)/n
  v <- ss/(n-1)
  return(v)
} ## Multivariate Dispersion



pseudo.F<-function (x, factor, distancia){
  d<-vegdist(x, method = distancia)
  TSS<-SS(d)
  group<-as.data.frame(factor)
  x$grp<-factor
  #
  factor <- as.factor(factor)
  #
  lab <- names(table(group)) 
  lev <- table(group)
  CR<- c(1:nlevels(factor)) 
  for (i in 1:nlevels(factor)){
    CR[i]<-SS(vegdist(x[x$grp==lab[i],1:length(x)-1],method = distancia))
  }
  RSS<- sum(CR)
  Var <- c(1:nlevels(factor))        
  d.res<-as.data.frame(matrix(nrow=length(levels(factor)), ncol=3))   
  for (i in 1:nlevels(factor)){
    Var[i]<-v(vegdist(x[x$grp==lab[i],1:length(x)-1],method = distancia))
    d.res[i,]<-c(lev[i],
                 Var[i],
                 (1-(lev[i]/sum(lev)))*Var[i])
  }
  den<-sum(d.res$V3)
  ASS<-TSS-RSS
  Fobs<- ASS/den
  return(Fobs)
} ## modified Pseudo-F, based on Brown and Forsythe (1974) and Anderson et al. (2017)

###### Test ########################################################33

PERMANOVA2 <- function(x, factor, distancia, nperm = 999) {
  ## Check / modify arguments of the control argument
  # control <- check(x, control)$control 
  control <- how(nperm = nperm, within = Within(type = "free"))
  Fobs<-pseudo.F(x, factor, distancia = distancia)
  Nobs <- nobs(x)
  F.permu <- numeric(length = control$nperm) + 1
  F.permu[1] <- Fobs
  ## Generation of pseudo.F values for H0 using permutations without replacement
  for(i in seq_along(F.permu)) {
    ## return a permutation
    want <- permute(i, Nobs, control)
    ## calculate permuted F
    F.permu[i+1] <- pseudo.F(x[want,], factor, distancia = distancia)
  }
  ## probability for Fobs
  pval <- sum(abs(F.permu) >= abs(F.permu[1])) / (control$nperm + 1)
  ## Results
  return(data.frame("Pseudo-F" = F.permu[1], "p(perm)" = pval))
}## Permutation test based on Anderson et al. (2017)

