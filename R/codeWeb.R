# Fonction pour représenter les réseaux
  # arguments: TL = vecteur qui contient le niveau trophique des noeuds (dans le même ordre que la matrice de réseau webTL)
  #            webTL = matrice du réseau trophique, en colonne: noeud en tant que prédateur, en ligne: noeud que tant que proie
  #            colnode = vecteur qui contient les couleurs de chaque noeud (dans le même ordre que la matrice de réseau webTL)
  #            abund = vecteur qui contient l'abondance de chaque noeud (échelle en abundance relative dans le réseau, à modifier suivant distribution des abondances)
  #            collink = couleur des liens trophiques, défaut =  "grey70"
#exemples de couleurs sympa: couleur<-colors() coulbis<-couleur[c(26,51,552,393,520,10,498,652,75,536,96,1,640,56,226,128,259,503,383,471,116,151,203,490,497,35,394,468,645,41,153,29,656,137,258,650,22)]

PlotWeb <- function (TL, webTL,colnode,abund,collink="grey70", scale_abun = 0.01, rel_abun = TRUE, log_abun = FALSE){

  Sweb<-length(TL)
  g=matrix(0,nrow=Sweb,ncol=3)

  if (log_abun) {
    # to not have negative abun: 
    if(any(abund < 1)) {
      abund <- abund + 1
    }
    abund <- log(abund)

  }

  if (rel_abun) {
    g[,3]<- scale_abun * abund / sum(abund)
  } else {
    g[,3]<- scale_abun * abund
  }
  g[,2]<-TL/sum(TL)
  TLround<-round(TL)
  for (i in 1:max(round(TL))) {
    a<-TLround==i
    b<-1:Sweb
    b<-b[a]
    xaxis<-(1:length(b))/length(b)
    g[a,1]<-xaxis+ 0.5 - sum(xaxis)/length(b)
  }
  symbols(g[,1],g[,2],circles=g[,3],inches=FALSE,bg=colnode,fg=colnode,xlab="",ylab="",bty="n",xaxt="n",yaxt="n")
  for (i in 1:Sweb){
    for (j in 1:Sweb){
      if (webTL[i,j]>0){
         arrows(g[i,1],g[i,2],g[j,1],g[j,2],lwd=1, col=collink,length=0.1)
         }
    }
  }
  symbols(g[,1],g[,2],circles=g[,3],inches=FALSE,bg=colnode,fg=colnode,add=TRUE)
}


# fonction pour calculer les niveaux trophiques si besoin
GetTL2 <- function(web){

    ## takes predation matrix with consumers in columns
    ## identify the columns with basal species
    tweb <- t(web)

    ## make the rows add to one
    rs <- rowSums(tweb)
    for(i in 1:length(tweb[,1]))
        tweb[i,tweb[i,]>0] = tweb[i,tweb[i,]>0]/rs[i]

    nb.TL <- try(solve(diag(length(tweb[,1])) - tweb), T)

    if(class(nb.TL)=="try-error")
        nbTL <- rep(NA, length(tweb[,1]))

    if(class(nb.TL)!="try-error")
        nbTL <- rowSums(nb.TL)

    nbTL

}


