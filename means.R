means = function(data=c(),par=c(),par2=c()) { # Version 2019 - v0.0.8 
  if ((length(data)==0)| (length(par)==0)){ 
  if ( (length(data)==0)) {cat("Erreur ! Le vecteur ",substitute(data),"est vide.\n")} 
  if ( (length(par)==0))  {cat("Erreur ! Le vecteur ",substitute(par),"est vide.\n")} 
 } else { if (length(par2)>0) { 
  if ( (length(par) != length(data)) | (length(par2) != length(data)) | (length(par) != length(par2)) ) {
   cat("Erreur ! Les trois vecteurs n'ont pas la même taille.\n")
  }else {
   if (length(data[is.na(data)]>0)|length(par[is.na(par)]>0)|length(data[is.na(par2)]>0))
                  {cat("Attention ! Présence de valeurs vides NA\n")} 
   tab_temp <- na.omit(data.frame(data,par,par2))
   data <- tab_temp[,1];par <- tab_temp[,2];par2 <- tab_temp[,3]
        y = unique(par) ;    y2 = unique(par2)    
        matrice = matrix(rep(NA,length(y)*length(y2)), nc=length(y2), nr=length(y), byrow=F) 
        rownames(matrice) = y ; colnames(matrice) = y2 
        matrice_moyennes = matrice ; matrice_sd = matrice ; matrice_ic = matrice    
            resultat = c()        
            for (i in c(1:length(y2))){ 
            for (j in c(1:length(y))) { 
                temp = data[par2==y2[i]&par==y[j]] 
                matrice_moyennes[j,i] = mean(temp,na.rm = TRUE) 
                if(length(temp)==0){sd_temp=NA} 
                else {sd_temp = sd(temp,na.rm = TRUE)} 
                matrice_sd[j,i] = sd_temp 
                if(length(temp)==0){ic_temp=NA} 
                else {ic_temp = sd(temp,na.rm = TRUE)*1.96/sqrt(length(temp))} 
                matrice_ic[j,i] = ic_temp}} 
            resultat$moyennes = matrice_moyennes 
        resultat$sd = matrice_sd 
            resultat$ic = matrice_ic  ; 
        return(resultat)}
 } else { 
  if (length(par) != length(data)){
  cat("Erreur ! Les deux vecteurs n'ont pas la même taille.\n")
            } else {
   if (length(data[is.na(data)]>0)|length(par[is.na(par)]>0))
                  {cat("Attention ! Présence de valeurs vides NA\n")} 
             par = par[!is.na(data)];data = data[!is.na(data)];data = data[!is.na(par)];par = par[!is.na(par)] 
             temp = split(data,par,drop=TRUE) 
             x = c() ; y = sort(unique(par)) ; z = c() ; w = c() ; resultat = list() 
             for (i in c(1:(length(y)))) { 
                x = c(x,mean(temp[[i]],na.rm = TRUE)) 
                if (length(temp[[i]]) > 1) { 
                    z = c(z,sd(temp[[i]],na.rm = TRUE))  
                    w = c(w,sd(temp[[i]],na.rm = TRUE)*1.96/sqrt(length(temp[[i]])))} 
                else {z = c(z,NA);w = c(w,NA)} 
             }   
            resultat$moyennes = x[order(y)] ; resultat$sd = z[order(y)] 
           resultat$ic = w[order(y)] ; resultat$parametres = y[order(y)] 
            return(resultat)}}} }
# Voir l'exemple de l'application means ci-dessous 