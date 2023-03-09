#=========================
#        CQFidalgo
#      Laboratorio 5
#=========================

library(SyncRNG)

#=======================
#       FUNCIONES
#=======================

#Función que crea la matriz inicial y devuelve los pesos de los pasillos.
#Parametros:
#size: numero de habitaciones por fila/columna.
#ratio: proporción entre los elementos de la matriz.
#semilla1: semilla utilizada parala estructura y las coordenadas inicial y final. 
#semilla2: semilla utilizada para introducir valores a los pesos de los pasilos.
generaLaberinto <- function(size, ratio, semilla1, semilla2) {
  s1<-SyncRNG(seed=semilla1)
  s2<-SyncRNG(seed=semilla2)
  mat_size<-size*2+1
  mat<-matrix(rep(0,mat_size^2),nrow=mat_size)
  #Rellena con habitacion o pared
  for(i in 1:mat_size){
    for(j  in 1:mat_size) {
      if((i%%2==0)&(j%%2==0)) {
        mat[i,j]<-0
      }
      else{
        mat[i,j]<-10
      }
    }
  }
  num_doors<-2*(size-1)*(size - 1)
  open<-ratio*num_doors
  
  for(i in 1:open) {
    vert_horz<-s1$randi()%%2
    if(vert_horz==0) {
      row<-s1$randi()%%size
      col<-s1$randi()%%(size - 1)
      row<-row*2+2
      col<-col*2+3
    }
    else{
      row<-s1$randi()%%(size-1)
      col<-s1$randi()%%size
      row<-row*2+3
      col<-col*2+2
    }
    mat[row,col]<-s2$randi()%%9+1 #valores de pesos
  }
  
  #escoge y marca habitación de salida
  rowSal<-s1$randi()%%(size)
  colSal<-s1$randi()%%(size)
  mat[rowSal*2+2,colSal*2+2]<-(-1)
  
  #escoge y marca habitación de destino
  rowDes<-s1$randi()%%(size)
  colDes<-s1$randi()%%(size)
  mat[rowDes*2+2,colDes*2+2]<-(-2)
  
  return(list(mat,c(rowSal,colSal),c(rowDes,colDes)))
}


############################
#### ALGORITMO DIJKSTRA ####
############################

#Algoritmo de Dijkstra sin cola
#Algoritmo que encuentra el camino más corto entre dos habitaciones (nodos),
#para ello se ayuda de una matriz de distancias (solo contiene habitaciones)
#que en un principio tiene valores infinitos y va actualizando según va 
#incluyendo esas coordenadas en la matriz final, una de habitaciones visitadas, 
#que toma valores 0 y 1 (inicialmente a 0, toma valor 1 si se visita esa habitacion)
#y una matriz final que es la que se puede representar graficamente.

#Parametros:
#datos: matriz con los pesos generados aleatoriamente.
dijkstraSinCola<-function(datos){
  matriz <- datos[[1]]
  rowSal <- datos[[2]][1]
  colSal <- datos[[2]][2]
  rowDes <- datos[[3]][1]
  colDes <- datos[[3]][2]
  size<-(ncol(matriz)-1)/2 #Tamaño de las matrices de habitaciones
  #Crea la matriz de distancias entre las habitaciones inicializandolas a infinito.
  dist <-matrix(rep(Inf,size^2),ncol=(size),nrow=(size))
  #Crea la matriz de visitadas inicianizandola a 0 (tendrá un 1 en las coordenadas que visite)
  visit<-matrix(rep(0,size^2),ncol=(size),nrow=(size))
  dist[rowSal+1,colSal+1]<-0 #El punto de salida tendrá distancia 0
  grafica<-matriz #Crea la matriz final que podremos representar graficamente

  repeat{
    min<-Inf #El menor tiene un valor muy grande inicialmente
    fil<-0
    col<-0
    
    #Busca el valor mas pequeño de la matriz distancias que no esté visitado
    for (i in 1:dim(dist)[1]) {
      for (j in 1:dim(dist)[2]) {
        if(min>dist[i,j] && visit[i,j]==0){ #La actualiza si es mas pequeño y no está visitado
          min<-dist[i,j]
          fil<-i
          col<-j
        }
      }
    }
    #Introduce un 1 en las coordenadas de la matriz de visitados del menor encontrado.
    visit[fil,col]<-1
    
    for(i in c(-1,1)) { #Se mueve entre los cuatro pasillos adyacentes
      #Comprueba si es no es pared, no está visitado y no es el final
      #Si cumple, actualiza la matriz final
      if(matriz[(fil*2)+i,col*2]<10 && visit[fil+i,col]==0 && matriz[fil*2,(col*2)]!=-2) { #Arriba y abajo
        grafica[(fil*2)+i,col*2]<-min
        #Actualiza distancia si esta es menor que la que hay en sus coordenadas de
        #la matriz distancias
        if(dist[fil+i,col]>dist[fil,col]+matriz[(fil*2)+i,col*2]) {
          dist[fil+i,col]<-dist[fil,col]+matriz[(fil*2)+i,col*2] 
        }
      }
      #Comprueba si es no es pared, no está visitado y no es el final
      #Si cumple, actualiza la matriz final
      if(matriz[fil*2,(col*2)+i]<10 && visit[fil,col+i]==0 && matriz[fil*2,(col*2)]!=-2) { #Derecha e izquierda
        grafica[fil*2,(col*2)+i]<-min
        #Actualiza distancia si esta es menor que la que hay en sus coordenadas de
        #la matriz distancias
        if(dist[fil,col+i]>dist[fil,col]+matriz[fil*2,(col*2)+i]){
          dist[fil,col+i]<-dist[fil,col]+matriz[fil*2,(col*2)+i]
        }
      }
    }
    
    #Para el "repeat" si llegamos a la habitacion final 
    if(matriz[fil*2,col*2]==-2){
      break
    }
  }
  
  #Le doy a maximo la distancia optima
  max<-min
  dista<-0
  
  repeat{
    visit[fil,col]<-2
    
    for(i in c(-1,1)){
      if(matriz[(fil*2)+i,col*2]<10 && visit[fil+i,col]<2 && matriz[fil*2,(col*2)]!=-1){
        if(dist[fil+i,col]<max && max-matriz[(fil*2)+i,(col*2)]==dist[fil+i,col]){
          max<-dist[fil+i,col]
          r<-fil+i
          c<-col
        }
      }

      if(matriz[fil*2,(col*2)+i]<10 && visit[fil,col+i]<2 && matriz[fil*2,(col*2)]!=-1){
        if(dist[fil,col+i]<max && max-matriz[(fil*2),(col*2)+i]==dist[fil,col+i]){
          max<-dist[fil,col+i]
          r<-fil
          c<-col+i
        }
      }
    }

    grafica[(fil*2)+(r-fil),(col*2)+(c-col)]<--25
    dista<-dista+matriz[(fil*2)+(r-fil),(col*2)+(c-col)]
    fil<-r
    col<-c
    if(matriz[fil*2,(col*2)]==-1){
      break
    }
  }
  
  
  #Actualiza la grafica final con las distancias
  for(i in 1:dim(dist)[1]){
    for(j in 1:dim(dist)[2]){
      grafica[i*2,j*2]<-dist[i,j]
      #Elimina los pasillos no visitados para que no se represente una cuadricula
      if(visit[i,j]==0){
        if(grafica[i*2,(j*2)-1]<=10){ #Izquierda
          grafica[i*2,(j*2)-1]=Inf
        }
        if(grafica[i*2,(j*2)+1]<=10){ #Derecha
          grafica[i*2,(j*2)+1]=Inf
        }
        if(grafica[(i*2)-1,j*2]<=10){ #Abajo
          grafica[(i*2)-1,j*2]=Inf
        }
        if(grafica[(i*2)+1,j*2]<=10){ #Arriba
          grafica[(i*2)+1,j*2]=Inf
        }
      }
      if(visit[i,j]==2){
        grafica[i*2,j*2]<--25
      }
    }
  }
  
  #Modificamos los valores de la matriz para poder representarlos mejor
  #ayudandonos de la opcion "break"
  for(i in 1:dim(matriz)[1]){
    for(j in 1:dim(matriz)[2]){
      #Se cambian los valores de las paredes (los 10´s)
      if(matriz[i,j]==10){ #Pared
        grafica[i,j]=-20
      }
      if(matriz[i,j]==-2){ #Final
        grafica[i,j]=-15
      }
      if(matriz[i,j]==-1){ #Inicio
        grafica[i,j]=--10
      }
      if(grafica[i,j]==Inf){ #No visitado
        grafica[i,j]=-5
      }
    }
  }

  #Se devuelve el valor de la matriz
  return(grafica)
}




#############################################
###### ALGORITMO DIJKSTRA BIDIRECCIONAL #####
#############################################

#Se introducen cinco parametros a la funcion: el laberinto, la fila y la columna de salida y 
#la fil y la columna de llegada.
#La función dijkstraBidimensional devuelve una matriz preprara para ser utilizada con image y la distancia con la que se hace el 
#recorrido
dijkstraBidimensional<-function(datos){
  matriz <- datos[[1]]
  rowSal <- datos[[2]][1]
  colSal <- datos[[2]][2]
  rowDes <- datos[[3]][1]
  colDes <- datos[[3]][2]
  
  size<-(ncol(matriz)-1)/2 #Tamaño de las matrices de habitaciones
  #Crea las matrices de distancias 1 entre las habitaciones inicializandolas a infinito.
  dist1 <-dist2 <-matrix(rep(Inf,size^2),ncol=(size),nrow=(size))
  #Crea las matrices de visitadas 1 inicianizandolas a 0 (tendrán un 1 en las coordenadas que visite)
  visit<-visit2<-matrix(rep(0,size^2),ncol=(size),nrow=(size))
  
  #Los nodos de inicio tendrán distancia 0
  dist1[rowSal+1,colSal+1]<-0
  dist2[rowDes+1,colDes+1]<-0
  
  k<-0
  grafica<-matriz
  while (k!=-1) {
    minS<-minF<-Inf
    filS<-colS<-filF<-colF<-0
    

    for (i in 1:size) {
      for (j in 1:size) {
        if(minS>dist1[i,j]&&visit[i,j]==0){
          minS<-dist1[i,j]
          filS<-i
          colS<-j
        }
        if(minF>dist2[i,j]&&visit2[i,j]==0){
          minF<-dist2[i,j]
          filF<-i
          colF<-j
        }
      }
    }

    visit[filS,colS]<-visit2[filF,colF]<-1
    
  for(i in c(-1,1)){
    if(matriz[(filS*2)+i,colS*2]<10 && visit[filS+i,colS]==0 && matriz[filS*2,(colS*2)]!=-2){
      grafica[(filS*2)+i,colS*2]=minS
      if(dist1[filS+i,colS]>minS+matriz[(filS*2)+i,colS*2]){
        dist1[filS+i,colS]=minS+matriz[(filS*2)+i,colS*2]
      }
    }

    if(matriz[filS*2,(colS*2)+i]<10 && visit[filS,colS+i]==0 && matriz[filS*2,(colS*2)]!=-2){
      grafica[filS*2,(colS*2)+i]=minS
      if(dist1[filS,colS+i]>minS+matriz[filS*2,(colS*2)+i]){
        dist1[filS,colS+i]=minS+matriz[filS*2,(colS*2)+i]
      }
    }

    if(matriz[(filF*2)+i,colF*2]<10 && visit2[filF+i,colF]==0 && matriz[filF*2,(colF*2)]!=-1){
      grafica[(filF*2)+i,colF*2]=minF
      if(dist2[filF+i,colF]>minF+matriz[(filF*2)+i,colF*2]){
        dist2[filF+i,colF]=minF+matriz[(filF*2)+i,colF*2]
      }
    }
    if(matriz[filF*2,(colF*2)+i]<10 && visit2[filF,colF+i]==0 && matriz[filF*2,(colF*2)]!=-1){
      grafica[filF*2,(colF*2)+i]=minF
      if(dist2[filF,colF+i]>minF+matriz[filF*2,(colF*2)+i]){
        dist2[filF,colF+i]=minF+matriz[filF*2,(colF*2)+i]
      }
    }
  }
    k<-k+1

    #Si se llega al nodo doblemente visitado paro y veo cual es ese nodo
    if(filS==filF && colS==colF){
      k<--1
      distancia<-minS+minF
      filaN<-filS
      columnaN<-colS
      cat("la distancia minima: ",distancia,"\n")
    }else{
      if(visit[filS,colS]==1 && visit2[filS,colS]==1){
        k<--1
        distancia<-minS+dist2[filS,colS]
        minF<-dist2[filS,colS]
        filaN<-filS
        columnaN<-colS
        cat("la distancia minima: ",distancia,"\n")
      }
      if(visit[filF,colF]==1 && visit2[filF,colF]==1){
        k<--1
        distancia<-dist1[filF,colF]+minF
        minS<-dist1[filF,colF]
        filaN<-filF
        columnaN<-colF
        cat("la distancia minima: ",distancia,"\n")
      }
    }
  }
  maxS<-minS
  maxF<-minF
  
  visit3<-visit4<-matrix(rep(0,size*size),ncol = size,nrow=size)
  filS<-filF<-filaN
  colS<-colF<-columnaN
  dista<-0
  repeat{
    visit3[filS,colS]<-visit4[filF,colF]<-1
    
    for(i in c(-1,1)){
      if(matriz[(filS*2)+i,colS*2]<10 && visit3[filS+i,colS]==0 && matriz[filS*2,(colS*2)]!=-1){
        if(dist1[filS+i,colS]<maxS){
          if(maxS-matriz[(filS*2)+i,(colS*2)]==dist1[filS+i,colS]){
            maxS<-dist1[filS+i,colS]
            fS<-filS+i
            cS<-colS
          }
        }
      }
      if(matriz[filS*2,(colS*2)+i]<10 && visit3[filS,colS+i]==0 && matriz[filS*2,(colS*2)]!=-1){
        if(dist1[filS,colS+i]<maxS){
          if(maxS-matriz[(filS*2),(colS*2)+i]==dist1[filS,colS+i]){
            maxS<-dist1[filS,colS+i]
            fS<-filS
            cS<-colS+i
          }
        }
      }
  
      if(matriz[(filF*2)+i,colF*2]<10 && visit4[filF+i,colF]==0 && matriz[filF*2,(colF*2)]!=-2){
        if(dist2[filF+i,colF]<maxF ){
          if( maxF-matriz[(filF*2)+i,(colF*2)]==dist2[filF+i,colF]){
            maxF<-dist2[filF+i,colF]
            fD<-filF+i
            cD<-colF
          }
        }
      }
      if(matriz[filF*2,(colF*2)+i]<10 && visit4[filF,colF+i]==0 && matriz[filF*2,(colF*2)]!=-2){
        if(dist2[filF,colF+i]<maxF){
          if(maxF-matriz[(filF*2),(colF*2)+i]==dist2[filF,colF+i]){
            maxF<-dist2[filF,colF+i]
            fD<-filF
            cD<-colF+i
          }
        }
      }
    }
    grafica[(filS*2)+(fS-filS),(colS*2)+(cS-colS)]<-grafica[(filF*2)+(fD-filF),(colF*2)+(cD-colF)]<--30
    dista<-dista+matriz[(filS*2)+(fS-filS),(colS*2)+(cS-colS)]+matriz[(filF*2)+(fD-filF),(colF*2)+(cD-colF)]
    filS<-fS
    colS<-cS
    filF<-fD
    colF<-cD
    if(matriz[filS*2,(colS*2)]==-1 && matriz[filF*2,(colF*2)]==-2){
      break
    }
  }
  
  for(i in 1:size){
    for(j in 1:size){
      if(dist1[i,j]==Inf && dist2[i,j]==Inf){
        grafica[i*2,j*2]<-Inf
      }else{
        if(dist1[i,j]<Inf){
          grafica[i*2,j*2]<-dist1[i,j]
        }
        if(dist2[i,j]<Inf){
          grafica[i*2,j*2]<-dist2[i,j]
        }
      }
      
      if(visit[i,j]==0 && visit2[i,j]==0){
        if(grafica[i*2,(j*2)+1]<=10){
          grafica[i*2,(j*2)+1]=Inf
        }
        if(grafica[(i*2)+1,j*2]<=10){
          grafica[(i*2)+1,j*2]=Inf
        }
        if(grafica[i*2,(j*2)-1]<=10){
          grafica[i*2,(j*2)-1]=Inf
        }
        if(grafica[(i*2)-1,j*2]<=10){
          grafica[(i*2)-1,j*2]=Inf
        }
      }
      if(visit3[i,j]==1){
        grafica[i*2,j*2]=-30
      }
      if(visit4[i,j]==1){
        grafica[i*2,j*2]=-30
      }
    }
  }
  #ponemos el nodo donde se juntan los dos
  grafica[filaN*2,columnaN*2]<--25
  for(i in 1:dim(matriz)[1]){
    for(j in 1:dim(matriz)[2]){
      if(matriz[i,j]==10){
        grafica[i,j]=-20
      }
      if(matriz[i,j]==-2){
        grafica[i,j]=-15
      }

      if(matriz[i,j]==-1){
        grafica[i,j]=-10
      }
      if(grafica[i,j]==Inf){
        grafica[i,j]=-5
      }
    }
  }
  return(grafica)
}


#######################################
######   ALGORITMO A ESTRELLA   #######
#######################################

aEstrella<-function(datos){
  matriz <- datos[[1]]
  rowSal <- datos[[2]][1]
  colSal <- datos[[2]][2]
  rowDes <- datos[[3]][1]
  colDes <- datos[[3]][2]
  size<-(ncol(matriz)-1)/2
  #matriz con las distancias
  dist<-matrix(rep(Inf,size*size),ncol = size,nrow=size)
  #matriz booleana en la cual los 1 signfican que esa habitación esta visitada
  visit<-heuristica<-matrix(rep(0,size*size),ncol = size,nrow=size)
  #añadimos el punto de salida
  dist[rowSal+1,colSal+1]<-0
  dista<-0
  k<-0
  #creamos la matriz de calor, en un principio es una copia del laberinto pero la iremos modificando por el camino
  grafica<-matriz
  #creamos la heuristica
  for (i in 1:size) {
    for(j in 1:size){
      heuristica[i,j]<-abs(i-rowDes+1)+abs(j-colDes+1) 
    }
  }
  while (k!=-1) {
    min<-Inf
    fil<-col<-0
    #buscamos la habitacion que menor distancia tiene, nos va a coger el menor por filas 
    for (i in 1:size) {
      for (j in 1:size) {
        if(min>dist[i,j]+heuristica[i,j]*2 && visit[i,j]==0){
          min<-dist[i,j]+heuristica[i,j]*2
          nodo<-dist[i,j]
          fil<-i
          col<-j
        }
      }
    }
    
    #marcamos ha habitacion menor distancia encontrada como visitado
    visit[fil,col]<-1
    
    #coprobamos si podemos movernos hacia arriba, abajo, derecha e izquierda de la habitacion menor,
    #para poder actualizar la distancia tenemos que comprobar que para donde queremos ir hay un pasillo,
    #que la siguiente habitacion no esta visitada y que no estamos en el destino
    if(matriz[(fil*2)+1,col*2]<10 && visit[fil+1,col]==0 && matriz[fil*2,(col*2)]!=-2){
      #por el pasillo que pasamos le ponemos la distancia de la habitación menor
      grafica[(fil*2)+1,col*2]=nodo
      #comprobamos que la distancia que tiene esa habitacion en ese momento es mayor que la que vamos a escribir,
      #si esto ocurre sobreescribimos la nueva distancia
      if(dist[fil+1,col]>nodo+matriz[(fil*2)+1,col*2]){
        dist[fil+1,col]=nodo+matriz[(fil*2)+1,col*2]
      }
    }
    #lo mismo que hemos hecho para derecha izquierda pero pero para arriba y abajo
    if(matriz[fil*2,(col*2)+1]<10 && visit[fil,col+1]==0 && matriz[fil*2,(col*2)]!=-2){
      grafica[fil*2,(col*2)+1]=nodo
      if(dist[fil,col+1]>nodo+matriz[fil*2,(col*2)+1]){
        dist[fil,col+1]=nodo+matriz[fil*2,(col*2)+1]
      }
    }
    if(matriz[(fil*2)-1,col*2]<10 && visit[fil-1,col]==0 && matriz[fil*2,(col*2)]!=-2){
      #por el pasillo que pasamos le ponemos la distancia de la habitación menor
      grafica[(fil*2)-1,col*2]=nodo
      #comprobamos que la distancia que tiene esa habitacion en ese momento es mayor que la que vamos a escribir,
      #si esto ocurre sobreescribimos la nueva distancia
      if(dist[fil-1,col]>nodo+matriz[(fil*2)-1,col*2]){
        dist[fil-1,col]=nodo+matriz[(fil*2)-1,col*2]
      }
    }
    #lo mismo que hemos hecho para derecha izquierda pero pero para arriba y abajo
    if(matriz[fil*2,(col*2)-1]<10 && visit[fil,col-1]==0 && matriz[fil*2,(col*2)]!=-2){
      grafica[fil*2,(col*2)-1]=nodo
      if(dist[fil,col-1]>nodo+matriz[fil*2,(col*2)-1]){
        dist[fil,col-1]=nodo+matriz[fil*2,(col*2)-1]
      }
    }
    k<-k+1
    #paramos si llegamos al nodo destino e imprimimos la distancia final con la que llegamos a esta habitacion
    if(matriz[fil*2,col*2]==-2){
      k<--1
      cat("la distancia minima: ",nodo,"\n")
    }
  }
  max<-nodo
  #matriz booleana en la cual los 1 signfican que esa habitación esta visitada

  
  while(k!=1){
    #marcamos ha habitacion menor distancia encontrada como visitado
    visit[fil,col]<-2
    
    #coprobamos si podemos movernos hacia arriba, abajo, derecha e izquierda de la habitacion menor,
    #para poder actualizar la distancia tenemos que comprobar que para donde queremos ir hay un pasillo,
    #que la siguiente habitacion no esta visitada y que no estamos en el destino
    if(matriz[(fil*2)+1,col*2]<10 && visit[fil+1,col]<2 && matriz[fil*2,(col*2)]!=-1){
      #comprobamos que la distancia que tiene esa habitacion en ese momento es mayor que la que vamos a escribir,
      #si esto ocurre sobreescribimos la nueva distancia
      if(dist[fil+1,col]<max && max-matriz[(fil*2)+1,(col*2)]==dist[fil+1,col]){
        max<-dist[fil+1,col]
        f<-fil+1
        c<-col
      }
    }
    #lo mismo que hemos hecho para derecha izquierda pero pero para arriba y abajo
    if(matriz[fil*2,(col*2)+1]<10 && visit[fil,col+1]<2 && matriz[fil*2,(col*2)]!=-1){
      if(dist[fil,col+1]<max&& max-matriz[(fil*2),(col*2)+1]==dist[fil,col+1]){
        max<-dist[fil,col+1]
        f<-fil
        c<-col+1
      }
    }
    if(matriz[(fil*2)-1,col*2]<10 && visit[fil-1,col]<2 && matriz[fil*2,(col*2)]!=-1){
      #comprobamos que la distancia que tiene esa habitacion en ese momento es mayor que la que vamos a escribir,
      #si esto ocurre sobreescribimos la nueva distancia
      if(dist[fil-1,col]<max && max-matriz[(fil*2)-1,(col*2)]==dist[fil-1,col]){
        max<-dist[fil-1,col]
        f<-fil-1
        c<-col
      }
    }
    #lo mismo que hemos hecho para derecha izquierda pero pero para arriba y abajo
    if(matriz[fil*2,(col*2)-1]<10 && visit[fil,col-1]<2 && matriz[fil*2,(col*2)]!=-1){
      if(dist[fil,col-1]<max&& max-matriz[(fil*2),(col*2)-1]==dist[fil,col-1]){
        max<-dist[fil,col-1]
        f<-fil
        c<-col-1
      }
    }
    grafica[(fil*2)+(f-fil),(col*2)+(c-col)]<--25
    dista<-dista+matriz[(fil*2)+(f-fil),(col*2)+(c-col)]
    fil<-f
    col<-c
    if(matriz[fil*2,(col*2)]==-1){
      k<-1
      cat("la distancia camino: ",dista,"\n")
    }
  }
  
  #actualizamos la matriz de calores con las distancias
  for(i in 1:size){
    for(j in 1:size){
      grafica[i*2,j*2]<-dist[i,j]
      
      #quitamos los pasillos de las size no visitadas
      if(visit[i,j]==0){
        if(grafica[i*2,(j*2)+1]<=10){
          grafica[i*2,(j*2)+1]=Inf
        }
        if(grafica[(i*2)+1,j*2]<=10){
          grafica[(i*2)+1,j*2]=Inf
        }
        if(grafica[i*2,(j*2)-1]<=10){
          grafica[i*2,(j*2)-1]=Inf
        }
        if(grafica[(i*2)-1,j*2]<=10){
          grafica[(i*2)-1,j*2]=Inf
        }
      }
      if(visit[i,j]==2){
        grafica[i*2,j*2]<--25
      }
    }
  }
  #cambiamos algunos parametros de la matriz para poder imprimirla con colores distintos
  for(i in 1:dim(matriz)[1]){
    for(j in 1:dim(matriz)[2]){
      #actualizamos los valores de las paredes
      if(matriz[i,j]==10){
        grafica[i,j]=-20
      }
      #actualizamos el valor del destino
      if(matriz[i,j]==-2){
        grafica[i,j]=-15
      }
      #actualizamos el valor de la salida
      if(matriz[i,j]==-1){
        grafica[i,j]=-10
      }
      #actualizamos el valor de los psillos y habitaciones no visitados
      if(grafica[i,j]==Inf){
        grafica[i,j]=-5
      }
    }
  }
  #devolvemos el valor de la matriz a imprimir
  return(grafica)
}
#=================================================
#                  EJECUCION 
#=================================================

datos<-generaLaberinto(60,1,40,999)


# #=========Dijkstra sin cola de prioridad============
# ptm_dijkstrasin <- proc.time() #Guardo el tiempo justo antes de la ejecucion
# heat1<-dijkstraSinCola(datos) #Guardo en heat1 la matriz con las distancias de los caminos
# time_dijkstrasin<-proc.time()[3]-ptm_dijkstrasin[3] #Restamos el tiempo actual menos el guardado anteriormente
# cat("El tiempo que usa Dijkstra sin cola de prioridad es: ", time_dijkstrasin)
# 
# grad1 <- colorRampPalette(c("#ffffb2", "#fd8d3c", "#bd0026")) #Paleta de colores calidos
# modmax1<-ceiling(max(heat1)/10) #Cálculo que uso para encontrar el maximo de las distancias y asi poder representar mejor los colores
# breaks <- c(-26, -21, -18,-13, -8, seq(-0.1,modmax1*10, by=10)) #Puntos de corte que utilizo para pintar los diferentes colores
# image(heat1, breaks=breaks, col = c("yellow", "black", "red","turquoise1", "white", grad1(modmax1)),axe=FALSE) #Pinto la matriz


# #=========Dijkstra bidireccional============
# ptm_dijkstrasin <- proc.time() #Guardo el tiempo justo antes de la ejecucion
# heat1<-dijkstraBidimensional(datos) #Guardo en heat1 la matriz con las distancias de los caminos
# time_dijkstrasin<-proc.time()[3]-ptm_dijkstrasin[3] #Restamos el tiempo actual menos el guardado anteriormente
# cat("El tiempo que usa Dijkstra sin cola de prioridad es: ", time_dijkstrasin)
# 
# grad1 <- colorRampPalette(c("#ffffb2", "#fd8d3c", "#bd0026")) #Paleta de colores calidos
# modmax1<-ceiling(max(heat1)/10) #Cálculo que uso para encontrar el maximo de las distancias y asi poder representar mejor los colores
# breaks <- c(-31,-26,-21,-18,-13,-8, seq(-0.1,modmax1*10, by=10)) #Puntos de corte que utilizo para pintar los diferentes colores
# image(heat1, breaks=breaks, col = c("green","yellow","black","red","turquoise1","white", grad1(modmax1)),axe=FALSE) #Pinto la matriz

#=========Dijkstra bidireccional============
ptm_dijkstrasin <- proc.time() #Guardo el tiempo justo antes de la ejecucion
heat1<-aEstrella(datos) #Guardo en heat1 la matriz con las distancias de los caminos
time_dijkstrasin<-proc.time()[3]-ptm_dijkstrasin[3] #Restamos el tiempo actual menos el guardado anteriormente
cat("El tiempo que usa Dijkstra sin cola de prioridad es: ", time_dijkstrasin)

grad1 <- colorRampPalette(c("#ffffb2", "#fd8d3c", "#bd0026")) #Paleta de colores calidos
modmax1<-ceiling(max(heat1)/10) #Cálculo que uso para encontrar el maximo de las distancias y asi poder representar mejor los colores
breaks <- c(-26,-21,-18,-13,-8, seq(-0.1,modmax1*10, by=10)) #Puntos de corte que utilizo para pintar los diferentes colores
image(heat1, breaks=breaks, col = c("yellow","black","red","turquoise1","white", grad1(modmax1)),axe=FALSE) #Pinto la matriz

