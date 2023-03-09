#=========================
#        CQFidalgo
#      Laboratorio 4
#   Algoritmo Dijkstra
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
    mat[row,col]<-s2$randi()%%10 #valores de pesos
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

#Algoritmo de Dijkstra sin cola
#Algoritmo que encuentra el camino más corto entre dos habitaciones (nodos),
#para ello se ayuda de una matriz de distancias (solo contiene habitaciones)
#que en un principio tiene valores infinitos y va actualizando según va 
#incluyendo esas coordenadas en la matriz final, una de habitaciones visitadas, 
#que toma valores 0 y 1 (inicialmente a 0, toma valor 1 si se visita esa habitacion)
#y una matriz final que es la que se puede representar graficamente.

#Parametros:
#datos: matriz con los pesos generados aleatoriamente.
dijkstraSinCola <- function(datos) { #Implementacion sin cola
  matriz <- datos[[1]]
  rowSal <- datos[[2]][1]
  colSal <- datos[[2]][2]
  size<-(ncol(matriz)-1)/2 #Tamaño de las matrices de habitaciones
  #Crea la matriz de distancias entre las habitaciones inicializandolas a infinito.
  dist <-matrix(rep(Inf,size^2),ncol=(size),nrow=(size))
  #Crea la matriz de visitadas inicianizandola a 0 (tendrá un 1 en las coordenadas que visite)
  visit<-matrix(rep(0,size^2),ncol=(size),nrow=(size))
  dist[rowSal+1,colSal+1]<-0 #El punto de salida tendrá distancia 0
  grafica<-matriz #Crea la matriz final que podremos representar graficamente
  
  repeat{#Buscamos el camino minimo hasta encontrarlo
    min<-Inf #El menor tiene un valor muy grande inicialmente
    fil<-0
    col<-0
    
    #Busca el valor mas pequeño de la matriz distancias que no esté visitado
    for(i in 1:dim(dist)[1]){
      for(j in 1:dim(dist)[2]){
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
    if(matriz[fil*2,(col*2)]==-2) {
      break
    }
  }
  #Actualiza la matriz resultante para poder dibujarla
  grafica<-actualizar(1,dist,grafica,visit,matriz)
  #Devuelve la matriz con las distancias de los nodos marcados como visitados
  #preparada para ser dibujada
  return(grafica)
}

#Algoritmo de Dijkstra con cola
#La implementacion es muy parecida a la de sin colas pero en este caso solo las
#habitaciones del conjunto frontera tendrán valor en la matriz distancias, el
#resto tendran valor NA.
dijkstraConCola<-function(datos){
  matriz <- datos[[1]]
  rowSal <- datos[[2]][1]
  colSal <- datos[[2]][2]
  size<-(ncol(matriz)-1)/2 #Tamaño de las matrices de habitaciones
  #Crea la matriz de distancias entre las habitaciones inicializandolas a NA.
  dist<-matrix(rep(NA,size^2),ncol = size,nrow=size)
  #Crea la matriz de visitadas inicianizandola a 0 (tendrá un 1 en las coordenadas que visite)
  visit<-matrix(rep(0,size^2),ncol = size, nrow=size)
  dist[rowSal+1,colSal+1]<-0 #El punto de salida tendrá distancia 0
  grafica<-matriz #Crea la matriz final que podremos representar graficamente
  
  repeat{ #Busca el camino minimo hasta encontrarlo
    min<-Inf #El menor tiene un valor muy grande inicialmente
    fil<-0
    col<-0
    
    #Busca el valor mas pequeño de la matriz distancias
    min<-min(dist,na.rm = T)
    coor<-which(dist==min,arr.ind=TRUE)
    fil<-coor[1,1]
    col<-coor[1,2]
    
    #Introduce un 1 en las coordenadas de la matriz de visitados del menor encontrado.
    visit[fil,col]<-1
    dist[fil,col]<-NA #Si es NA no es conjunto frontera
    for(i in c(-1,1)){ #Se mueve entre los cuatro pasillos adyacentes
      #Comprueba si es no es pared, no está visitado y no es el final
      if(matriz[(fil*2)+i,col*2]<10 && visit[fil+i,col]==0 && matriz[fil*2,(col*2)]!=-2){ #Arriba y abajo
        grafica[(fil*2)+i,col*2]<-min
        #Si no es frontera, introduzco la distancia correspondiente
        if(is.na(dist[fil+i,col])){
          dist[fil+i,col]<-min+matriz[(fil*2)+i,col*2]
          grafica[(fil*2)+i*2,col*2]<-dist[fil+i,col]
        }else{#Si es frontera comparo la distancia anterior y actualizo si es menor que la actual
          if(dist[fil+i,col]>min+matriz[(fil*2)+i,col*2]){
            dist[fil+i,col]<-min+matriz[(fil*2)+i,col*2]
            grafica[(fil*2)+i*2,col*2]<-dist[fil+i,col]
          }
        }
      }
      if(matriz[fil*2,(col*2)+i]<10 && matriz[fil*2,(col*2)]!=-2 && visit[fil,col+i]==0){ #Derecha e izquierda
        grafica[fil*2,(col*2)+i]<-min
        #Si no es frontera, introduzco la distancia correspondiente
        if(is.na(dist[fil,col+i])){
          dist[fil,col+i]<-min+matriz[fil*2,(col*2)+i]
          grafica[fil*2,(col*2)+i*2]<-dist[fil,col+i]
        }else{#Si es frontera comparo la distancia anterior y actualizo si es menor que la actual
          if(dist[fil,col+i]>min+matriz[fil*2,(col*2)+i]){
            dist[fil,col+i]<-min+matriz[fil*2,(col*2)+i]
            grafica[fil*2,(col*2)+i*2]<-dist[fil,col+i]
          }
        }
      }
    }
    #Para el "repeat" si llegamos a la habitacion final 
    if(matriz[fil*2,(col*2)]==-2){
      break
    }
  }
  #Actualiza la matriz resultante para poder dibujarla
  
  grafica<-actualizar(2,dist,grafica,visit,matriz)
  
  #Devuelve la matriz con las distancias de los nodos marcados como visitados
  #preparada para ser dibujada
  return(grafica)
}


actualizar<-function(a,dist,grafica,visit,matriz){
  #Actualiza la grafica final con las distancias
  for(i in 1:dim(dist)[1]) {
    for(j in 1:dim(dist)[2]) {
      if(a==1){grafica[i*2,j*2]<-dist[i,j]}
      #Elimina los pasillos no visitados para que no se represente una cuadricula
      if(visit[i,j]==0) { #Valor 0 si no está visitado
        if(grafica[i*2,(j*2)-1]<=10){ #Izquierda
          grafica[i*2,(j*2)-1]<-Inf
        }
        if(grafica[i*2,(j*2)+1]<=10){ #Derecha
          grafica[i*2,(j*2)+1]<-Inf
        }
        if(grafica[(i*2)-1,j*2]<=10){ #Abajo
          grafica[(i*2)-1,j*2]<-Inf
        }
        if(grafica[(i*2)+1,j*2]<=10){ #Arriba
          grafica[(i*2)+1,j*2]<-Inf
        }
        if(a==2){ #Para la funcion Dijkstra con cola
          if(grafica[i*2,j*2]==0){ #Si tiene un 0 es que no está visitado
            grafica[i*2,j*2]<-Inf
          }
        }
      }
    }
  }
  #Modificamos los valores de la matriz para poder representarlos mejor
  #ayudandonos de la opcion "break"
  for(i in 1:dim(matriz)[1]){
    for(j in 1:dim(matriz)[2]){
      if(matriz[i,j]==10){ #Pared
        grafica[i,j]<--0.7
      }
      if(matriz[i,j]==-2){ #Final
        grafica[i,j]<--2
      }
      if(matriz[i,j]==-1){ #Inicio
        grafica[i,j]<--1
      }
      if(grafica[i,j]==Inf){ #No visitada
        grafica[i,j]<--0.5
      }
    }
  }
  #Devuelve la matriz preparada para representada graficamente
  return(grafica)
}


#=================================================
#                  EJECUCION 
#=================================================

datos<-generaLaberinto(100,1,671,305)


#=========Dijkstra sin cola de prioridad============
ptm_dijkstrasin <- proc.time() #Guardo el tiempo justo antes de la ejecucion
heat1<-dijkstraSinCola(datos) #Guardo en heat1 la matriz con las distancias de los caminos
time_dijkstrasin<-proc.time()[3]-ptm_dijkstrasin[3] #Restamos el tiempo actual menos el guardado anteriormente
cat("El tiempo que usa Dijkstra sin cola de prioridad es: ", time_dijkstrasin)

grad1 <- colorRampPalette(c("#ffffb2", "#fd8d3c", "#bd0026")) #Paleta de colores calidos
modmax1<-ceiling(max(heat1)/10) #Cálculo que uso para encontrar el maximo de las distancias y asi poder representar mejor los colores
breaks <- c(-5, -1.5, -0.8, -0.6, seq(-0.1,modmax1*10, by=10)) #Puntos de corte que utilizo para pintar los diferentes colores
image(heat1, breaks=breaks, col = c("green", "red", "black", "blue", grad1(modmax1)),axe=FALSE) #Pinto la matriz



#=========Dijkstra con cola de prioridad============
ptm_dijkstracon <- proc.time() #Guardo el tiempo justo antes de la ejecucion
heat2<-dijkstraConCola(datos) #Guardo en heat2 la matriz con las distancias de los caminos
time_dijkstracon<-proc.time()[3]-ptm_dijkstracon[3] #Restamos el tiempo actual menos el guardado anteriormente
cat("El tiempo que usa Dijkstra con cola de prioridad es: ", time_dijkstracon)

grad2 <- colorRampPalette(c("#cacefa", "#8083bf", "#2b2e69")) #Paleta de colores frios
modmax2<-ceiling(max(heat2)/10) #Cálculo que uso para encontrar el maximo de las distancias y asi poder representar mejor los colores
breaks <- c(-5, -1.5, -0.8, -0.6, seq(-0.1,modmax2*10, by=10)) #Puntos de corte que utilizo para pintar los diferentes colores
image(heat2, breaks=breaks, col = c("green", "red", "black", "brown", grad2(modmax2)),axe=FALSE) #Pinto la matriz
