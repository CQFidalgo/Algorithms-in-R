#=======================
#       FUNCIONES
#=======================

#=======================
#    FUNCIONES TAREA 1
#=======================

library(reshape2)
library(plot.matrix)
library(viridis)
set.seed(12345)

funcion1<-function(x,y,n) {
  res<-c()
  for(i in x) {
    fil<-c()
    for(j in y) {
      fil <- append(fil,cos((i*i+j*j)*12)/(2*((i*i+j*j)*6.28+1)))}
    res<-rbind(res, fil)
  }
  rownames(res) <- x #titulos de las filas son los valores de x
  colnames(res) <- y #titulos de las columnas son los valores de y
  
  res<-cbind(-1,res,-1)
  res<-rbind(-1,res,-1)
  return(res)
}
hc<-function(p){ #p: numero de caminos
  max.local<-c()
  for (i in 1:p) { #repetimos p veces para hacer p caminos
    p<-p+1
    ini<-c(sample(1:n, 1), sample(1:n, 1)) #Elegimos el punto de inicio
    i<-ini[1]+1 #Añade 1 porque la matriz ahora está desplazada en una unidad
    j<-ini[2]+1
    a <- i
    b <- j
    points(x[i - 1],y[j - 1],col = "green",pch = 15,cex = 50 / n)
    
    repeat {
      cont = 0
      if (matrix[a, b] < matrix[i, j + 1]) { #derecha
        a <- i
        b <- j + 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i, j - 1]) { #izquierda
        a <- i
        b <- j - 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i + 1, j]) { #abajo
        a <- i + 1
        b <- j
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i - 1, j]) { #arriba
        a <- i - 1
        b <- j
        cont <- cont + 1
      }
      if (cont == 0) {
        max.local <- append(max.local,matrix[a,b])
        points(x[a - 1],y[b - 1],col = "red",pch = 15,cex = 80 / n)
        break
      } else{
        i <- a
        j <- b
        points(x[a - 1],y[b - 1],col = "black",pch = 15,cex = 50 / n)
      }
    }
  }
  return (max.local)
}

bootstrap <- function(p) {
  maximo <- max(matrix)
  m2 = matrix(0, ncol = 100, nrow = p)
  for (i in 1:p) {
    for (j in 1:100) {
      resultado = hc(p)
      if (maximo == max(resultado)) { #Comparamos el maximo de Hill climbing con el real.
        m2[i, j] = 1 
      }
    }
  }
  mean(m2)
  return (mean(m2)) #Devolvemos el promedio de veces que consigue el maximo para el p dado. 
}

pminimo<-function(p){
  for(t in 1:1000){
    if(round(bootstrap(p),2)<0.98){ 
      print(p)
      print(round(bootstrap(p),2))
      print("Es menor")
      p<-p+1
    }
    else{
      print(p)
      print(round(bootstrap(p),2))
      break
    }
  }
  return(p)
}


#==========================
#    FUNCIONES TAREA 2.2
#==========================


funcion2<-function(x,y,n) {
  res<-c()
  for(i in x) {
    fil<-c()
    for(j in y) {
      fil <- append(fil,(i/(i*i+1) - j/(j*j+1) + 2*(sqrt(i*i+j*j)-1)/((sqrt(i*i+j*j)-1)*(sqrt(i*i+j*j)-1)+1)))}
    res<-rbind(res, fil)
  }
  rownames(res) <- x #titulos de las filas son los valores de x
  colnames(res) <- y #titulos de las columnas son los valores de y
  
  res<-cbind(-1,res,-1)
  res<-rbind(-1,res,-1)
  return(res)
}

hc2.2<-function(p){ #p: numero de caminos
  max.local<-c()
  for (i in 1:p) { #repetimos p veces para hacer p caminos
    p<-p+1
    ini<-c(sample(1:n, 1), sample(1:n, 1)) #Elegimos el punto de inicio
    i<-ini[1]+1 #Añade 1 porque la matriz ahora está desplazada en una unidad
    j<-ini[2]+1
    a <- i
    b <- j
    
    repeat {
      cont = 0
      if (matrix[a, b] < matrix[i, j + 1]) { #derecha
        a <- i
        b <- j + 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i, j - 1]) { #izquierda
        a <- i
        b <- j - 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i + 1, j]) { #abajo
        a <- i + 1
        b <- j
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i - 1, j]) { #arriba
        a <- i - 1
        b <- j
        cont <- cont + 1
      }
      if (cont == 0) {
        max.local <- append(max.local,matrix[a,b])
        break
      } else{
        i <- a
        j <- b
      }
    }
  }
  return (max(max.local))
}



#==========================
#    FUNCIONES TAREA 2.3
#==========================


calculo<-function(i,j){
  resultado<-(i/(i*i+1) - j/(j*j+1) + 2*(sqrt(i*i+j*j)-1)/((sqrt(i*i+j*j)-1)*(sqrt(i*i+j*j)-1)+1))
  
  return(resultado)
}

funcion3<-function(x,y,n) {
  res<-c()
  res<-matrix(-100, nrow = n, ncol = n)
  rownames(res) <- x #titulos de las filas son los valores de x
  colnames(res) <- y #titulos de las columnas son los valores de y
  
  res<-cbind(-1000,res,-1000)
  res<-rbind(-1000,res,-1000)
  return(res)
}

hc2.3<-function(p){ #p: numero de caminos
  max.local<-c()
  for (i in 1:p) { #repetimos p veces para hacer p caminos
    p<-p+1
    ini<-c(sample(1:n, 1), sample(1:n, 1)) #Elegimos el punto de inicio
    i<-ini[1]+1 #Añade 1 porque la matriz ahora está desplazada en una unidad
    j<-ini[2]+1
    a <- i
    b <- j
    repeat {
      cont = 0
      if(matrix[i,j]==-100){
        (matrix[i,j]<-calculo(x[i-1],y[j-1]))}
      
      if(matrix[i+1,j]==-100){
        (matrix[i+1,j]<-calculo(x[i+1-1],y[j-1]))
      }
      if(matrix[i-1,j]==-100){
        (matrix[i-1,j]<-calculo(x[i-1-1],y[j-1]))
      }
      if(matrix[i,j+1]==-100){
        (matrix[i,j+1]<-calculo(x[i-1],y[j+1-1]))
      }
      if(matrix[i,j-1]==-100){
        (matrix[i,j-1]<-calculo(x[i-1],y[j-1-1]))
      }
      if (matrix[a, b] < matrix[i, j + 1]) { #derecha
        a <- i
        b <- j + 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i, j - 1]) { #izquierda
        a <- i
        b <- j - 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i + 1, j]) { #abajo
        a <- i + 1
        b <- j
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i - 1, j]) { #arriba
        a <- i - 1
        b <- j
        cont <- cont + 1
      }
      if (cont == 0) {
        max.local <- append(max.local,matrix[a, b] )
        break
      } else{
        i <- a
        j <- b
      }
    }
  }
  return (max(max.local))
}


#==========================
#    FUNCIONES TAREA 2.4
#==========================

hc2.4<-function(p){ #p: numero de caminos
  m0<-matrix(0, nrow = n+2, ncol = n+2)
  max.local<-c()
  for (i in 1:p) { #repetimos p veces para hacer p caminos
    p<-p+1
    ini<-c(sample(1:n, 1), sample(1:n, 1)) #Elegimos el punto de inicio
    i<-ini[1]+1 #Añade 1 porque la matriz ahora está desplazada en una unidad
    j<-ini[2]+1
    a <- i
    b <- j
    #points(x[i - 1],y[j - 1],col = "green",pch = 15,cex = 50 / n)
    
    repeat {
      cont = 0
      if (matrix[a, b] < matrix[i, j + 1]) { #derecha
        a <- i
        b <- j + 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i, j - 1]) { #izquierda
        a <- i
        b <- j - 1
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i + 1, j]) { #abajo
        a <- i + 1
        b <- j
        cont <- cont + 1
      }
      if (matrix[a, b] < matrix[i - 1, j]) { #arriba
        a <- i - 1
        b <- j
        cont <- cont + 1
      }
      if (cont == 0) {
        max.local <- append(max.local,matrix[a,b])
        #points(x[a - 1],y[b - 1],col = "red",pch = 15,cex = 80 / n)
        break
      } else{
        if(m0[a,b]==1){
          #points(x[a - 1],y[b - 1],col = "grey",pch = 15,cex = 50 / n)
          break
        }else{
          i <- a
          j <- b
          #points(x[a - 1],y[b - 1],col = "black",pch = 15,cex = 50 / n)
          m0[a,b]<-1
        }
      }
    }
  }
  return (max(max.local))
}






#========================
#    EJECUCION TAREA 1
#========================


#=================Crear Matriz===========================
n<-200
x<-seq(-1,2,length.out=n)
y<-x

ptm_matrix1 <- proc.time()
matrix<-funcion1(x,y,n)
time_matrix1<-proc.time()[3]-ptm_matrix1[3]
cat("El tiempo necesario para crear la matriz es: ", time_matrix1)

#=================Dibujar Matriz=========================
datos<-melt(matrix)
colores<-assignColors(datos$value,col=magma,breaks=100)
plot(datos$Var1,datos$Var2,ylim=rev(range(y)),col=colores,pch=15,cex=50/n)

#=================Hill Climbing=========================
ptm_hc1 <- proc.time()
maximo_hc1 <- max(hc(5))
cat("El máximo de la función con el recorrido de hill climbing es: ", maximo_hc1)
time_hc1<-proc.time()[3]-ptm_hc1[3]
cat("El tiempo de la función con el recorrido de hill climbing es: ", time_hc1)

#============Búsqueda del numero minimo de caminos=======
ptm_minimo <- proc.time()
pminimo<-pminimo(1)
cat("El numero minimo de caminos para conseguir el maximo el 99.9% de las veces es: ", pminimo)
time_minimo<-proc.time()[3]-ptm_minimo[3]
cat("El tiempo que hemos tardado en calcular el numero de caminos necesario es: ", time_minimo)



#===========================
#    EJECUCION TAREA 2.2
#===========================

#=================Crear Matriz===========================
n<-2000
x<-seq(-6,6,length.out=n)
y<-x
p<-50

ptm_matrix2.2 <- proc.time()
matrix<-funcion2(x,y,n)
time_matrix2.2<-proc.time()[3]-ptm_matrix2.2[3]
cat("El tiempo necesario para crear la matriz es: ", time_matrix2.2)


#=================Hill Climbing=========================
ptm_hc2.2 <- proc.time()
maximo_hc2.2 <- hc2.2(p)
cat("El máximo de la función con el recorrido de hill climbing es: ", maximo_hc2.2)
time_hc2.2<-proc.time()[3]-ptm_hc2.2[3]
cat("El tiempo de la función con el recorrido de hill climbing es: ", time_hc2.2)



#===========================
#    EJECUCION TAREA 2.3
#===========================

#=================Crear Matriz===========================
n<-2000
x<-seq(-6,6,length.out=n)
y<-x
p<-5000

ptm_matrix2.3 <- proc.time()
matrix<-funcion3(x,y,n)
time_matrix2.3<-proc.time()[3]-ptm_matrix2.3[3]
cat("El tiempo necesario para crear la matriz es: ", time_matrix2.3)

#=================Hill Climbing=========================
ptm_hc2.3 <- proc.time()
maximo_hc2.3 <- hc2.3(p)
cat("El máximo de la función con el recorrido de hill climbing es: ", maximo_hc2.3)
time_hc2.3<-proc.time()[3]-ptm_hc2.3[3]
cat("El tiempo de la función con el recorrido de hill climbing es: ", time_hc2.3)



#===========================
#    EJECUCION TAREA 2.4
#===========================

#=================Crear Matriz===========================
n<-2000
x<-seq(-6,6,length.out=n)
y<-x
p<-1000

ptm_matrix2.4 <- proc.time()
matrix<-funcion2(x,y,n)
time_matrix2.4<-proc.time()[3]-ptm_matrix2.4[3]
cat("El tiempo necesario para crear la matriz es: ", time_matrix2.4)

#=================Hill Climbing=========================
ptm_hc2.4 <- proc.time()
maximo_hc2.4 <- hc2.4(p)
cat("El máximo de la función con el recorrido de hill climbing es: ", maximo_hc2.4)
time_hc2.4<-proc.time()[3]-ptm_hc2.4[3]
cat("El tiempo de la función con el recorrido de hill climbing es: ", time_hc2.4)



