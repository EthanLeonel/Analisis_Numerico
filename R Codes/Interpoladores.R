"Interpolador de Lagrange"
# El interpolador de Lagrange necesita el punto x donde vamos a evaluar el polinomio, 
# un vector de puntos xi y un vector de imagenes fxi. 
Interpolador_Lagrange <- function(x,xi,fxi){
  # Primero vamos a construir el producto de las Li's, almacenaremos todo en un vector
  # que usaremos después para crear el polinomio. 
  L =c()
  # Empezamos a creer el producto de las Li, entonces la longitud del producto es de 1 hasta 
  # la longitud del vector de las fxi. Es decir el producto de Li desde i=1 hasta n. 
  # Lo hacemos desde i=1 hasta n porque R empieza a contar el la primera posición del vector
  # a partir de 1. 
  for(i in 1:length(fxi)){
    # Inicializamos el vector Li.
    L[i] = 1 
    for(j in 1:length(fxi)){
      # Recordemos que i != j pues si no el denominador se anula. Entonces ya realizamos 
      # producto. 
      if( i != j){
        # Se va calculando el producto de las Li y se va almacenando. 
        L[i] = L[i]*((x-xi[j])/(xi[i]-xi[j]))
      }
    }
  }
  # Ahora inicializamos la variable polinomio que contendrá el polinomio que creamos y después lo
  # evaluará en el punto x que le pasamos a la función. 
  Polinomio = 0 
  # Hacemos la suma desde i=1 hasta n del producot de fxi*Li como lo indiga el algoritmo visto
  # en clase. 
  for(i in 1:length(fxi)){
    Polinomio = Polinomio + (fxi[i]*L[i])
  }
  # Imprimos el resultado de evaluar el polinomio que interpolamos en el punto x. 
  cat("El polinomio evaluado en ",x," es:",Polinomio)
}

x_1 <- c(2,2.5,4)
f_x_1 <- c(.5,.4,.25)
Interpolador_Lagrange(3,x_1,f_x_1)



"Interpolador de newton"

# Este interpolador necesita como parámetros para trabajar una matriz de diferencias 
# así como un vector de puntos

# Primero creamos la matriz de diferencias que tiene como parámetros dos vectores, que son
# x_i y f(x_i)
# Matriz de diferencias
Matriz_diferencias<-function(xi,fxi){
  # Obtenemos la longitud de cada vector, tienen que ser de misma longitud. 
  r1<-length(xi)
  r2<-length(fxi)
  # El programa se detiene si la longitud de ambos vectores no es la misma. 
  if(r1!=r2){
    print("El método no se puede aplicar")
    break
  }else{
  # Si son iguales empezamos a crear la matriz, primero creamos una matriz 
  # con las dimensiones de los vectores de puros ceros. 
    mat<-matrix(0,r1,r2)
  # Llenamos la primera columna de la matriz con los elemenos del vector f(xi)
    for (i in 1:r1){
      mat[i,1]<-fxi[i]
    }
    # Llenamos las demás entradas de la matriz, usamos dos for que ambos empiezan en 2
    # pues la primera columna y el primer renglón de la matriz ya tienen valores.
    for(i in 2:r1){
      for(j in 2:i){
        # La matriz se va llenando con elementos del tipo:  (fxi-fxi-1)/(xi-xi-1)
        # Ejemplo: en la posición 2,2 tenemos al elemento (f(x1)-f(x0))/(x1-x0) 
        # para la posición 2,3 tenemos al elemento (f(x2)-f(x1))/(x2-x1) y así
        # sucesivamente se va llenando posición por posición. Entonces entre más se mueve j
        # más a la derecha vamos calculando valores (diferencias de 1,2,... orden).
        mat[i,j]<-(mat[i,(j-1)]-mat[(i-1),(j-1)])/(xi[i]-xi[i-(j-1)])
      }
    }
    return(mat)
    }
}

# Ya que tenemos construida la matriz de Diferencias ahora si podemos construir el interpolador
# este tendrá como parámetros la matriz de diferencias, el vector de puntos x_i y el punto p
# donde vamos a evaluar. 

Interpolador_Newton<-function(A,xi,p){
  # Obtenemos la dimensión de la matriz de diferencias (es cuadrada).
  a<-dim(A)
  # Extraemos la primera y segunda entrada del vector. 
  a1<-a[1]
  a2<-a[2]
  # Agrupamos los productos de las xi's en un vector. 
  vec_r<-rep(0,a1)
  # Realizamos el producto entre vectores.
  for( i in 2:a1){
    prod<-1
    for(j in 1:(i-1)){
      # Este vector va acumulando las multiplicaciones de las xi's
      prod<-(prod*(p-xi[j]))
    }
    # Vamos guardando el vector de multplicaciones. 
    vec_r[(i-1)]<-prod
  }
  # Realizamos la multiplicación de ai's por los productos de las xi's
  inter<-A[1,1]
  # Creamos el polinomio Pn(x) = a0 +a1(x-x0)+...+an-1(x-x0)...(x-xn-2)
  for(i in 2:a1){
    inter<-inter+(A[i,i]*vec_r[(i-1)])
  }
  return(inter)
}

# Ejemplo 
vec_xi<-c(2,2.5,4)
vec_fxi<-c(.5,.4,.25)
MD<-Matriz_diferencias(vec_xi,vec_fxi)
Aprox <- Interpolador_Newton(MD,vec_xi,3)
Aprox

"----------------------Trazadores cúbicos---------------------------"
# xi: vector con los valores de los xi's
# yi: vector con los valores de los f(xi)

TC <- function(xi,yi,v){
  if(length(xi) == length(yi)){
    n <- length(xi)
    #Construios nuestra matriz para calcular los ci's, Ac = b
    A <- diag(n)
    for(i in 1:(n-2)){
      A[i+1,i] <- (xi[i+1]-xi[i])
      A[i+1,i+1] <- 2*(xi[i+2]-xi[i])
      A[i+1,i+2] <- (xi[i+2]- xi[i+1])
    }
    #Construimos el vector de resultados
    b <- matrix(0,nrow = n,ncol = 1) 
    for(i in 1:(n-2)){
      b[i+1,1] <- 3*((yi[i+2]-yi[i+1])/(xi[i+2]-xi[i+1])-(yi[i+1]-yi[i])/(xi[i+1]-xi[i]))
    }
    AI <- solve(A)
    #Obtenemos nuestro vector de ci's
    c <- c(AI %*% b)
    #Calculamos nuestros valores de di's
    d <- c()
    for(i in 1:(n-1)){
      du <- (c[i+1]-c[i])/(3*(xi[i+1]-xi[i]))
      d <- c(d,du)
    }
    #Calculamos nuestros valores de bi's
    bi <- c()
    for(i in 1:(n-1)){
      bu <- (yi[i+1]- yi[i])/(xi[i+1]-xi[i])-(xi[i+1]-xi[i])*(c[i]) - (d[i])*(xi[i+1]-xi[i])^2
      bi <- c(bi,bu)
    }
    #Calculamos nuestros valores de ai's
    a <- c()
    for(i in 1:(n-1)){
      a <- c(a,yi[i])
    }
    #Calculamos nuestra matrix s llenando renglon por renglon
    s <- matrix(0,n-1,4)
    for(i in 1:(n-1)){
      s[i,] <- c(a[i],bi[i],c[i],d[i])
    }
    #Si v esta dentro de los valores de xi vamos a evaluar entre cuales 2 valores de xi se encuentra
    if(xi[1]<= v && v < xi[n]){
      for(i in 1:(n-1)){
        if(xi[i]<= v && v < xi[i+1]){
          #cuando encuentre los dos valores que entre los cuales se encuentre v
          # hara el formulazo eplicado en el inciso 3 de la tarea
          r <- s[i,1] + s[i,2]*(v-xi[i]) + s[i,3]*(v-xi[i])^2 + s[i,4]*(v-xi[i])^3
          
        }
      }
      cat("La aproximación para",v, "es", r)
    }else{print("El valor a evaluar no esta en el rango de x")}
    
  }else{
    print("El tamaño de tus vectores no coincide")
  }
}
xi <- c(.017,.02,5.53)
yi <- c(.154,.18,6.94)

TC(xi,yi,6)
TC(xi,yi,.018)







