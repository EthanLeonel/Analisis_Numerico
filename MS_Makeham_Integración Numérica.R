
f1 <- function(x){
  .0001+.0003*(1.075)^(20+x)
}
"---------------------------------- Método del Regla del trapecio Simple-----------------------------------"
#Suponemos que solo conocemos 2 nodos
TS <- function(f,x0,x1){
  q <- (x1-x0)/2 * (f(x1) + f(x0))
  cat("La aproximación numérica de la integral es:",q)
}
TS(f1,0,10)
"---------------------------------- Método del Regla del trapecio Compuesto-----------------------------------"

"La idea es que el método tiene como parámetros la función que deseamos integrar,
el intervalo [a,b] donde se define la integral de la función y n viene siendo
el númer de intervalos conocidos, estos están dados por el número de puntos
que tengamos, para este método como necesitamos un número par de puntos,esta n
siempre será un número impar pues si conocemos 2n puntos tendremos 
(2n)-1 = 2n-1 de intervalos para todo natural n>=1 
dejando siempre un número impar de intervalos."
trapecio_compuesto <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    # Verificamos que la f que nos da el usuario sea solo una función sin evaluar,
    # si no es así el método simplemente no corre y pide en un mensaje que se le de
    # una función como parámetro. 
    print("f debe ser una función")
    break
  }
  # En esta parte estamos obteniendo h, que es la distancia que habrá de punto a punto.
  # Se divide entre n pues es la cantidad de intervalos que originalmente habrán desde
  # el punto a hasta el punto b. Esto se determina de acuerdo al número de puntos que 
  # conozcamos, en este caso se necesitan 2n puntos , lo que quiere decir que habrán
  # 2n-1 para todo natural, n>=1 intervalos.
  h <- (b - a) / n
  # En esta parte creamos el vector de puntos x_i desde [a,b] equidistantes de acuerdo
  # con la h que calculamos en el paso anterior.
  x_i <- seq.int(a, b, by = h)
  # Aquí calculamos las f(x_i) y creamos una lista que contenga las i_s desde 0 hasta
  # el número de intervalos que definimos para posteiormente crear un data frame 
  # que contenga los valores i_s x_i f(x_i) 
  f_x_i <- c(f(x_i))
  i_s <- seq.int(0,n,by=1)
  # Inicializamos el vector que va a ir guardando las f(x_i) de la suma de la fórmula.
  u <- c()
  # Empezamos un loop que va a ir iterando y calculando las f(x_i) que queremos.
  # Este va de 1 hasta n-1 pues el método asume que para i = 0, el término f(x_0)=f(a)
  # solo aparece una vez en la fórmula por lo tanto ese lo colocamos explícitamente, 
  # ahora para i = n, estamos ya en el el último punto del intervalo [a,b], 
  # entonces  f(x_n) = f(b) el cual también solo aparece una vez, 
  # entonces se coloca explícitamente en la fórmula y no debemos preocuparnos 
  # por calcular estos dos f(x_i).Entonces solo nos enfocamos en calcular los puntos x_i 
  # entre [1,n-1].
  for(i in 1:(n-1)){
    # Aquí se toma el elemento i+1 del vector de puntos x_i, esto pues R empieza el 
    # conteo de los elementos de un vector desde 1 como el primer elemento del vector.
    # entonces colocamos i+1 para que empiece a partir de la segunda posición del vector
    # hasta la penúltima posición del vector, que viene a ser el intervalo [1,n-1] 
    # de puntos de los cuales queremos conocer sus respectivas f(x_i) y guardamos
    # los valores que vamos obteniendo en un vector.
    u <- c(u,f(x_i[i+1]))
  }
  # Hacemos la suma de los elementos f(x_i) que no son x_0 ni x_n. 
  SumaU <- sum(u)
  # Calculamos la aproximación de la integral con la fórmula vista en clase,
  # en esta parte colocamos f(a) y f(b) explícitamente pues solo aparecen una vez.
  aproximacion <- (h/2)*(f(a)+(2*SumaU)+f(b))
  # Creamos un DataFrame que nos muestre i_s,x_i,f(x_i) ordenados y acomodados
  # con su correspondiente x_i. 
  Tabla <- data.frame(i_s,x_i,f_x_i)
  # Renombramos columnas. 
  colnames(Tabla) <- c("i","xi","f(xi)")
  # Imprimimos los resultados.
  print(Tabla)
  cat("\n","La aproximación numérica de la integral es:",aproximacion)
}

trapecio_compuesto(f1, 0, 10, 3)

"---------------------------------- Método de Simpson 1/3 Simple-----------------------------------"
#Suponemos que solo conocemos 3 nodos

MSS <- function(f,x0,x1){
  h <- abs(x1-x0)/2
  q <- (h/3) * (f(x0) + 4* f(x0 + h) + f(x1))
  cat("La aproximación numérica de la integral es:",q)
}
MSS(f1,0,10)

"---------------------------------- Método de Simpson 1/3 Compuesto-----------------------------------"


"La idea es que el método tiene como parámetros la función que deseamos integrar,
el intervalo [a,b] donde se define la integral de la función y n viene siendo
el númer de intervalos conocidos, estos están dados por el número de puntos
que tengamos, para este método como necesitamos un número impar de puntos, esta n
siempre será un número par pues si conocemos 2n+1 puntos tendremos 
(2n+1)-1 = 2n de intervalos dejando siempre un número par de intervalos." 

Simpson_Compuesto_1_3 <- function(f, a, b, n){
  # Verificamos que la f que nos da el usuario sea solo una función sin evaluar,
  # si no es así el método simplemente no corre y pide en un mensaje que se le de
  # una función como parámetro. 
  if (is.function(f) == FALSE) {
    print('f debe ser una función, por favor ingresar una función f sin evaluar.')
    break
  }
 # En esta parte estamos obteniendo h, que es la distancia que habrá de punto a punto.
 # Se divide entre n pues es la cantidad de intervalos que originalmente habrán desde
 # el punto a hasta el punto b. Esto se determina de acuerdo al número de puntos que 
 # conozcamos, en este caso se necesitan 2n+1 puntos , lo que quiere decir que habrán
 # 2n intervalos. 
  h <- (b - a) / n
 # En esta parte creamos el vector de puntos x_i desde [a,b] equidistantes de acuerdo
 # con la h que calculamos en el paso anterior. 
  x_i <- seq.int(a, b, by = h)
  f_x_i <- c(f(x_i))
  i_s <- seq.int(0,n,by=1)
 # Inicializamos variables donde almacenaremos los resultados de f(x_i) para cada tipo
 # de suma.
  par <- c()
  impar <- c()
 # Empezamos un loop que va a ir iterando y calculando las f(x_i) que queremos
 # cuando i es par y cuando i es impar. Este va de 1 hasta n-1 pues el método asume
 # que para i = 0, el término f(x_0)=f(a) solo aparece una vez en la fórmula por lo tanto
 # ese lo colocamos explícitamente, ahora para i = n, estamos ya en el el último punto
 # del intervalo [a,b],entonces  f(x_n) = f(b) el cual también solo aparece una vez, 
 # entonces se coloca explícitamente en la fórmula y no debemos preocuparnos 
 # por calcular estos dos f(x_i).Entonces solo nos enfocamos en calcular los puntos x_i 
 #entre [1,n-1].
  for(i in 1:(n-1)){
    # verifica si i es par y calcula su f(x_i)
    if(i%%2==0){
      # Aquí se toma el elemento i+1 del vector de puntos x_i, esto pues R empieza el 
      # conteo de los elementos de un vector desde 1 como el primer elemento del vector.
      # entonces colocamos i+1 para que empiece a partir de la segunda posición del vector
      # hasta la penúltima posición del vector, que viene a ser el intervalo [1,n-1] 
      # de puntos de los cuales queremos conocer sus respectivas f(x_i) y guardamos
      # los valores que vamos obteniendo en un vector.
      par <- c(par,f(x_i[i+1]))
    # verifica si i es impar y calcula su f(x_i)  
    }else if(i%%2!= 0){
      #Aquí se toma el elemento i+1 del vector de puntos x_i, esto pues R empieza el 
      # conteo de los elementos de un vector desde 1 como el primer elemento del vector.
      # entonces colocamos i+1 para que empiece a partir de la segunda posición del vector
      # hasta la penúltima posición del vector, que viene a ser el intervalo [1,n-1] 
      # de puntos de los cuales queremos conocer sus respectivas f(x_i) y guardamos
      # los valores que vamos obteniendo en un vector.
      impar <- c(impar,f(x_i[i+1]))
    }
  }
  # Hacemos la suma de los elementos f(x_i), donde i es par. 
  Sumapar <- sum(par)
  # Hacemos la suma de los elementos f(x_i), donde i es impar.
  Sumaimpar <- sum(impar)
  # Calculamos la aproximación de la integral con la fórmula vista en clase,
  # en esta parte colocamos f(a) y f(b) explícitamente pues solo aparecen una vez. 
  aproximacion <- (h/3)*(f(a)+(4*Sumaimpar)+(2*Sumapar)+f(b))
  # Creamos un DataFrame que nos muestre i_s,x_i,f(x_i) ordenados y acomodados
  # con su correspondiente x_i. 
  Tabla <- data.frame(i_s,x_i,f_x_i)
  # Renombramos las columnas.
  colnames(Tabla) <- c("i","xi","f(xi)")
  # Imprimimos los resultados. 
  print(Tabla)
  cat("\n","La aproximación numérica de la integral es:",aproximacion)
}

Simpson_Compuesto_1_3(f1, 0, 10, 4)

"---------------------------------- Método de Simpson 3/8 Smple-----------------------------------"
#suponemos que conocemos 4 nodos
MSS <- function(f,x0,x1){
  h <- abs(x1-x0)/3
  q <- 3*h/8 * (f(x0) + 3 * f(x0+h) + 3 * f(x0 + 2 * h) + f(x1))
  cat("La aproximación numérica de la integral es:",q)

}
MSS(f1,0,10)


"---------------------------------- Método de Simpson 3/8 Compuesto-----------------------------------"
#Suponemos que se ingresará un numero de nodos tales que el número de intervalos es multiplo de 3


RSC <- function(f,n,x0,xn){
  #construimos h
  z <- abs(xn-x0)/(n -1)
  #iniciamos un vector vacío
  e <- c()
  #Calculamos las imagenes de cada nodo y las guardamos en un vector
  for (i in 0:(n-1)){
    q <-f(x0+i*z)
    e <- c(e,q)
  }
  #Hacemos la suma de de f(x3i)
  r <- 0
  for(i in 1:(((n-1)/3)-1)){
    t <- e[3*i+1]
    r <- r+t
  }
  #hacemos la suma de f(x3i+1) + f(x3i+2)
  u <- 0
  for(i in 0:(((n-1)/3)-1)){
    y <- e[(3*i+2)] + e[(3*i+3)]
    u <- u+y
  }
  #Finalmente ejecutamos la formula y obtenemos el resultado
  x <- (3*z/8)*(e[1] + (2 * r) + (3 * u)+e[n])
  
  cat("La distancia entre cada intervalo es de:" , z ,"\n","La aproximación numérica de la integral es:",x)
}

RSC(f1,7,0,10)

"------------------------------- Cuadratura Gaussina ---------------------------------"
Cuadratura_Gaussiana <- function(f,a,b,n){
  #construimos una matriz con los wi's correspondientes 
  m1 <- matrix(c(1,1,0,0,0,.5555555556,.8888888889,.5555555556,0,0,.3478548451,
                 .6521451549,.6521451549,.3478548451,0,.236926885,.4786286705,.5688888889,
                 .4786286705,.236926885),5,4)
  #Construimos una matriz con los xi's correspondientes
  m2 <- matrix(c(-sqrt(1/3),sqrt(1/3),0,0,0,.7745966692,0,-.7745966692,0,0,
                 .8611363116,.3399810436,-.3399810436,-.8611363116,0,.9061798459,.5384693101,0,
                 -.5384693101,-.9061798459),5,4)
  #creamos vectores vacos para poderlos llenar de acuerdo al numero de nodos
  #wi's
  w <- rep(0,n)
  #f(xi)'s
  x <- rep(0,n)
  #Caso cuando nuestra integral va de -1 a 1
  if(a == -1 && b == 1){
    # Llenamos el vector w con sus valores correspondientes de la matriz m1 
    # dependiendo el valor de n Llenamos el vector x con los valores de f(x) 
    # evaluadas en los x de la matrix m2
    for(i in 1:n){
      w[i] <- m1[i,n-1]
      x[i] <- f(m2[i,n-1])
    }
    #multiplicamos entrada por entrada de los vectores x y w y sumamos.
    s <- sum(w*x)
    cat("La aproximación es", s)
  }else{
    #En caso que a != -1 ó b != 1
    # Llenamos el vector w con sus valores correspondientes de 
    # la matriz m1 dependiendo el valor de n
    # Llenamos el vector x con los valores de f() evaluadas en la tranformación 
    for(i in 1:n){
      w[i] <- m1[i,n-1]
      x[i] <- f((((b-a)*m2[i,n-1])+a+b)/2)
    }
    #multiplicamos entrada por entrada de los vectores x y w y sumamos a 
    # esto lo multuplicamos por el 
    #tamaño del intervalo dividido entre el 2 del diferencial
    s <- ((b-a)/2)*sum(w*x)
    cat("La aproximación es", s)
  }
}
Cuadratura_Gaussiana(f1,0,10,5)

