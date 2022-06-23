
f1 <- function(x){
  .0001+.0003*(1.075)^(20+x)
}

f2 <- function(x) {
  return(exp(2 * x) * sin(3 * x))
}

f3 <- function(x) {
  -1/x^2
}

f4 <- function(x){
  x^2*log(x)
}

"Empezamos a crear el Método del Regla del trapecio Compuesto"

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
trapecio_compuesto(f2, 0, 2, 7)
trapecio_compuesto(f3, 1, 2, 3)
trapecio_compuesto(f6, 2, 4, 3)


"Empezamos a crear el método de Simpson 1/3 Compuesto"

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
Simpson_Compuesto_1_3(f2, 0, 2, 8)
Simpson_Compuesto_1_3(f3, 1, 2, 4)
Simpson_Compuesto_1_3(f4, 2, 4, 4)


