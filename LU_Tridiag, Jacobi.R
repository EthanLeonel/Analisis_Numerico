"Método LU para matrices tridiagonales"

"Aquí mandamos a llamar una librería que nos ayudará a calcular inversas
 y verificar que la matriz que el usuario meta sea cuadrada y simética."
library(matrixcalc)

"En esta parte el usuario crea la matriz según el sistema de ecuaciones que tenga
 así como el vector b de términos independientes."
Matriz_B <- rbind(c(2,-1,0),
                  c(-1,2,-1),
                  c(0,-1,2))
Vector_b <- cbind(c(3,-3,1))


"Empezamos a crear el algoritmo LU para resolver sistemas de ecuaciones lineales 
 con condición de que la matriz de este sistema sea tridiagonal."
Metodo_LU_tridiag <- function(Matriz_A,Vector_b){
  # Aquí verificamos si la matriz que nos entrega el usuario cumple con las 
  # características de ser cuadrada, simétrica y no singular, de no ser así sale 
  # un mensaje de advertencia para que el usuario ingrese una matriz con esas condiciones.
  if(is.square.matrix(Matriz_A) != T || is.symmetric.matrix(Matriz_A) !=T ||
     is.non.singular.matrix(Matriz_A)!= T ){
    cat("La matriz no cumple con las condiciones de ser cuadrada,simétrica o no singular.",
        "\n","Por favor ingrese una matriz que cumpla todas las condiciones.")
  } 
  # Aquí creamos la Matriz L y U con las dimensiones de la matriz A. L solo tendrá ceros
  # en su diagonal, U es una matriz vacía. 
  Matriz_U <- matrix(0,nrow = nrow(Matriz_A), ncol = ncol(Matriz_A))
  Matriz_L <- diag(rep(1,ncol(Matriz_A)))
  # Aquí le decimos a la función que la posición a11 de la matriz A será igual a la 
  # posición u11 de la matriz U por las fórmulas que deducimos en el ejercicio 6. 
  Matriz_U[1,1] <- Matriz_A[1,1]
  # Aquí empezamos a llenar la diagonal superior de la Matriz U, por el ejercicio 6
  # deducimos que el valor de la posición u_i,i+1 de la Matriz U es igual al valor de
  # la posición a_i,i+1 de la Matriz A. Entonces creamos un for que se detiene hasta
  # el penúltimo renglón de la matriz que nos entrega el usuario y va llenando posición
  # por posición. 
  for(i in 1:(nrow(Matriz_A)-1)){
    Matriz_U[i,i+1] <- Matriz_A[i,i+1]
  }
  # Aquí llenamos la diagonal inferior de la matriz L con ayuda de las fórmulas que 
  # deducimos en el ejercicio 6. Para la diagonal inferior de la matriz L la fórmula es:
  # l_i,i-1 = a_i,i-1 / u_i-1,i-1
  # Para la diagonal de la Matriz U la fórmula es: 
  # u_i,i = a_i,i -  l_i,i-1 * u_i-1,i 
  # Así se va realizando el proceso y se van llenando las matrices L y U respectivamente.
  for(i in 2:nrow(Matriz_A)){
    Matriz_L[i,i-1] <- Matriz_A[i,i-1]/Matriz_U[i-1,i-1]
    Matriz_U[i,i] <- Matriz_A[i,i]-Matriz_L[i,i-1]*Matriz_U[i-1,i]
  }
  # Aquí hacemos el cálculo de L*y = b, entonces sacamos la inversa de L y la multiplicamos
  # por el vector b, de tal forma que obtenemos y. 
  Vector_y <- matrix.inverse(Matriz_L)%*%Vector_b
  # Aquí ya que tenemos nuestro vector auxiliar y, entonces resolvemos U*x = y 
  # calculamos la inversa de U y la multiplicamos por el vector y de tal forma obtenemos x.
  Vector_x <- matrix.inverse(Matriz_U)%*%Vector_y
  cat("El vector resultado es:","\n")
  print(Vector_x)
}

Metodo_LU_tridiag(Matriz_B,Vector_b)

"----------------------------------------------------------------------------------"

"Método de Jacobi"

"Creamos el método de Jacobi, Inicialmente pedimos que el usuario la matriz del sistema, el vector de 
 términos independientes, un vector inicial con el cual empezaremos a iterar y el número de iteraciones."

Metodo_Jacobi <- function(Matriz_A,Vector_b,Vector_xl,iteraciones){
  # En esta sección vamos a empezar a ordenar la matriz en dado caso que el usuario no nos la entregue 
  # con la condición de que la matriz debe ser diagonalmente dominante, entonces haríamos el intercambio 
  # de renglones para que en la diagonal se encuentren los coeficientes más grandes correspondientes 
  # a cada renglón en la posición a_i,i
  
  # Creamos una matriz A y un vector b vacíos con las dimensiones correspondientes a la matriz A y el
  # vector de términos independientes b. 
  Matriz_A_Ordenada <- matrix(0,ncol = ncol(Matriz_A),nrow = nrow(Matriz_A))
  Vector_b_Ordenado <- matrix(0,ncol = 1 ,nrow = nrow(Vector_b))
  # Inicializamos este vector que almacenará la posición del coeficiente mayor de cada renglón de la matriz. 
  Posicion <- c()
  
  # Iteraremos este proceso hasta que todos los renglones queden ordenados dependiendo de la cantidad de 
  # renglones de la matriz. 
  for(i in 1:nrow(Matriz_A)){
  # Rescatamos el renglón i de la matriz. 
  Renglon_i <- Matriz_A[i,]
  # Extraemos la posición del coeficiente mayor del renglón i
  Posicion_Coeficiente <- max(which(max(Renglon_i)==Renglon_i))
  # Almacenamos esa posición en nuestro vector que anteriormente iniciañizamos. 
  Posicion <- c(Posicion,Posicion_Coeficiente)
  }
  # Aquí hacemos el proceso de cambiar renglones para que el coeficiente mayor quede en la posición a_i,i del
  # renglón i. Lo mismo sucede con el vector, solo que ahí acomodamos el término independiente del vector i 
  # en su nueva posición i según se hayan intercambiado los renglones a conveniencia para que la matriz sea 
  # diagonalmente dominante. 
  for(i in 1:nrow(Matriz_A)){
    Matriz_A_Ordenada[Posicion[i],] <- Matriz_A[i,]
    Vector_b_Ordenado[Posicion[i],] <- Vector_b[i,]
  }
  
  # Aquí empezamos a crear la matriz D, la inicializamos con puros ceros y con la misma dimensión que la 
  # Matriz A ya ordenada en el paso anterior. 
  Matriz_D <- matrix(0,nrow = nrow(Matriz_A_Ordenada), ncol = ncol(Matriz_A_Ordenada))
  # Extraemos la Diagonal de la Matriz A ordenada. 
  Diagonal_A <- diag(Matriz_A_Ordenada)
  # Creamos un for que se detendrá hasta el número de renglones que tenga la matriz ordenada A 
  # que nos va a ir llenando nuestra matriz D con los elementos de la diagonal de la Matriz ordenada A 
  # en los lugares a_i,i de la Matriz D, es decir la diagonal de la Matriz D la llenamos
  # con los elementos de la diagonal de la Matriz ordenada A.
  for(i in 1:length(Diagonal_A)){
    Matriz_D[i,i] <- Diagonal_A[i]
  }
  # Si la Matriz D no cumple con ser no singular, el programa se detiene con un mensaje de error. 
  if(is.non.singular.matrix(Matriz_D) != T){
    print("La matriz D no cumple con ser no snigular, verifique bien el sistema de entrada.")
  }
  # Calculamos La Matriz D Inversa para efectuar los cálculos
  Matriz_D_Inversa <- matrix.inverse(Matriz_D)
  # Calculamos la Matriz Q
  Matriz_Q <- Matriz_A_Ordenada - Matriz_D
  # Empezamos a realizar los cálculos hasta el número de iteraciones que el usuario meta.
  for(i in 1:iteraciones){
    # Obtenemos el vector x_i para i=1,..., # iteraciones
    x_i <- Vector_b_Ordenado - Matriz_Q%*%Vector_xl
    x_i <- Matriz_D_Inversa%*%x_i
    # Repetimos el proceso.
    Vector_xl <- x_i
  }
  cat("EL vector solucón aproximado es:","\n",Vector_xl)
}

Matriz <- rbind(c(3,-1,1),
                c(3,3,7),
                c(3,6,2))
Vector <- cbind(c(1,4,0))

Metodo_Jacobi(Matriz,Vector,c(0,0,0),3)



