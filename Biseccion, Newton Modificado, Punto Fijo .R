"Método de la Bisección."

"Insertamos la función a la cual le queremos encontrar raíz.
 También se debe verificar que la función sea C^2 en [a,b] (continua y que exista 
 f'(x) y f''(x)."
f<- function(x){ 
  x^3-(5*x^2)+7*x-1  
}

"Inicializamos variables que almacenan los valores x,f(x), Error relativo
 y error absoluto que se obtienen en cada iteración. También fijamos a i = 1
 que es con lo que empezará a correr el algortimo las iteraciones."
valor_x<-c() 
valor_f_x<-c()
error_relativo_x <- c()
error_absoluto_x <- c()
i<-1


"Creamos el algoritmo de Bisección que nos va a entregar
 la raíz de la función."
Metodo_Biseccion<- function(a,b,iteraciones){ 
  # La función tiene tres argumentos, los cuales consisten en los extremos del 
  # intervalo [a,b] respectivamente y el número de iteraciones que queremos que realice
  # el programa
  # Creamos un while que va a detener el programa hasta que se cumplan las iteraciones
  # que le dijimos al programa que hiciera 
  while(i< iteraciones+1){ 
    # Aquí vamos aumentando la iteración 
    i= i+1
    # Aquí calculamos el punto medio entre a y b, lo almacenamos en la variable x. 
    x<- (a+b)/2
    # Aquí evaluamos la función f en x, almacenamos x y f(x) en las variables que 
    # inicializamos anteriormente.   
    valor_x<-c(valor_x,x)
    valor_f_x<-c(valor_f_x,f(x)) 
    # Aquí colocamos una condición que nos dice que si f(a)*f(x)<0 renombramos 
    # a "b" como con nuestro valor x que obtuvimos al calcular el punto medio. 
    # Esto se debe a que si f(a)*f(x)<0 es porque la raíz se encuentra entre 
    # a y x, entonces nuestro nuevo intervalo es [a,x = b] ya dándole su nuevo valor a "b". 
    if ((f(a)*f(x))<0){
      b<-x
      nuevo_x <- (a+x)/2
      error_absoluto <- abs(nuevo_x-x)
      error_relativo <- abs((nuevo_x-x)/nuevo_x)
      error_absoluto_x <- c(error_absoluto_x,error_absoluto)
      error_relativo_x <- c(error_relativo_x, error_relativo)
    } else{ # Aquí lo contrario, si sudcede que f(a)*f(x)>0 entonces la raíz no se 
    # encuentra en el intervalo [a,x] entonces esa parte no nos sirve y ahora 
    # "a" pasa a ser x, entonces nuestro nuevo intervalo es: [a=x,b]
      a<-x
      nuevo_x <- (b+x)/2
      error_absoluto <- abs(nuevo_x-x)
      error_relativo <- abs((nuevo_x-x)/nuevo_x)
      error_absoluto_x <- c(error_absoluto_x,error_absoluto)
      error_relativo_x <- c(error_relativo_x, error_relativo)
    }
    # Aquí mencionamos el caso específico que si de casualidad encontramos la raíz
    # justo al calcular el punto medio nuevo entonces el programa se detiene pues 
    # ya encontramos nuestra x tal que f(x) = 0. 
    if (f(x)==0){  #si la raiz es igual a 0 se detiene el codigo con el comando break
      break
    }
  }
  # Creamos un data frame con los valores obtenidos en cada iteración 
  Tabla <- data.frame(valor_x,valor_f_x,error_absoluto_x,error_relativo_x)
  colnames(Tabla) <- c("Valor de x","Valor de f(x)","Error Relativo","Error Absoluto")
  print(Tabla)
  # Obtenemos todos los datos importantes de la última iteración y los imprimimos.  
  relativo <- tail(Tabla,1)[,3]
  absoluto <- tail(Tabla,1)[,4]
  cat("\n","La raíz es:",x,"\n","El número de iteraciones fue:",nrow(Tabla),"\n",
      "Con un error relativo de:",relativo,"\n","Con un error absoluto de:",absoluto)
}
Metodo_Biseccion(0,10,50)

"---------------------------------------------------------------------"
"Método de Newton Modificado."
#install.packages('Deriv') #se descarga paquete de derivadas 
library(Deriv)

"Insertamos la función a la cual le queremos encontrar raíz.
 También se debe verificar que la función sea C^2 en [a,b] (continua y que exista 
 f'(x) y f''(x)."
f<- function(x){ 
  x^3-(5*x^2)+7*x-3  
}

"Se calcula su primera y segunda derivada"
derivadaf=Deriv(f)
segund_derivadaf = Deriv(derivadaf)

"Inicializamos variables que almacenan los valores x,f(x), Error relativo
 y error absoluto que se obtienen en cada iteración. También fijamos a i = 1
 que es con lo que empezará a correr el algortimo las iteraciones."
valor_x<-c() 
valor_f_x<-c()
error_relativo_x <- c()
error_absoluto_x <- c()
i<-1

"Creamos el algoritmo de Newton modificado que nos va a entregar
 la raíz de la función."
Newton_Modificado <- function(v_inicial, iteraciones){
  # La función tiene dos argumentos, un valor incial, generalmente v_inicial = 0 
  # y el número de iteraciones a realizarse. 
  # Creamos un while que va a detener el programa hasta que se cumplan las iteraciones
  # que le dijimos al programa que hiciera 
  while(i<iteraciones+1){
    # Aquí vamos aumentando de iteración
    i= i+1
    # Calculamos el nuevo x_i
    x= v_inicial-(f(v_inicial)*derivadaf(v_inicial))/((derivadaf(v_inicial))^2-f(v_inicial)*segund_derivadaf(v_inicial))
    # Calculamoslos errores relativos y absolutos 
    error_relativo = abs((x-v_inicial)/x)
    error_absoluto = abs(x-v_inicial)
    # Almacenamos los errores relativos y absolutos
    error_absoluto_x <- c(error_absoluto_x,error_absoluto)
    error_relativo_x <- c(error_relativo_x,error_relativo)
    # Almacenamos los valores x y f(x) que se vayan haciendo en cada iteración
    valor_x <- c(valor_x,x)
    valor_f_x <- c(valor_f_x,f(x))
    # Reasignamos el valor x que obtuvimos como valor inicial y se repite el proceso
    v_inicial<-x
    
    # En esta parte metemos la condición de que si f(x) = 0, es decir, encontró la raíz
    # el progrma, entonces se detenga y hemos termiando. 
    if (f(x)==0){  
      break
    }
  }
  # Creamos un data frame con los valores obtenidos en cada iteración 
  Tabla <- data.frame(valor_x,valor_f_x,error_relativo_x,error_absoluto_x)
  colnames(Tabla) <- c("Valor de x","Valor de f(x)","Error relativo","Error absoluto")
  print(Tabla)
  # Obtenemos todos los datos importantes de la última iteración y los imprimimos.  
  relativo <- tail(Tabla,1)[,3]
  absoluto <- tail(Tabla,1)[,4]
  cat("\n","La raíz es:",x,"\n","El número de iteraciones fue:",nrow(Tabla),"\n",
      "Con un error relativo de:",relativo,"\n","Con un error absoluto de:",absoluto)
}
Newton_Modificado(0,10)

"-----------------------------------------------------------------------------------"

"Método de punto fijo"

"Se da una función que sea continua y derivable en un intervalo [a,b] "
f<- function(x){ 
  x^2-4*x-3 
  } 
"Para aplicar el algoritmo de punto fijo buscamos que f(x) -> g(x) = x, se transforme
 en una función g(x) tal que g(x) = x, en este caso se pide al usuario que entregue
 la función ya de esta forma y que verifique para g(x) que esta es de clase C1 en [a,b]
 entonces tiene por lo mentos un punto fijo y también debe verificar que ese punto
 fijo es único"
g <- function(x){
  3/(x-4)
}

"Inicializamos variables que almacenan los valores x,f(x), Error relativo
 y error absoluto que se obtienen en cada iteración. También fijamos a i = 1
 que es con lo que empezará a correr el algortimo las iteraciones."
valor_x<-c() 
valor_f_x<-c()
error_relativo_x <- c()
error_absoluto_x <- c()
i<-1

"Creamos el algoritmo de Método de Punto Fijo que nos va a entregar
 la raíz de la función."
Metodo_Punto_Fijo <- function(x,iteraciones){
  # La matriz tiene dos parámetros (x,iteraciones), donde x será el valor inicial con el
  # que se empezará el método (generalmente el punto medio de [a,b])
  while(i<iteraciones+1){
    # Aquí calculamos el nuevo punto x_i evaluando g(x) 
  x_i <- g(x)
  # Calculamos los errores relativos y absolutos y los almacenamos en las variables que 
  # anteriormente inicializamos. 
  error_absoluto = abs(x_i-x)
  error_absoluto_x <- c(error_absoluto_x,error_absoluto)
  error_relativo = abs((x_i-x)/x_i)
  error_relativo_x <- c(error_relativo_x,error_relativo)
  # Almacenamos los valores x_i que obtendremos en cada iteración, así como los valores
  # f(x) para ver como el método se va a acercando a su cero. 
  valor_x <- c(valor_x,x_i)
  valor_f_x <- c(valor_f_x,f(x))
  # Aquí repetimos el Loop haciendo que x_i sea el nuevo y así repetir el algoritmo 
  x <- x_i
  if(f(x)==0){ # Aquí detenemos el algortimo hasta que f(x) = 0, es decir, encontramos la raíz.
    break
  }
  }
  # Creamos un data frame con los valores obtenidos en cada iteración 
  Tabla <- data.frame(valor_x,valor_f_x,error_relativo_x,error_absoluto_x)
  colnames(Tabla) <- c("Valor de x","Valor de f(x)","Error relativo","Error absoluto")
  print(Tabla)
  # Obtenemos todos los datos importantes de la última iteración y los imprimimos.  
  relativo <- tail(Tabla,1)[,3]
  absoluto <- tail(Tabla,1)[,4]
  cat("\n","La raíz es:",x,"\n","El número de iteraciones fue:",nrow(Tabla),"\n",
      "Con un error relativo de:",relativo,"\n","Con un error absoluto de:",absoluto)
  }

Metodo_Punto_Fijo(-.5,6)





