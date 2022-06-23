f <- function(x){
  a <- exp(-x)/x
  return(a)
}
"----------------------Primera derivada centrada a 3 puntos---------------------------"
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

PDC3 <- function(f,h,xi){
  d <- (f(xi+h)-f(xi-h))/(2*h)
  cat("La aproximación es:" , d )
}
PDC3(f,.1,1)

"----------------------Primera derivada progresiva a 3 puntos---------------------------"
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

PDP3 <- function(f,h,xi){
  d <- (4*f(xi+h)- f(xi+2*h)-3*f(xi))/(2*h)
  cat("La aproximación es:" , d )
}
PDP3(f,.1,1)


"----------------------Primera derivada centrada a 5 puntos---------------------------"
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

PDC5 <- function(f,h,xi){
  d <- (215/36 * f(xi)- 8*f(xi-h) + f(xi-2*h) +3/4 *(64/27 * f(xi-3*h) - f(xi-4*h)))*(3/(11*h))
  cat("La aproximación es:" , d )
}
PDC5(f,.1,1.2)

"----------------------Primera derivada regresiva a 5 puntos---------------------------"
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

PDR5 <- function(f,h,xi){
  d <- (f(xi)-8*f(xi-h)+8*f(xi+h)-f(xi+h*2))/(12*h)
  cat("La aproximación es:" , d )
}
PDC5(f,.1,1)



f_1 <- function(x){
  (3*x)^(log(x))
}

"Derivada de Segundo Orden 3 Puntos Centrada."
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

Deriv_Segundo_Centrada_3 <- function(x,f){
  h <- x[2]-x[1]
  f_x_i_2 <- (f(x[3])+f(x[1])-2*f(x[2]))/h^2
  cat("La derivada evaluada en",x[2]," es: ",f_x_i_2)
}
x_1 <- c(.95,1,1.05)
Deriv_Segundo_Centrada_3(x_1,f_1)

"Derivada de Segundo Orden 3 Puntos Progresiva."
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

Deriv_Segundo_Progresiva_3 <- function(x,f){
  h <- x[2]-x[1]
  f_x_i_2 <- (f(x[3])-2*f(x[2])+f(x[1]))/h^2
  cat("La derivada evaluada en",x[1]," es: ",f_x_i_2)
}
x_2 <- c(1,1.05,1.1)
Deriv_Segundo_Progresiva_3(x_2,f_1)


"Derivada de Segundo Orden 5 Puntos Regresiva."
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

Deriv_Segundo_Regresiva_5 <- function(x,f){
  h <- x[5]-x[4]
  f_x_i_2 <- (12*f(x[5])-27*f(x[4])+16*f(x[3])+f(x[2])+-2*f(x[1]))/(7*h^2)
  cat("La derivada evaluada en",x[5]," es: ",f_x_i_2)
}
x_3 <- c(.80,.85,.9,.95,1)
Deriv_Segundo_Regresiva_5(x_3,f_1)


"Derivada de Segundo Orden 5 Puntos Centrada."
#f es la funcion
#h es el tamaño de los intervalos
#xi es el punto donde se quiere iniciar

Deriv_Segundo_Centrada_5 <- function(x,f){
  h <- x[3]-x[2]
  f_x_i_2 <- (8*f(x[4])+8*f(x[2])-f(x[5])-f(x[1])-14*f(x[3]))/(4*h^2)
  cat("La derivada evaluada en",x[3]," es: ",f_x_i_2)
}
x_4 <- c(.9,.95,1,1.05,1.1)
Deriv_Segundo_Centrada_5(x_4,f_1)









