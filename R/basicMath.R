#' Add Numbers
#'
#' This function adds two numbers
#'
#' @param x integer
#' @param y integer
#' @return The sum of two integers
#' @export
add <-  function(x, y){x + y}

#' Subtract Numbers
#'
#' This function adds two numbers
#'
#' @param x integer
#' @param y integer
#' @return The difference of two integers
#' @export
subtract <-  function(x, y){x - y}

#' Multiply Numbers
#'
#' This function multiplies two numbers
#'
#' @param x integer
#' @param y integer
#' @return The multiplication of two integers
#' @export
multiply <- function(x, y){x * y}

#' Divide Numbers
#'
#' This function divides two numbers
#'
#' @param x integer
#' @param y integer
#' @return The division of two integers
#' @export
divide <- function(x, y){
  if (y == 0) {
    NULL
  } else {
    x/y
  }
}

#' Factorial
#'
#' This function gets the factorial of x (i.e. x!)
#'
#' @param x integer
#' @return The factorial of an integer
#' @export
factorial <- function(x) {
  i <- x
  total <- 1
  while (i != 0){
    total <- total * i
    i <- i - 1
  }

  return(total)
}

#' Square root
#'
#' This function returns the square root of an integer
#'
#' @param x integer
#' @return The square root of an integer
#' @export
sqr_root <- function(x) {sqrt(x)}


#' Is a Triangle
#'
#' This function determines whether 3 values make a triangle.
#'
#' @param x,y,z 3 integer values
#' @return Boolean value if passes the triangle inequality
#' @export
is_triangle <- function(x, y, z) {
  if (x + y > z || x + z > y || y + z > x) {
    return(true)
  } else {
    return(false)
  }
}

#' Hypotenuse
#'
#' This function finds the hypotenuse given two sides of a triangle.
#'
#' @param x,y 2 integer values (sides of a triangle)
#' @return Numerical value of hypotenuse
#' @export
hypotenuse <- function(x, y) {sqrt(x^2 + y^2)}

#' Area of triangle
#'
#' This function finds the area of a triangle.
#'
#' @param b,h base and height values of triangle
#' @return Numerical value of area
#' @export
area_tri <- function(b,h) {0.5*b*h}

#' Area of parallelogram
#'
#' This function finds the area of a parallelogram
#'
#' @param b,h base and height values of parallelogram
#' @return Numerical value of area
#' @export
area_parallelogram <- function(b,h) {b*h}

#' Area of Square
#'
#' This function finds the area of a square
#'
#' @param x value of side of square
#' @return Numerical value of area
#' @export
area_sq <- function(x) {x^2}

#' Area of Circle
#'
#' This function finds the area of a circle
#'
#' @param r value of side of radius
#' @return Numerical value of area
#' @export
area_circ <- function(r) {pi*r^2}

#' Circumference of Circle
#'
#' This function finds the circumerence of a circle
#'
#' @param r value of side of radius
#' @return Numerical value of area
#' @export
circumference <- function(r) {2*pi*r}

