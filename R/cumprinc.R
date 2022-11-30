#' Remaining principal at time n
#'
#' @param s original loan amount
#' @param r interest rate. Divide by 100 if in a percent and again by 12 if yearly 
#' @param t loan term in months
#' @param n month to return value for
#' @keywords cumprinc excel amortization amortized loan principal
#' 
#' @examples 
#' s <- 10000
#' r <- 5 / 100 / 12
#' t <- 60
#' n <- 5
#' princ_remn( s, r, t, n)
#'
#' @return numeric value of remaining principal
#' @export
princ_remn <- function(s,r,t,n){
  
  d <- r * s/(1 - (1 + r)^-t)
  
  (d + (1 + r)^n * (r * s - d)) / r
  
}

#' Principal to be paid back at time n
#'
#' @param s original loan amount
#' @param r interest rate. Divide by 100 if in a percent and again by 12 if yearly 
#' @param t loan term in months
#' @param n month to return value for
#' @keywords cumprinc excel amortization amortized loan principal
#' 
#' @examples 
#' s <- 10000
#' r <- 5 / 100 / 12
#' t <- 60
#' n <- 5
#' princ_month( s, r, t, n)
#'
#' @return numeric value of principal paid in given month
#' @export
princ_month <- function(s,r,t,n){
  
  d <- r * s/(1 - (1 + r)^-t)
  
  (d - r * s) * (r + 1)^(n - 1)
  
}

#' Accumulated principal paid back at time n
#'
#' @param s original loan amount
#' @param r interest rate. Divide by 100 if in a percent and again by 12 if yearly 
#' @param t loan term in months
#' @param n month to return value for
#' @keywords cumprinc excel amortization amortized loan principal
#' 
#' @examples 
#' s <- 10000
#' r <- 5 / 100 / 12
#' t <- 60
#' n <- 5
#' princ_accum( s, r, t, n)
#'
#' @return numeric value of accumulated paid principal
#' @export
princ_accum <- function(s,r,t,n){
  
  d <- r * s/(1 - (1 + r)^-t)
  
  (d - r * s) * ((1 + r)^n - 1)/r
  
}