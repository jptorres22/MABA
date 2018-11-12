### HW 2

#1 Define an R function that removes NA values from a vector.
v<-c(1,5,3, NA, 5)
remove_na=function(v){
  v[!is.na(v)]
}
remove_na(v)


#2 Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
factorial=function(x){
  if(x<0){
    return("factorial does not exist for negative numbers")
  }
  else if (x==0 || x==1){
    return(1)
  }
  else {
    (x*factorial(x-1))
  }
}
factorial(5)

#3 Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
#2x2 matrix
num <- c(1,2,3,4)
m = matrix(data = num, nrow = 2, ncol = 2, byrow = TRUE)
det=function(m){
  (m[1,1] * m[2,2]) - (m[1,2] * m[2,1])
}
det(m)

#4 Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
vec <- c(2,5,4,"a",3, "x")
sort_vector <- function(vec){
  vec[!sort(vec)]
}
sort(vec)

#5 Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
day <- function(x){
  d<-as.Date(x)
  p<-as.POSIXlt(d)
  if (p$wday == 1) {
    print("Monday")
  } else if (p$wday == 2) {
    print("Tuesday")
  } else if (p$wday == 3) {
    print("Wednesday")
  } else if (p$wday == 4) {
    print("Thursday")
  } else if (p$wday == 5) {
    print("Friday")
  } else if (p$wday == 6) {
    print("Saturday")
  } else {
    print("Sunday")
  }
  }
day("2018-12-25")


#6 Create a function to compute for your net pay at work.
mnet_pay = function(basic, taxable = 0, sss= 110, phealth = 100, nontaxable = 0, lwop = 0, months_pay = 13, working_days = 22){
  
  annual = (basic + taxable - sss - phealth) * months_pay
  
  if (annual <=  250000){
    net = annual
  } else if (annual <= 400000) {
    net = annual - ((annual - 250000) * 0.2)
  } else if (annual <= 800000) {
    net = annual - ((annual - 400000) * 0.25) - 30000
  } else if (annual <= 2000000) {
    net = annual - ((annual - 800000) * 0.30) - 130000
  } else if (annual <= 8000000) {
    net = annual - ((annual - 2000000) * 0.32) - 490000
  } else {
    net = annual - ((annual - 8000000) * 0.35) - 2410000
  }
  
  mnet = net/months_pay
  net_final = mnet + nontaxable - (mnet/working_days) * lwop
  return (net_final)
}
mnet_pay(basic = 30000)

#7 Create a function that accepts a vector and and integer n and returns nth highest number
nvec <- c(5,3,1,12,6)
highn <- function(nvec){
  sorthighn <- sort(nvec, decreasing = TRUE)
  head(sorthighn)[1]
}
highn(nvec)

#8 Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
c_int = function(annintrate = 0.01, comp_freq = 12, nyears = 3, principal = 10000) {
  ci = principal * (1+ (annintrate/comp_freq))^(nyears*comp_freq) - principal
  print(ci)
}
c_int(principal = 50000)

#9 Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
isPrime <- function(n){
  if (n == 2){
    TRUE
  }
  #divide given number by two to to number less one and check if whole number so modulo is zero 
    else if (any(n %% 2:(n-1) == 0)){
    FALSE
  }
    else {
    TRUE
  }
} 
isPrime(3)
  
