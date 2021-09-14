
sigma = 0.2
eta = 0.8

Ufun = function(a,b,y) {
  
  x = (a^(1-sigma) + b^(1-sigma))^(1/(1-sigma))
  U = x^(1-eta) + y^(1-eta)
  
  return(U)
  
}

a0 = 10
b0 = 10
y0 = 10

Ustart = Ufun(a0, b0, y0)

ayprice = function(delA) {
  
  rootfun = function(delY) {
    return(Ufun(a0 + delA, b0, y0 - delY) - Ustart)
  }
  
  out = uniroot(rootfun, c(0, 10))$root
  return(out)
  
}

ayp1 = 1 / ayprice(1)
ayp2 = 2 / ayprice(2)
ayp3 = 3 / ayprice(3)

ayp1
ayp2
ayp3

ayp3 / ayp1

abprice = function(delA) {
  
  rootfun = function(delB) {
    return(Ufun(a0 + delA, b0 - delB, y0) - Ustart)
  }
  
  out = uniroot(rootfun, c(0, 10))$root
  return(out)
  
}

abp1 = 1 / abprice(1)
abp2 = 2 / abprice(2)
abp3 = 3 / abprice(3)

abp1
abp2
abp3

abp3 / abp1








