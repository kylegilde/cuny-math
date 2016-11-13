
#Derivatives
require(Deriv)
Deriv(-2*x^3)
Deriv(-8/x^2)
Deriv(5*x^(1/3))
Deriv(-2*x^(9/8))
Deriv((-2*x^-2+1)*(-5*x+9))
Deriv((5*x^.5+7)/(-x^3+1))
Deriv((3*x^-3-8*x+6)^(4/3))


#13. After a sewage spill, the level of pollution in Sootville is estimated by  f (t)  =  (550t^2)/√( &t^2  + 15), where t  is the time in days since the spill occurred.  
#How fast is the level changing after 3  days?  
ft=function(t) (550*t^2)/sqrt(t^2+15)
a=Deriv(ft)
a(3)

#14. The average home attendance per week at a Class AA baseball park varied according to the formula N(t)  = 1000(6 + 0.1t)^(1/2) where  t is the number of weeks into the season (0 £ t £ 14) and  N represents the number of people.
nt=function(t)(1000*(6+.1*t)^.5)
nt(3)
#Step 1. What was the attendance during the third week into the season?  Round your answer to the nearest whole number.

dnt=Deriv(nt)
dnt(5)
#19.61161

#Consider the following function:
Deriv(3*x^3 + 4*y^3)

#17. A frozen pizza is placed in the oven at t = 0.  The function F(t)  = 14 +  (367t^2)/(t^2  + 100)  approximates the temperature (in degrees Fahrenheit) of the pizza at time t . 

ft=function(t) 14+367*t^2/(t^2+100)
plot(ft(0:1000))
#Step 2. Over time, what temperature is the pizza approaching?
max(ft(1:30))


#18. Solving for X
fx <- function(x) x^3/3340000  -  (7 * x^2)/9475  +  42417727 * x/1265860000  +  1/33
plot(fx(0:30))
m <- max(fx(1:30))
m
fxdiv <- Deriv(fx)

fxdiv
plot(fxdiv(1:30))
uniroot(fxdiv, c(0 ,30))


#applied calc exercises 

fx <- function(x) 2 * x^3 - 15 * x^2 + 6 #x = 0 , 5 
s=seq(-10,10,by=.01)
plot(fx(s))

fxdiv <-  Deriv(fx)
plot(fxdiv)
fxdiv(0)
fxdiv(5)
fxdiv2 <- Deriv(fxdiv)
fxdiv2
plot(fxdiv2)

fxdiv2(0)
fxdiv2(5)
fxdiv2(2.5)
?uniroot(fxdiv2, c(0, 5))
max(fxdiv2(0:5))


#8. 
gx <- x^3 - 3 * x^2 - 9 * x + 7    # x = –1 , 3 .
#9.
hx <- x^4 - 8 * x^2 - 2        #x = –2, 0, 2 
#10.
fx <- x.ln(x)             #x = 1/e 


#20. Use the Second Derivative Test to find all local extrema, if the test applies.  Otherwise, use the First Derivative Test.  Write any local extrema as an ordered pair.
fx <- function(x) -6*x^3  + 27*x^2  + 180*x
fp <- function(x) 180 + x * (54 - 18 * x)
s=seq(-100,100,by=1)
plot(fp(s))
uniroot(fp, c(-100,0))
uniroot(fp, c(0,100))

fx(5.000026)
fx(-2.000014)

s=seq(-100,100,by=1)
plot(fp(s))
uniroot(fp, c(-100,0))
uniroot(fp, c(0,100))

#21.WRONG A beauty supply store expects to sell 120 flat irons during the next year.  It costs $1.60 to store one flat iron for one year.  To reorder, there is a fixed cost of $6 , plus $4.50  for each flat iron ordered.  In what lot size and how many times per year should an order be placed to minimize inventory costs? 
#x = times per year
#L = 120 / x #Lot Size
storage_cost <- function(x) 1.6 * 120 / x 
order_cost <- function(x) 4.5 * 120 / x + 6
total_cost <- function(x) x * order_cost(x) + storage_cost(x)
curve(total_cost, 1, 365)
tc_deriv <- Deriv(total_cost)
tc_deriv
curve(tc_deriv, 1, 10)
uniroot(tc_deriv, c(1, 365))$root


fx <- function(i) 540 + 6*i + 192 / i
s=seq(0,10,by=1)
plot(fx(s))
plot(fx)
fx(3)

fp <- function(i) 6 - (192/i^2)
s=seq(0,30,by=1)
plot(fp(s))
uniroot(fp, c(0,365))
fp(4)

fp(46)
fpa(46)

#22. A shipping company must design a closed rectangular shipping crate with a square base.  The volume is 18432 ft^3.  The material for the top and sides costs $3 per square foot and the material for the bottom costs $5 per square foot.  Find the dimensions of the crate that will minimize the total cost of material.   
require(Deriv)
width <- function(h, L) 18432/(L*h)
z <- function(h, L)  8*18432/(L*h)*L + 6*h*L + 6*h*18432/(L*h)


fxp <- Deriv(z)
fxp #c(L = 6 * h - 110592/L^2, h = 6 * L - 147456/h^2)

funh <- function(L) 110592/(6*L^2)
funLoffunh <- function(L) 6 * L - 147456/funh(L)^2

Lvalue1 <- uniroot(funLoffunh, c(0.00000001,18432))$root
Lvalue1
#Lvalue2 <- uniroot(funLoffunh, c(Lvalue1 + 0.00000001, 18432))$root
# only 1 L value in this domain (other L value is 0)

Hvalue1 <- funh(Lvalue1)
Hvalue1
crit <- c(Hvalue1, Lvalue1)

zL <- function(h, L)  6 * h - 110592/L^2                       
zh <- function(h, L)  6 * L - 147456/h^2   

Deriv(zL) #c(h = 6, L = 221184/L^3)
zLL <- function(L) 221184/L^3
zLh <- 6
 
Deriv(zh) # c(h = 294912/h^3, L = 6)
zhh <- function(h) 294912/h^3
zhL <- 6

D <- function(zxx, zxy, zyy, zyx) zxx * zyy - zxy * zyx
D(zhh(crit[1]), zhL, zLL(crit[2]), zLh) #108 > 0 so we have a max or min
zhh(crit[1]) # 9.000008 is greater than 0 so we have a local min

dims <- c(ceiling(crit[1]), crit[2], width(crit[1], crit[2])) #dims
dims
z(dims[1], dims[2]) #cost
prod(dims)


#23. A farmer wants to build a rectangular pen and then divide it with two interior fences.  The total area inside of the pen will be 1056 square yards.  The exterior fencing costs $14.40 per yard  and the interior fencing costs $12.00 per yard .  Find the dimensions of the pen that will minimize the cost. 
cost <- function(w) 28.8 * (1056 / w) + 52.8 * w
cder <- Deriv(cost)
uniroot(cder, c(0.00001,1056))
1056/24

#24. It is determined that the value of a piece of machinery declines exponentially.  
#A machine that was purchased 7 years ago for $67000 is worth $37000 today.  What will be the value of the machine 9 years from now?  
r <- -((37000/67000)^(1/7) - 1)
37000*(1 - r)^9
17244.5

#25. reversed the x and p. The demand function for a television is given by p = D(x)  = 23.2 - 0.4x dollars.
# Find the level of production for which the revenue is maximized. 
# p = qty,  x = price, Revenue = p * x
dem <- function(x) 23.2 - 0.4*x
rev <- function(x) x * dem(x)
dem(29)
s=seq(0,100,by=1)
plot(rev(s), type ="l")
max(rev(s))

d_rev <- Deriv(rev)
d_rev
a <- uniroot(d_rev, c(0, 10000))
a$root
dem(a$root)

Rev <- rev(a$root)
Rev
Rev/a$root

#26. (little off) The amount of goods and services that costs $400 on January 1, 1995  costs $426.80  on January 1, 2006 . 
#Estimate the cost of the same goods and services on  January 1, 2017.  
# Assume the cost is growing exponentially.  Round your answer to the nearest cent.
# new = orig * (1+r) ^ t

r <- (426.8/400)^(1/11)-1
r
400*(1 + r)^22
#455.3956  ans: 455.44

#27. CORRECT A manufacturer has determined that the marginal profit from the production and sale of x  clock radios is approximately 380 - 4x dollars per clock radio.   
# Step 1. Find the profit function if the profit from the production and sale of 38  clock radios is $1700. 
C <- -(-1700 +380 * 38 -2 * 38^2)
profit <- function(x) -9852 + 380 * x - 2 * x^2

# Step 2. What is the profit from the sale of 56 clock radios? 
profit(56)
5156

#28. 
library(mosaic)
fxp <- function(y) -5 * (log(y))^5 / y
curve(fxp, -100, 100)
antiD(-5 * (log(y))^5 / y ~ y) #doesn't always work


#29 CORRECT
fx <- function(x) 75 - 9 * x^.5
integrate(Vectorize(fx),0,9)
Fx <- function(x) 75 * x - 6 * x^1.5
Fx(9)
#513

#30 
s=seq(-2,2,by=.1)
fx <- function(x) 6*x^2
fx2 <- function(x) 6 * x^.5
plot(fx(s), fx2(s))
plot(s,fx(s),type="l",col="red")
lines(s,fx2(s),col="green")

fx(1) == fx2(1)

integrate(Vectorize(fx2),0,1)$value
integrate(Vectorize(fx),0,1)$value


#32
fx <- function(x) x * sqrt(x + 7)
integrate(Vectorize(fx),-7,2)

#33

total_dist <- 46
current_height <- 46
bounces <- 2
for (i in 1:(bounces - 1))
{
    current_height <- current_height * .22
    total_dist <- total_dist + current_height * 2
    #print(current_height)
    #print(total_dist)
}
total_dist
round(total_dist, 2)    

#34
fx <- function(x) 3 * exp(5 * x - 3)
Deriv(fx)
3 * e^17 + 15 * e^17 * (x-4) + 75/2 * e^17 * (x-4)^2 + 125/2 * e^17 * (x-4)^3 + 625/8 * e^17 * (x-4)^4 + 625/8 * e^17 * (x-4)^5


###############################

# deSolve Functions that solve initial value problems of a system of first-order ordinary differential equations
install.packages("deSolve")
library(deSolve)




### INDEFINITE INTEGRATION ###
#Constant Multiple Rule, int(k*f)=k*int(f)+C
library(mosaic)
antiD(5~x)
#Sum Rule (separable)
antiD(x-2+x^2~x)
#Power Rule, x^n -> (x^(n+1)/(n+1)
antiD(x^3~x)
#Exp(x)=>exp(x)+C
antiD(exp(x)~x)
#1/x=>ln|x|+C
antiD(1/x~x)

### Two (and More) Variable Problems ###
library(cubature)
fxyz=function(x) {(x[1]-2)^2+(x[2]+3)^2+(x[3]-4)^2-9}
adaptIntegrate(fxyz,lower=c(-10,-10,-10),upper=c(10,10,10))

#####SCRATCH PAPER#####
sqrt(7^2+42^2)
log(100)
(42*7)/2
MC <- function(x) 4 * x^-.5 + 2
integrate(Vectorize(MC),4,5)


fx(5000) - fx(4000)

MC <- function(x) (4 * x)/sqrt(7 + x^2)+ 2
integrate(Vectorize(MC),4,5)
exp(1)^2
exp(2)


#Supply and demand functions
dq <- function(q)  -.8 * q + 150
sq <- function(q) 5.2 *q


#visualize intersection
s = seq(0,30,by=1)
plot(s,sq(s),type="l",col="red")
lines(s,dq(s),col="green")

# find (q*, p*)
poq <- function(q) sq(q) - dq(q)
q <- uniroot(poq, c(0,30))
c(q$root,dq(q$root))

# consumer & producer surpluses
a <- integrate(Vectorize(dq),0,q$root)$value
a  -  q$root * dq(q$root)

a2 <- integrate(Vectorize(sq),0,q$root)$value
q$root * dq(q$root) - a2

#Continuous Income Stream
inc <- function(t) 75000 * exp(-.028 * t)
integrate(Vectorize(inc), 0, 8)

#p219
y1 <- function(t) 7000 * exp(-.017 * t) 
yrs <- function(t)  (6200 + 800 * t) * exp(-.017 * t)
integrate(Vectorize(y1), 0, 1)$value + integrate(Vectorize(yrs), 1, 8)$value

fx <- function(x) 5000 * exp(.08 * x)
require(Deriv)
Deriv(fx)

#cH 3 FUNCTIONS OF 2 VARIABLES p259
fxy <- function(x, y) exp(x + y)/(y^3 + y) + y * log(y)
Deriv(fxy)

fxyzw <- function(x, y, z, w) 35 * x^2 * w - 1/z + y * z^2
Deriv(fxyzw)
require(cubature)
?adaptIntegrate

sin(0)
