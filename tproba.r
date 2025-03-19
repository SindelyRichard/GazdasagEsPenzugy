y = c(1,2,3,4)
x = c(5,6,7,8)
n = length(x)
m = length(y)

xAtlag = sum(x)/n
yAtlag = sum(y)/m

Sn = 0
for (i in 1:n) {
   Sn = Sn+((x[i]-xAtlag)**2)
} 
Sn = Sn*(1/(n-1)    )

Sm = 0
for (i in 1:m) {
   Sm = Sm+((y[i]-yAtlag)**2)
} 
Sm = Sm*(1/(m-1))

t=((xAtlag-yAtlag)/(sqrt((n-1)*Sn+(m-1)*Sm)))*sqrt((n*m*(n+m-2))/(n+m))
print(t)