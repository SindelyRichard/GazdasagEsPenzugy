avg<-function(x){
s = 0
n = length(x)
for(i in 1:n){
s = s+x[i]
}
return(s/n)
}
u<-function(x,m,szoras){
return((m-avg(x))/(sqrt(length(x)))/szoras)
}
szoras = 1
p = 0.95
x = c(1,2,3,4,5)
avg(x)
m0 = 4
u(x,m0,szoras)
interval = c(-(p+1.1),p+1.1)
x_vals <- seq(-10, 10, length.out = 1000)
y_vals <- dnorm(x_vals, mean = m0, sd = szoras)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2, 
     xlab = "x", ylab = "Valószínűségi sűrűség", 
     main = "Standard normál eloszlás és az u próbája")
abline(v = u(x,m0,szoras), col = "red", lwd = 2, lty = 2) 
abline(v=interval[1], col = "blue", lwd = 2, lty = 2)
abline(v=interval[2], col = "blue", lwd = 2, lty = 2)
