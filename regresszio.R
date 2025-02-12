vegyes<-function(x,y){
s = 0
for(i in 1:length(x)){
s = x[i]*y[i]
}
return(s)
}
osszeadas<-function(x){
s = 0
for (i in 1:length(x)){
s = s+x[i]
}
return(s)

}
x = c(1,2,3,4)
y = c(1.1,1.9,3,4.1)
a = (length(x)*vegyes(x,y)-osszeadas(x)*osszeadas(y))/(length(x)*vegyes(x,x)-osszeadas(x)^2)
b = (vegyes(x,x)*osszeadas(y)- vegyes(x,y)*osszeadas(x))/(length(x)*vegyes(x,x)-osszeadas(x)^2)

a
b