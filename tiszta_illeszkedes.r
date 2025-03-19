k = c(11,21,13,32,13,10)
N = 100
p = c(1/6,1/6,1/6,1/6,1/6,1/6)
r=length(k)
if(N != sum(k)){
    print("N értéke hibás!")
}else{
if(sum(p)>1){
    print("A p tömb hibás!")
}   else{
    x = 0
    for (i in 1:r) {
        x = x+(((k[i]-N*p[i])**2)/(N*p[i]))
        }
    print(x)
    alfa = 0.05
    kritikus_ertek = qchisq(1 - alfa,r-1)
    print(paste("Kritikus érték:",  kritikus_ertek))
    if(x<=kritikus_ertek){  
        print("Elfogadjuk")
    }else{
        print("Elutasítjuk")
    }
}
}