    x = c(1,2,3,4)
    y = c(5,2.3,3.2,7)
    xnull=5

    s=0
    for(i in 1:length(x)){
        p=1
        for(j in 1:length(x)){
            if(i!=j){
                p = (xnull-x[j])/(x[i]-x[j])*p
            }
        }
        s = s+y[i]*p
    }

    print(s)    