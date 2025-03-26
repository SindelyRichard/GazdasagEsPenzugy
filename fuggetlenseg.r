        K <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow = 3,ncol=4)
        N = sum(K)
        print(K)
        ki = 0  

        sor = function(K){

            rows<-nrow(K)
            cols<-ncol(K)
            sum_rows<-numeric(rows)
        
            for (i in 1:rows) {      
            sum_row = 0 
            for(j in 1:cols){
                sum_row = sum_row + K[i,j]  
            }
             sum_rows[i]<-sum_row
            }
            return(sum_rows)    
        }

        oszlop = function(K){
            rows<-nrow(K)
            cols<-ncol(K)   
            sum_cols<-numeric(cols)
            for (i in 1:cols) {      
                sum_col = 0 
                for(j in 1:rows){
                    sum_col = sum_col + K[j,i]  
                }
                sum_cols[i]<-sum_col
            }
                return(sum_cols)    
        }

        x = function(K){
            rows<-nrow(K)
            cols<-ncol(K)
            s = 0;

            sor_sum <- sor(K)
            oszlop_sum <- oszlop(K)
        
            for (i in 1:rows) {
                for(j in 1:cols){
                    s = s + (((K[i,j] - (sor_sum[i] * oszlop_sum[j]) / N)^2) / ((sor_sum[i] * oszlop_sum[j]) / N))
                }       
            }
            return(s)
        }

        qp = function(x){
            s <- numeric(length(x))
            for (i in 1:length(x)) {
                s[i] = x[i]/N
            }
            return(s)
        }
        print(qp(sor(K)))
        print(qp(oszlop(K)))

        print(x(K))
