x = c(1, 2, 3, 4, 5)
y = c(2, 10, 34, 80, 154)

negyzet = function(x, num){
	s = 0;
	for(i in 1 : length(x))
		s = s + x[i]**num;
		print(s)
}

vegyes = function(x, y, num){
	s = 0;
	for(i in 1 : length(x))
		s = s + y[i]* x[i]**num;
		print(s)
}

nevezo <- matrix(c(negyzet(x, 6), negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), 
			 negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), negyzet(x, 2),
 			 negyzet(x, 4), negyzet(x, 3), negyzet(x, 2), negyzet(x, 1),
			 negyzet(x, 3), negyzet(x, 2), negyzet(x, 1), length(x)),
                   nrow = 4, ncol = 4)

szam1 <- matrix(c(vegyes(x, y, 3), vegyes(x, y, 2), vegyes(x, y, 1), negyzet(y, 1), 
			negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), negyzet(x, 2),
 			negyzet(x, 4), negyzet(x, 3), negyzet(x, 2), negyzet(x, 1),
			negyzet(x, 3), negyzet(x, 2), negyzet(x, 1), length(x)),
                  nrow = 4, ncol = 4)

szam2 <- matrix(c(negyzet(x, 6), negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), 
			vegyes(x, y, 3), vegyes(x, y, 2), vegyes(x, y, 1), negyzet(y, 1),
 			negyzet(x, 4), negyzet(x, 3), negyzet(x, 2), negyzet(x, 1),
			negyzet(x, 3), negyzet(x, 2), negyzet(x, 1), length(x)),
                  nrow = 4, ncol = 4)

szam3 <- matrix(c(negyzet(x, 6), negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), 
			negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), negyzet(x, 2),
 			vegyes(x, y, 3), vegyes(x, y, 2), vegyes(x, y, 1), negyzet(y, 1),
			negyzet(x, 3), negyzet(x, 2), negyzet(x, 1), length(x)),
                  nrow = 4, ncol = 4)

szam4 <- matrix(c(negyzet(x, 6), negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), 
			negyzet(x, 5), negyzet(x, 4), negyzet(x, 3), negyzet(x, 2),
 			negyzet(x, 4), negyzet(x, 3), negyzet(x, 2), negyzet(x, 1),
			vegyes(x, y, 3), vegyes(x, y, 2), vegyes(x, y, 1), negyzet(y, 1)),
                  nrow = 4, ncol = 4)

a3 = det(szam1) / det(nevezo) 
a2 = det(szam2) / det(nevezo)
a1 = det(szam3) / det(nevezo)
a0 = det(szam4) / det(nevezo)

print(paste("a3 szám:", round(a3)))
print(paste("a2 szám:", a2))
print(paste("a1 szám:", a1))
print(paste("a0 szám:", a0))
