x = seq(-10,10,by=1)
yorg = x^2-25*x-10
y = yorg + runif(length(x),-20,20)
plot(x,y)
lines(x,yorg,col = "blue")

X = matrix(c(x^2,x,rep(1,length(x))),ncol = 3)
Xt = t(X)
solve(Xt%*%X)%*%Xt%*%t(t(y))

a=runif(1,0,1)
b=runif(1,0,1)
c=runif(1,0,1)
#iterative solution
alpha = 1e-5
N=length(x)

for (i in 1:100000) {
  sumv=0
  td_a=0
  td_b=0
  td_c=0
  for (j in 1:N) {
    yfit = a*x[j]^2+b*x[j]+c
    td_a = td_a+2*(yfit-y[j])*x[j]^2
    td_b = td_b+2*(yfit-y[j])*x[j]
    td_c = td_c+2*(yfit-y[j])
    sumv = sumv+(yfit-y[j])^2
  }
  print("------------------")
  print(td_a)
  print(td_b)
  print(td_c)
  print(paste("sumv:",sumv,seq=""))
  a=a-alpha*td_a
  b=b-alpha*td_b
  c=c-alpha*td_c
}
###############################################
vnew = runif(3,-20,20)
N = length(x)
for (i in 1:2) {
  sumv = 0
  td_vc = c(0,0,0)
  td_mt = matrix(rep(0,9),ncol = 3)
  for (j in 1:N) {
    yfit = vnew[1]*x[j]^2+vnew[2]*x[j]+vnew[3]
    td_vc = td_vc + c(2*(yfit - y[j])*x[j]^2,2*(yfit-y[j])*x[j],2*(yfit - y[j]))
    td_mt = td_mt + 2*matrix(c(x[j]^4,x[j]^3,x[j]^2,x[j]^3,x[j]^2,x[j],x[j]^2,x[j],1),ncol = 3)
    sumv=sumv+(yfit-y[j])
  }
  vnew = vnew-solve(td_mt)%*%td_vc
}

out = split(iris,iris$Species)
out
out[1]
out[[1]]
aggregate(iris[,1:4],by=list(iris$Species),mean)
aggregate(cbind(Sepal.Length,Sepal.Width)~Species,iris,mean)
a0 = array(1:10,c(2,5))
a0
apply(a0,1,sum)
devtools::install_github("datacamp/RDocumentation")
library(RDocumentation)