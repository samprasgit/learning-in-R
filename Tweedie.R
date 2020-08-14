#Tweedie回归
#Teweedie分布是泊松分布和伽马分布的复合分布，可以理解为损失次数服从泊松分布、每次的损失金额服从伽马分布假设下的累积损失分布。
set.seed(123)
n=500
A=gl(2,3,length= n)
B=gl(3,3,length = n)
beta1=c(0.2,-0.2,-0.3,.4)
beta2=c(7,0.3,-0.4,0.6)
mu1=exp(model.matrix(~A+B)%*%as.matrix(beta1))
mu2=exp(model.matrix(~A+B)%*%as.matrix(beta2))
ee=round(runif(n,min=0.5,max = 1.5),1)
num=rpois(n,mu1*ee)
cost=NULL
for (i in 1:n) {
  shape=2
  scale=mu2[i]/shape
  cost[i]=ifelse(num[i]==0,0,sum(rgamma(num[i],shape = shape,scale=scale)))
    cost[i]=round(cost[i])             
                 
}
sev=round(ifelse(num==0,0,cost/num))
dt=data.frame(A,B,ee,num,sev,cost)

tw1=cpglm(cost~A+B+offset(log(ee)),data=dt,control = list(bound.p=c(1.333,1.333)))
