#灰色预测模型G(1,1)
# http://www.cnblogs.com/homewch/p/5783073.html
GM11<-function(x0,t, plot=FALSE,print=FALSE){ #x0为输入学列，t为预测个数
        x1<-cumsum(x0) #一次累加生成序列1-AG0序列(累加序列)
        b<-numeric(length(x0)-1)# 初始化长度为length(x0)-1的整数部分个numeric类型且值为0的数据集b
        n<-length(x0)-1# n 为length(x0)-1长度 因为需要生成MEAN（紧邻均值）生成序列 其长度少1
        for(i in 1:n){ #生成x1的紧邻均值生成序列
                b[i]<--(x1[i]+x1[i+1])/2 
                b} #得序列b，即为x1的紧邻均值生成序列
        D<-numeric(length(x0)-1)
        D[]<-1
        B<-cbind(b,D)#作B矩阵
        BT<-t(B)#B矩阵转置
        M<-solve(BT%*%B)#求BT*B得逆
        YN<-numeric(length(x0)-1)
        YN<-x0[2:length(x0)]
        alpha<-M%*%BT%*%YN  #模型的最小二乘估计参数列满足alpha尖
        alpha2<-matrix(alpha,ncol=1)# 将结果变成一列
        # 得到方程的两个系数
        a<-alpha2[1]
        u<-alpha2[2]
        
        # 下面为结果输出
        # 输出参数估计值及模拟值
        y<-numeric(length(c(1:t)))# t为给定的预测个数
        y[1]<-x1[1] # 第一个数不变
        for(w in 1:(t-1)){  #将a,u的估计值代入时间响应序列函数计算x1拟合序列y
                y[w+1]<-(x1[1]-u/a)*exp(-a*w)+u/a 
        }
        xy<-numeric(length(y))
        xy[1]<-y[1]
        for(o in 2:t){ #运用后减运算还原得模型输入序列x0预测序列
                xy[o]<-y[o]-y[o-1] 
        } 
        
        if(print){
                cat("GM(1,1)参数估计值：",'\n',"发展系数-a=",-a,"  ","灰色作用量u=",u,'\n','\n') #利用最小二乘法求得参数估计值a,u
                cat("x(1)的模拟值：",'\n',y,'\n')
                cat("x(0)的模拟值：",'\n',xy,'\n','\n')                       
        }
        
        # 计算残差e
        e<-numeric(length(x0))
        for(l in 1:length(x0)){
                e[l]<-x0[l]-xy[l] #得残差序列（未取绝对值）
        }
        #计算相对误差
        e2<-numeric(length(x0))
        for(s in 1:length(x0)){
                e2[s]<-(abs(e[s])/x0[s]) #得相对误差
        }
        
        if(print){
                cat("绝对残差：",'\n',e,'\n')
                cat("相对残差：",'\n',e2,'\n','\n')
                cat("残差平方和=",sum(e^2),'\n')
                cat("平均相对误差=",sum(e2)/(length(e2)-1)*100,"%",'\n')
                cat("相对精度=",(1-(sum(e2)/(length(e2)-1)))*100,"%",'\n','\n')
        }
        
        
        #后验差比值检验
        avge<-mean(abs(e));esum<-sum((abs(e)-avge)^2);evar=esum/(length(e)-1);se=sqrt(evar)  #计算残差的均方差se
        avgx0<-mean(x0);x0sum<-sum((x0-avgx0)^2);x0var=x0sum/(length(x0));sx=sqrt(x0var)  #计算原序列x0的方差sx
        cv<-se/sx  #得验差比值（方差比）
        #计算小残差概率
        P<-sum((abs(e)-avge)<0.6745*sx)/length(e)
        
        if(print){
                cat("后验差比值检验:",'\n',"C值=",cv,'\n')#对后验差比值进行检验，与一般标准进行比较判断预测结果好坏。
                cat("小残差概率:",'\n',"P值=",P,'\n')
                if(cv < 0.35 && P>0.95){     
                        cat("C<0.35, P>0.95,GM(1,1)预测精度等级为：好",'\n','\n')
                }else{
                        if(cv<0.5 && P>0.80){
                                cat("C值属于[0.35,0.5), P>0.80,GM(1,1)模型预测精度等级为：合格",'\n','\n')
                        }else{
                                if(cv<0.65 && P>0.70){
                                        cat("C值属于[0.5,0.65), P>0.70,GM(1,1)模型预测精度等级为：勉强合格",'\n','\n')
                                }else{
                                        cat("C值>=0.65, GM(1,1)模型预测精度等级为：不合格",'\n','\n')
                                }
                        }
                }
        }
        
        if(plot){
                #画出输入序列x0的预测序列及x0的比较图像
                plot(xy,col='blue',type='b',pch=16,xlab='时间序列',ylab='值',ylim=c(0.9*min(c(x0,xy)), 1.1*max(c(xy,x0))))
                points(x0,col='red',type='b',pch=4)
                #legend("topleft",c('预测','原始'),title = "预测时序与原始时序对比",pch=c(16,4),lty=l,col=c('blue','red'))
        }
        
        list(xy=xy, a= -a, u=u)
}