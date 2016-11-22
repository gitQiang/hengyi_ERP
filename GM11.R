#��ɫԤ��ģ��G(1,1)
# http://www.cnblogs.com/homewch/p/5783073.html
GM11<-function(x0,t, plot=FALSE,print=FALSE){ #x0Ϊ����ѧ�У�tΪԤ�����
        x1<-cumsum(x0) #һ���ۼ���������1-AG0����(�ۼ�����)
        b<-numeric(length(x0)-1)# ��ʼ������Ϊlength(x0)-1���������ָ�numeric������ֵΪ0�����ݼ�b
        n<-length(x0)-1# n Ϊlength(x0)-1���� ��Ϊ��Ҫ����MEAN�����ھ�ֵ���������� �䳤����1
        for(i in 1:n){ #����x1�Ľ��ھ�ֵ��������
                b[i]<--(x1[i]+x1[i+1])/2 
                b} #������b����Ϊx1�Ľ��ھ�ֵ��������
        D<-numeric(length(x0)-1)
        D[]<-1
        B<-cbind(b,D)#��B����
        BT<-t(B)#B����ת��
        M<-solve(BT%*%B)#��BT*B����
        YN<-numeric(length(x0)-1)
        YN<-x0[2:length(x0)]
        alpha<-M%*%BT%*%YN  #ģ�͵���С���˹��Ʋ���������alpha��
        alpha2<-matrix(alpha,ncol=1)# ��������һ��
        # �õ����̵�����ϵ��
        a<-alpha2[1]
        u<-alpha2[2]
        
        # ����Ϊ������
        # �����������ֵ��ģ��ֵ
        y<-numeric(length(c(1:t)))# tΪ������Ԥ�����
        y[1]<-x1[1] # ��һ��������
        for(w in 1:(t-1)){  #��a,u�Ĺ���ֵ����ʱ����Ӧ���к�������x1�������y
                y[w+1]<-(x1[1]-u/a)*exp(-a*w)+u/a 
        }
        xy<-numeric(length(y))
        xy[1]<-y[1]
        for(o in 2:t){ #���ú�����㻹ԭ��ģ����������x0Ԥ������
                xy[o]<-y[o]-y[o-1] 
        } 
        
        if(print){
                cat("GM(1,1)��������ֵ��",'\n',"��չϵ��-a=",-a,"  ","��ɫ������u=",u,'\n','\n') #������С���˷���ò�������ֵa,u
                cat("x(1)��ģ��ֵ��",'\n',y,'\n')
                cat("x(0)��ģ��ֵ��",'\n',xy,'\n','\n')                       
        }
        
        # ����в�e
        e<-numeric(length(x0))
        for(l in 1:length(x0)){
                e[l]<-x0[l]-xy[l] #�òв����У�δȡ����ֵ��
        }
        #����������
        e2<-numeric(length(x0))
        for(s in 1:length(x0)){
                e2[s]<-(abs(e[s])/x0[s]) #��������
        }
        
        if(print){
                cat("���Բв",'\n',e,'\n')
                cat("��Բв",'\n',e2,'\n','\n')
                cat("�в�ƽ����=",sum(e^2),'\n')
                cat("ƽ��������=",sum(e2)/(length(e2)-1)*100,"%",'\n')
                cat("��Ծ���=",(1-(sum(e2)/(length(e2)-1)))*100,"%",'\n','\n')
        }
        
        
        #������ֵ����
        avge<-mean(abs(e));esum<-sum((abs(e)-avge)^2);evar=esum/(length(e)-1);se=sqrt(evar)  #����в�ľ�����se
        avgx0<-mean(x0);x0sum<-sum((x0-avgx0)^2);x0var=x0sum/(length(x0));sx=sqrt(x0var)  #����ԭ����x0�ķ���sx
        cv<-se/sx  #������ֵ������ȣ�
        #����С�в����
        P<-sum((abs(e)-avge)<0.6745*sx)/length(e)
        
        if(print){
                cat("������ֵ����:",'\n',"Cֵ=",cv,'\n')#�Ժ�����ֵ���м��飬��һ���׼���бȽ��ж�Ԥ�����û���
                cat("С�в����:",'\n',"Pֵ=",P,'\n')
                if(cv < 0.35 && P>0.95){     
                        cat("C<0.35, P>0.95,GM(1,1)Ԥ�⾫�ȵȼ�Ϊ����",'\n','\n')
                }else{
                        if(cv<0.5 && P>0.80){
                                cat("Cֵ����[0.35,0.5), P>0.80,GM(1,1)ģ��Ԥ�⾫�ȵȼ�Ϊ���ϸ�",'\n','\n')
                        }else{
                                if(cv<0.65 && P>0.70){
                                        cat("Cֵ����[0.5,0.65), P>0.70,GM(1,1)ģ��Ԥ�⾫�ȵȼ�Ϊ����ǿ�ϸ�",'\n','\n')
                                }else{
                                        cat("Cֵ>=0.65, GM(1,1)ģ��Ԥ�⾫�ȵȼ�Ϊ�����ϸ�",'\n','\n')
                                }
                        }
                }
        }
        
        if(plot){
                #������������x0��Ԥ�����м�x0�ıȽ�ͼ��
                plot(xy,col='blue',type='b',pch=16,xlab='ʱ������',ylab='ֵ',ylim=c(0.9*min(c(x0,xy)), 1.1*max(c(xy,x0))))
                points(x0,col='red',type='b',pch=4)
                #legend("topleft",c('Ԥ��','ԭʼ'),title = "Ԥ��ʱ����ԭʼʱ��Ա�",pch=c(16,4),lty=l,col=c('blue','red'))
        }
        
        list(xy=xy, a= -a, u=u)
}