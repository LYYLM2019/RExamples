function x1=sim_binding_Vasicek()
kappa=[0.01:0.01:0.2 0.21:0.1:0.51];
nob=432;
nos=10000;
delta=1/12;
phi=exp(-kappa*delta);
sigmaep=sqrt(0.5*(1-exp(-2*kappa*delta))./kappa);
sigmam=sqrt(0.5./kappa);
nn=length(kappa);
x1=zeros(nn,3);
kappahat=zeros(nos,1);
for i=1:nn
    for k=1:nos;
        randn('seed',k);
        ep=normrnd(0,1,nob,1);
        ep(1)=ep(1)*sigmam(i);
        ep(2:nob)=ep(2:nob)*sigmaep(i);
        y=filter(1, [1 -phi(i)], ep);
        yx=y(1:nob-1)-mean(y(1:nob-1));
        yy=y(2:nob)-mean(y(2:nob));
        phihat=sum(yx.*yy)/sum(yx.^2);
        kappahat(k)=-log(phihat)/delta;
    end
    x1(i,1:3)=[kappa(i) mean(kappahat) median(kappahat)];
end
x1=x1;
y=load('ff.txt');
yx=y(1:nob-1)-mean(y(1:nob-1));
yy=y(2:nob)-mean(y(2:nob));
phihat=sum(yx.*yy)/sum(yx.^2);
kappahat=-log(phihat)/delta
zz=x1(:,1);
bind1=x1(:,2);
bind2=x1(:,3);
II1=interp1(bind1,zz,kappahat,'linear','extrap');
II2=interp1(bind2,zz,kappahat,'linear','extrap');
x1=[II1 II2];
return
