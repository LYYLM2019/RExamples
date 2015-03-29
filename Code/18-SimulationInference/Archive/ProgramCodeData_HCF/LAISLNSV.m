function LAISLNSV()
%SML of the basic SV model
global data
global nos
global nseed
data=load('svpd1.txt');
para=[1    0.1920    0.9308];
nos=512
nseed=1;
param = fminsearch(@loglkhd_copulasv_mod_ggi,para,optimset('MaxFunEvals',1e+6,'MaxIter',1e+6,'Display','iter'),5)
return

function value=loglkhd_copulasv_mod_ggi(para,noi)
%Calculate log-likelihood of the basic SV model with  
%using importance sampling 
%%para(1)-sigma para(2)-gamma para(3)-phi,
global nos%number of importance samplers
global data
global h
global z
global nseed
n=length(data);
h=zeros(n+1,1);%column vector
z=data./(para(1)*exp(0.5*h(1:n)));
for i=1:noi
    Grad=Gr(para);
    Hes=Om(para);
    temp=Hes\Grad;
    h=h-temp;
    z=data./(para(1)*exp(0.5*h(1:n)));
end
h0=h;
Hes=Om(para);
cho=chol(-Hes);
randn('seed',nseed);
w=mvnrnd(zeros(1,n+1),eye(n+1),nos);%each row follows a N+1-dimen mulvar normal distribution; there are NOS rows
t0=diag(Hes);
t1=diag(Hes,-1);
par1=para(1)*para(1);
par2=para(2)*para(2);
par3=para(3)*para(3);
temp=-log(par2/(1-par3))-n*log(par1*par2)-log_det_tri(-t0,-t1,-t1);
v=w;
foverg=zeros(nos,1);
for k=1:nos
    pq=cho\w(k,1:n+1)';%produce (n+1) by 1 vector
    v(k,1:n+1)=h0'+pq';
    foverg(k)=-sum(v(k,1:n))+2*sum(log(normpdf(data./(para(1)*exp(0.5*v(k,1:n)')))))-v(k,1)*v(k,1)*(1-par3)/par2-sum((v(k,2:n)-para(3)*v(k,1:(n-1))).^2)/par2-pq'*Hes*pq;
end
foverg=0.5*(foverg+temp);
value=-log(mean(exp(foverg-mean(foverg))))-mean(foverg); 
%
function y=Gr(para)%gradient of log f(h)
global data
global h
global z
par2=para(2)*para(2);
par3=para(3)*para(3);
n=length(data);
y(1)=(para(3)*h(2)-h(1))/par2-0.5+0.5*z(1)*z(1);
y(2:n)=-0.5+0.5*z(2:n).*z(2:n)+(para(3)*h(3:n+1)-(1+par3)*h(2:n)+para(3)*h(1:n-1))/par2;
y(n+1)=-(h(n+1)-para(3)*h(n))/par2;
y=y';
return;
%%
function y=Om(para)%Hessian of log f, a tridiagonal matrix
global data
global h
global z
par2=para(2)*para(2);
par3=para(3)*para(3);
n=length(data);
nn=n+1;
t(1)=-1/par2-0.5*z(1)^2;
t(2:n)=-(1+par3)/par2-0.5*z(2:n).^2;
t(n+1)=-1/par2;
d=sparse(1:nn,1:nn,t,nn,nn);
e=sparse(2:nn,1:n,ones(1,n)*para(3)/par2,nn,nn);
y=e+d+e';
return;
%
function x=log_det_tri(a,b,c)
n=length(a);
d=zeros(n,1);
d(1)=a(1);
x=log(a(1));
for i=2:n
    d(i)=a(i)-b(i-1)*c(i-1)/d(i-1);
    x=x+log(d(i));
end
return
