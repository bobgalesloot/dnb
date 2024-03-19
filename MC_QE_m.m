{\rtf1\ansi\ansicpg1252\cocoartf2759
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 Menlo-Regular;}
{\colortbl;\red255\green255\blue255;\red15\green112\blue16;\red25\green25\blue25;\red8\green0\blue255;
}
{\*\expandedcolortbl;;\cssrgb\c0\c50196\c7451;\cssrgb\c12941\c12941\c12941;\cssrgb\c5490\c0\c100000;
}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs28 \cf2 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 % This is material illustrating the methods from the book\cf3 \strokec3 \
\cf2 \strokec2 % Financial Modelling  - Theory, Implementation and Practice with Matlab\cf3 \strokec3 \
\cf2 \strokec2 % source\cf3 \strokec3 \
\cf2 \strokec2 % Wiley Finance Series\cf3 \strokec3 \
\cf2 \strokec2 % ISBN 978-0-470-74489-5\cf3 \strokec3 \
\cf2 \strokec2 %\cf3 \strokec3 \
\cf2 \strokec2 % Date: 02.05.2012\cf3 \strokec3 \
\cf2 \strokec2 %\cf3 \strokec3 \
\cf2 \strokec2 % Authors:  Joerg Kienitz\cf3 \strokec3 \
\cf2 \strokec2 %           Daniel Wetterau\cf3 \strokec3 \
\cf2 \strokec2 %\cf3 \strokec3 \
\cf2 \strokec2 % Please send comments, suggestions, bugs, code etc. to\cf3 \strokec3 \
\cf2 \strokec2 % kienitzwetterau_FinModelling@gmx.de\cf3 \strokec3 \
\cf2 \strokec2 %\cf3 \strokec3 \
\cf2 \strokec2 % (C) Joerg Kienitz, Daniel Wetterau\cf3 \strokec3 \
\cf2 \strokec2 % \cf3 \strokec3 \
\cf2 \strokec2 % Since this piece of code is distributed via the mathworks file-exchange\cf3 \strokec3 \
\cf2 \strokec2 % it is covered by the BSD license \cf3 \strokec3 \
\cf2 \strokec2 %\cf3 \strokec3 \
\cf2 \strokec2 % This code is being provided solely for information and general \cf3 \strokec3 \
\cf2 \strokec2 % illustrative purposes. The authors will not be responsible for the \cf3 \strokec3 \
\cf2 \strokec2 % consequences of reliance upon using the code or for numbers produced \cf3 \strokec3 \
\cf2 \strokec2 % from using the code. \cf3 \strokec3 \
\
\
\
\pard\pardeftab720\partightenfactor0
\cf4 \strokec4 function \cf3 \strokec3 [pathS,pathV] = MC_QE_m(S0,r,d,T,Vinst,Vlong,kappa,epsilon,rho,NTime,NSim,NBatches)\
\pard\pardeftab720\partightenfactor0
\cf2 \strokec2 % discretization for the Heston model\cf3 \strokec3 \
\cf2 \strokec2 % using QE scheme and martingale correction\cf3 \strokec3 \
\
dT = T/NTime;                             \cf2 \strokec2 % time step\cf3 \strokec3 \
\
pathS = zeros(NSim,NTime+1,NBatches);     \cf2 \strokec2 % output pathS   \cf3 \strokec3 \
pathV = zeros(NSim,NTime+1,NBatches);     \cf2 \strokec2 % output pathV\cf3 \strokec3 \
\
lnS1 = zeros(NSim,NTime+1);               \cf2 \strokec2 % logspot price path\cf3 \strokec3 \
lnS1(:,1)=log(S0*exp(-d*T));              \cf2 \strokec2 % set S(0) adjust with dividend\cf3 \strokec3 \
\
V2 = zeros(NSim,NTime+1);                 \cf2 \strokec2 % variance path\cf3 \strokec3 \
V2(:,1) = Vinst;                          \cf2 \strokec2 % set V0\cf3 \strokec3 \
\
k1 = exp(-kappa*dT);\
k2 = epsilon^2*k1.*(1-k1)/kappa;\
k3 = exp(kappa*dT)*0.5.*k2.*(1-k1).*Vlong;\
\
psiC = 1.5;                     \cf2 \strokec2 % psi in (1,2)\cf3 \strokec3 \
gamma1 = .5;                    \cf2 \strokec2 % for PredictorCorrector\cf3 \strokec3 \
gamma2 = .5;                    \cf2 \strokec2 % for PredictorCorrector\cf3 \strokec3 \
\
c1 = (r-d)*dT;                  \cf2 \strokec2 % adjustment due to drift\cf3 \strokec3 \
\
K1 = gamma1*dT*(kappa*rho/epsilon - .5)-rho/epsilon; \cf2 \strokec2 % K1\cf3 \strokec3 \
K2 = gamma2*dT*(kappa*rho/epsilon - .5)+rho/epsilon; \cf2 \strokec2 % K2\cf3 \strokec3 \
K3 = gamma1*dT*(1-rho^2);                            \cf2 \strokec2 % K3\cf3 \strokec3 \
K4 = gamma2*dT*(1-rho^2);                            \cf2 \strokec2 % K4\cf3 \strokec3 \
\
A= K2+0.5*K4;                   \cf2 \strokec2 % further adjustment\cf3 \strokec3 \
\
\pard\pardeftab720\partightenfactor0
\cf4 \strokec4 for \cf3 \strokec3 l= 1:NBatches\
    UV1 = rand(NSim,NTime);         \cf2 \strokec2 % uniforms\cf3 \strokec3 \
    dW2 = randn(NSim,NTime);        \cf2 \strokec2 % Gaussians             \cf3 \strokec3 \
\
    K0 = zeros(NSim,1);             \cf2 \strokec2 % K0 for martingale adjust\cf3 \strokec3 \
\
    \cf4 \strokec4 for \cf3 \strokec3 i=2:NTime+1                 \cf2 \strokec2 % time loop\cf3 \strokec3 \
        m = Vlong + (V2(:,i-1)-Vlong)*k1; \cf2 \strokec2 % mean (moment matching)\cf3 \strokec3 \
        s2 = V2(:,i-1)*k2 + k3;           \cf2 \strokec2 % var (moment matching)\cf3 \strokec3 \
        psi = s2./m.^2;                   \cf2 \strokec2 % psi compared to psiC               \cf3 \strokec3 \
       \
        psihat = 1./psi;                                    \
        b2 = 2*psihat - 1 + sqrt(2*psihat.*(2*psihat-1));  \
        a = m ./ (1 + b2);                                  \
    \
        I1 = find(psi<=psiC);\cf2 \strokec2 % Non-Cent Chi^2 approx for psi < psiC\cf3 \strokec3 \
        I2 = ~I1;\
        \cf4 \strokec4 if \cf3 \strokec3 isempty(I1)\
        \cf4 \strokec4 else\cf3 \strokec3 \
            V2(I1,i) = a(I1).*(sqrt(b2(I1)) + norminv(UV1(I1,i-1))).^2;                \cf2 \strokec2 % Non-central chi squared approximation\cf3 \strokec3 \
        \cf4 \strokec4 end\cf3 \strokec3 \
        p = (psi - 1)./(psi + 1);               \cf2 \strokec2 % for switching rule   \cf3 \strokec3 \
        V2((UV1(:,i-1)<=p) & (psi>psiC),i) = 0; \cf2 \strokec2 % case u<= p & psi>psiC\cf3 \strokec3 \
        I1b = find((UV1(:,i-1)>p) & (psi>psiC));\cf2 \strokec2 % find is faster here!\cf3 \strokec3 \
    \
        beta = (1 - p)./m;                      \cf2 \strokec2 % for switching rule\cf3 \strokec3 \
        \cf4 \strokec4 if \cf3 \strokec3 isempty(I1b)\
        \cf4 \strokec4 else\cf3 \strokec3     \cf2 \strokec2 % Psi^(-1)\cf3 \strokec3 \
            V2(I1b,i) = log((1-p(I1b))./(1-UV1(I1b,i-1)))./beta(I1b);  \cf2 \strokec2 %Psi^(-1)    \cf3 \strokec3 \
        \cf4 \strokec4 end\cf3 \strokec3 \
        \cf2 \strokec2 % K0 for martingale adjustment\cf3 \strokec3 \
        K0(I1) = c1-A*b2(I1).*a(I1)./(1-2*A*a(I1)) + 0.5 * log(1-2*A*a(I1));\
        K0(I2) = c1-log(p(I2)+beta(I2).*(1-p(I2))./(beta(I2)-A));\
      \
        \cf2 \strokec2 % log Euler Predictor-Corrector step\cf3 \strokec3 \
        lnS1(:,i) = lnS1(:,i-1) + K0 - (K1+0.5*K3).*V2(:,i-1)+ K1.*V2(:,i-1) + K2.*V2(:,i) \cf4 \strokec4 ...\cf2 \strokec2  \cf3 \strokec3 \
                  + sqrt(K3.*V2(:,i-1) + K4.*V2(:,i)).*dW2(:,i-1);  \
    \cf4 \strokec4 end\cf3 \strokec3 \
    pathS(:,:,l) = exp(lnS1);\
    pathV(:,:,l) = V2;\
\cf4 \strokec4 end}