% This is material illustrating the methods from the book
% Financial Modelling  - Theory, Implementation and Practice with Matlab
% source
% Wiley Finance Series
% ISBN 978-0-470-74489-5
%
% Date: 02.05.2012
%
% Authors:  Joerg Kienitz
%           Daniel Wetterau
%
% Please send comments, suggestions, bugs, code etc. to
% kienitzwetterau_FinModelling@gmx.de
%
% (C) Joerg Kienitz, Daniel Wetterau
% 
% Since this piece of code is distributed via the mathworks file-exchange
% it is covered by the BSD license 
%
% This code is being provided solely for information and general 
% illustrative purposes. The authors will not be responsible for the 
% consequences of reliance upon using the code or for numbers produced 
% from using the code. 
function [pathS,pathV] = MC_QE_m(S0,r,d,T,Vinst,Vlong,kappa,epsilon,rho,NTime,NSim,NBatches)
% discretization for the Heston model
% using QE scheme and martingale correction
dT = T/NTime;                             % time step
pathS = zeros(NSim,NTime+1,NBatches);     % output pathS   
pathV = zeros(NSim,NTime+1,NBatches);     % output pathV
lnS1 = zeros(NSim,NTime+1);               % logspot price path
lnS1(:,1)=log(S0*exp(-d*T));              % set S(0) adjust with dividend
V2 = zeros(NSim,NTime+1);                 % variance path
V2(:,1) = Vinst;                          % set V0
k1 = exp(-kappa*dT);
k2 = epsilon^2*k1.*(1-k1)/kappa;
k3 = exp(kappa*dT)*0.5.*k2.*(1-k1).*Vlong;
psiC = 1.5;                     % psi in (1,2)
gamma1 = .5;                    % for PredictorCorrector
gamma2 = .5;                    % for PredictorCorrector
c1 = (r-d)*dT;                  % adjustment due to drift
K1 = gamma1*dT*(kappa*rho/epsilon - .5)-rho/epsilon; % K1
K2 = gamma2*dT*(kappa*rho/epsilon - .5)+rho/epsilon; % K2
K3 = gamma1*dT*(1-rho^2);                            % K3
K4 = gamma2*dT*(1-rho^2);                            % K4
A= K2+0.5*K4;                   % further adjustment
for l= 1:NBatches
    UV1 = rand(NSim,NTime);         % uniforms
    dW2 = randn(NSim,NTime);        % Gaussians             
    K0 = zeros(NSim,1);             % K0 for martingale adjust
    for i=2:NTime+1                 % time loop
        m = Vlong + (V2(:,i-1)-Vlong)*k1; % mean (moment matching)
        s2 = V2(:,i-1)*k2 + k3;           % var (moment matching)
        psi = s2./m.^2;                   % psi compared to psiC               
       
        psihat = 1./psi;                                    
        b2 = 2*psihat - 1 + sqrt(2*psihat.*(2*psihat-1));  
        a = m ./ (1 + b2);                                  
    
        I1 = find(psi<=psiC);% Non-Cent Chi^2 approx for psi < psiC
        I2 = ~I1;
        if isempty(I1)
        else
            V2(I1,i) = a(I1).*(sqrt(b2(I1)) + norminv(UV1(I1,i-1))).^2;                % Non-central chi squared approximation
        end
        p = (psi - 1)./(psi + 1);               % for switching rule   
        V2((UV1(:,i-1)<=p) & (psi>psiC),i) = 0; % case u<= p & psi>psiC
        I1b = find((UV1(:,i-1)>p) & (psi>psiC));% find is faster here!
    
        beta = (1 - p)./m;                      % for switching rule
        if isempty(I1b)
        else    % Psi^(-1)
            V2(I1b,i) = log((1-p(I1b))./(1-UV1(I1b,i-1)))./beta(I1b);  %Psi^(-1)    
        end
        % K0 for martingale adjustment
        K0(I1) = c1-A*b2(I1).*a(I1)./(1-2*A*a(I1)) + 0.5 * log(1-2*A*a(I1));
        K0(I2) = c1-log(p(I2)+beta(I2).*(1-p(I2))./(beta(I2)-A));
      
        % log Euler Predictor-Corrector step
        lnS1(:,i) = lnS1(:,i-1) + K0 - (K1+0.5*K3).*V2(:,i-1)+ K1.*V2(:,i-1) + K2.*V2(:,i) ... 
                  + sqrt(K3.*V2(:,i-1) + K4.*V2(:,i)).*dW2(:,i-1);  
    end
    pathS(:,:,l) = exp(lnS1);
    pathV(:,:,l) = V2;
end
