function out = fh_ss(xx)
global psi gamma taua taur tauw taui za zm z;
global mu_u mu_r mu_f ;
global abar beta delta alpha_s alpha_a alpha_a1 alpha_s1 alpha_s2 alpha_m ;
global dr ds dl pm pst zs;
global r wf w cur cf bur kf hu hr ha hs km hm ;
global pif pis cs_a cs_m cs_s BB OP;

ps = xx(1);
pa = xx(2);

wfguess = log(wf) ;

cs_a = 1/((1+gamma+psi)*pa*(1+taua)) ;
cs_m = gamma/((1+gamma+psi)*pm*(1+taua)) ;
cs_s = psi/((1+gamma+psi)*ps) ;

r = 1/beta - 1;
kappa_m = exp((1/(alpha_m-1))*log((r+delta)/(alpha_m*zm*(1-taui)))) ;
w = (1-alpha_m)*zm*(1-taui)*kappa_m^alpha_m ;

cf_A = log(1/beta-1+delta-taur*delta) - log(1-taur) - ...
    log((1-alpha_s1-alpha_s2)*(pst*z)) - alpha_s1*log(dl);
cf_B = log((1-alpha_a1)*pa*za) + alpha_a1*log(ds) ;
cf_C = log(alpha_s2*pst*z) + alpha_a1*log(dl);
cf_D = log((1-alpha_a)*pa*za) + alpha_a*log(dr) - log(1-tauw) ;
cf_E = 1 - alpha_s2 +...
    (alpha_s2*(alpha_s1+alpha_s2-1))/(alpha_s1+alpha_s2) ;
cf_F = cf_C + ((alpha_s1+alpha_s2-1)/(alpha_s1+alpha_s2))*cf_A ;

fh   = @(u) u + alpha_a*log(1-mu_f/mu_r*...
      (exp((1/alpha_a1)*(cf_B-u))+...
      exp((1/cf_E)*(cf_F-u)))) - cf_D ;
ulog = fzero(fh, wfguess) ;
wf   = exp(ulog) ;

ha = exp((1/alpha_a1)*(cf_B-ulog)) ;
hs = exp((1/cf_E)*(cf_F-ulog)) ; 
hr = mu_f*(ha+hs)/mu_r ;
kf = (alpha_s2*log(hs) - cf_A)/(alpha_s1+alpha_s2) ;
kf = exp(kf);

tmp = log((1-tauw)*w) - log((1-alpha_s)*ps*zs);
hu  = 1-exp(-tmp/alpha_s) ; 
hm  = mu_u*hu ;
km  = kappa_m*hm ;
clear tmp;

pis = pst*z*(dl^alpha_s1)*(hs^alpha_s2)*(kf^(1-alpha_s1-alpha_s2))...
        - wf*hs;
pif = pa*za*(ds^alpha_a1)*(ha^(1-alpha_a1)) - wf*ha ;
cf  = (1-taur)*(pif+pis) - delta*(1-taur)*kf ;

bur = km ;
BB(1)   = zm*(km^alpha_m)*(hm^(1-alpha_m)) + mu_f*(pis+wf*hs) -...
        delta*(km+mu_f*kf) - cs_m*mu_f*cf + cs_m*pa*(1+taua)*abar ;
BB(2)   = mu_f*taur*(pif+pis) + tauw*(mu_u*w*hu + mu_r*wf*hr)...
        + taui*zm*(km^alpha_m)*(hm^(1-alpha_m)) - mu_f*taur*delta*kf ...
        + taua*pa*cs_a*mu_f*cf + taua*pa*abar*(1-cs_a*pa*(1+taua)) ...
        - taua*(1+taua)*cs_m*pa*abar + taua*cs_m*mu_f*cf ;
BB(3)   = (1-tauw)*w*hu + ps*zs*(1-hu)^(1-alpha_s) ;
BB(4)   = (1-tauw)*wf*hr + pa*za*(dr^alpha_a)*(1-hr)^(1-alpha_a) ;
BB(5)   = km ;
cur     = mu_u*BB(3) + mu_r*BB(4) + r*km ;

dm_a = cs_a*(cur + mu_f*cf) + abar*(1-cs_a*pa*(1+taua)) ;
su_a = mu_r*za*(dr^alpha_a)*(1-hr)^(1-alpha_a) + ...
    mu_f*za*(ds^alpha_a1)*(ha^(1-alpha_a1));

dm_s = cs_s*(cur + mu_f*cf) - cs_s*pa*(1+taua)*abar ;
su_s = mu_u*zs*(1-hu)^(1-alpha_s) ;

diff_a = dm_a - su_a ;
diff_s = dm_s - su_s ;

out = [diff_s, diff_a];

end