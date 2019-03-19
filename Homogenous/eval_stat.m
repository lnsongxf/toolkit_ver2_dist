[eprice fval] = fsolve(@fh_ss,[ps,pa]);

ps = eprice(1);
pa = eprice(2);

gov = BB(2) + taua*(pa*cs_a + cs_m)*cur ;

cur_a = cs_a*(cur-pa*(1+taua)*abar) + abar ;
cur_m = cs_m*(cur-pa*(1+taua)*abar) ;
cur_s = cs_s*(cur-pa*(1+taua)*abar) ;

cf_a = cs_a*(cf-pa*(1+taua)*abar) + abar ;
cf_m = cs_m*(cf-pa*(1+taua)*abar) ;
cf_s = cs_s*(cf-pa*(1+taua)*abar) ;

ca     = cur_a + mu_f*cf_a ;
cm     = cur_m + mu_f*cf_m ;
cs     = cur_s + mu_f*cf_s ;
aggcon = pa*ca + cm + ps*cs;

cshare_a = pa*ca/aggcon ;
cshare_m = cm/aggcon ;
cshare_s = ps*cs/aggcon ;

ys     = ps*mu_u*zs*(1-hu)^(1-alpha_s) ;
ya     = pa*(mu_r*za*(dr^alpha_a)*(1-hr)^(1-alpha_a) + ...
        mu_f*za*(ds^alpha_a1)*(ha^(1-alpha_a1)));
ym     = zm*(km^alpha_m)*(hm^(1-alpha_m));
yx     = mu_f*pst*z*(dl^alpha_s1)*(hs^alpha_s2)*(kf^(1-alpha_s1-alpha_s2));
agginv = delta*(km+mu_f*kf) ;

gdp_c = aggcon + agginv + gov ;
gdp_y = ya + ym + ys + yx ;

yshare_a = ya/gdp_y ;
yshare_m = ym/gdp_y ;
yshare_s = ys/gdp_y ;
yshare_x = yx/gdp_y ;

ptax   = tauw*(mu_u*w*hu + mu_r*wf*hr) ;
vtax   = taua*pa*ca + taua*cm ;
btax   = taui*ym + mu_f*taur*(pif+pis) ;
aggtax = ptax + vtax + btax ;

tshare_v = vtax/aggtax ;
tshare_p = ptax/aggtax ;
tshare_b = btax/aggtax ;

tyratio  = aggtax/gdp_y ;

yshare_a_str = ['Agri in Y      ' '  za       ' ...
     num2str(za, '%6.4f') '    ' num2str(yshare_a,'%6.4f') ...
    '    ' num2str(yshare_a_data, '%6.4f')...
    '     ' num2str(yshare_a - yshare_a_data,'%6.4f')];
yshare_m_str = ['Manu in Y      ' '  zm       ' ...
     num2str(zm, '%6.4f') '    ' num2str(yshare_m,'%6.4f') ...
    '    ' num2str(yshare_m_data, '%6.4f')...
    '     ' num2str(yshare_m - yshare_m_data,'%6.4f')];
yshare_x_str = ['Exp in Y       ' '  z        ' ...
     num2str(z, '%6.4f') '    ' num2str(yshare_x,'%6.4f') ...
    '    ' num2str(yshare_x_data, '%6.4f')...
    '     ' num2str(yshare_x - yshare_x_data,'%6.4f')];
tshare_v_str = ['Value in T     ' ' taua      ' ...
     num2str(taua, '%6.4f') '    ' num2str(tshare_v,'%6.4f') ...
    '    ' num2str(tshare_v_data, '%6.4f')...
    '     ' num2str(tshare_v - tshare_v_data,'%6.4f')];
tshare_p_str = ['Income in T    ' ' tauw      ' ...
     num2str(tauw, '%6.4f') '    ' num2str(tshare_p,'%6.4f') ...
    '    ' num2str(tshare_p_data, '%6.4f')...
    '     ' num2str(tshare_p - tshare_p_data,'%6.4f')];
tshare_b_str = ['Corp in T      ' ' taur      ' ...
     num2str(taur, '%6.4f') '    ' num2str(tshare_b,'%6.4f') ...
    '    ' num2str(tshare_b_data, '%6.4f')...
    '     ' num2str(tshare_b - tshare_b_data,'%6.4f')];
tyratio_str  = ['Tax/GDP        ' ' taua      ' ...
     num2str(taua, '%6.4f') '    ' num2str(tyratio,'%6.4f') ...
    '    ' num2str(tyratio_data, '%6.4f')...
    '     ' num2str(tyratio - tyratio_data,'%6.4f')];
disp('===================================================================')
disp('                  Match of Moments')
disp('---------------- Sectoral GDP Share -------------------------------')
disp('Moments         Para      Value     Model     Data      Difference ')
disp('-------------------------------------------------------------------')
disp(yshare_a_str);
disp(yshare_m_str);
disp(yshare_x_str);
disp('------------------ Tax Structure ----------------------------------')
disp(tshare_v_str);
disp(tshare_p_str);
disp(tshare_b_str);
disp(tyratio_str);
disp('===================================================================')

disp('Equilibrium Prices: ps, pa, w, wf, r')
disp([ps pa w wf r]);