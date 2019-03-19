%  ------------------------------------------------------------------------
%                      PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%    
%  Purpose:
%      - Steady State Welfare Decomposition
%   
%  Author:
%      Xin Tang @ International Monetary Fund, Spring 2019
%   
%  Record of Revisions:
%          Date:                 Description of Changes
%      ===========        =================================
%       01/29/2019:                 Original Code
%  ------------------------------------------------------------------------

% Load Benchmark Steady State
msg = 'Select the Folder of Benchmark Results.';
fdname = uigetdir(pwd,msg);
if isequal(fdname,0)
    disp('User selected Cancel.')
else
    str = ['cd ''', fdname ''''];
    eval(str);
    load 'moments.txt'
    moments_0 = moments;
    load 'dist_urban.txt'
    dist_urban_0 = dist_urban;
    load 'dist_rural.txt'
    dist_rural_0 = dist_rural;    
    
    % Population share only load for ss_1
    load 'param_matlab.txt'
    psi = param_matlab(1);
    gamma = param_matlab(2);
    muu = param_matlab(11);
    mur = param_matlab(12);
    muf = param_matlab(13);
    
    clear moments dist_urban dist_rural param_matlab;
    
    % Load New Steady State
    msg = 'Select the Folder of New Results.';
    fdname = uigetdir(pwd,msg);
    if isequal(fdname,0)
        disp('User selected Cancel.')
    else
    str = ['cd ''', fdname ''''];
    eval(str);
    load 'moments.txt'
    moments_1 = moments;
    load 'dist_urban.txt'
    dist_urban_1 = dist_urban;
    load 'dist_rural.txt'
    dist_rural_1 = dist_rural;    
    clear moments dist_urban dist_rural;
    
    % ============== Extract Data ================
    beta = 0.96;
    % ------------ Rural ----------------
    valuer_ss1 = dist_rural_0(:,6);
    distr_ss1 = dist_rural_0(:,1);
    valuer_ss2 = dist_rural_1(:,6);
    distr_ss2 = dist_rural_1(:,1);
    
    % ------------ Urban ----------------
    valueu_ss1 = dist_urban_0(:,6);
    distu_ss1 = dist_urban_0(:,1);
    valueu_ss2 = dist_urban_1(:,6);
    distu_ss2 = dist_urban_1(:,1);
    
    % ------------ Total ----------------    
    valuet_ss1 = [valuer_ss1; valueu_ss1];
    distt_ss1  = [distr_ss1*mur; distu_ss1*muu];
    
    valuet_ss2 = [valuer_ss2; valueu_ss2];
    distt_ss2  = [distr_ss2*mur; distu_ss2*muu];
    
    % Aggregate Consumptions
    % Original Steady State
    agguca_0 = moments_0(39);
    aggrca_0 = moments_0(40);
    aggtca_0 = muu*agguca_0+mur*aggrca_0;
    aggucm_0 = moments_0(42);
    aggrcm_0 = moments_0(43);
    aggtcm_0 = muu*aggucm_0+mur*aggrcm_0;
    aggucs_0 = moments_0(36);
    aggrcs_0 = moments_0(37);
    aggtcs_0 = muu*aggucs_0+mur*aggrcs_0;
    
    % New Steady State
    agguca_1 = moments_1(39);
    aggrca_1 = moments_1(40);
    aggtca_1 = muu*agguca_1+mur*aggrca_1;
    aggucm_1 = moments_1(42);
    aggrcm_1 = moments_1(43);
    aggtcm_1 = muu*aggucm_1+mur*aggrcm_1;
    aggucs_1 = moments_1(36);
    aggrcs_1 = moments_1(37);
    aggtcs_1 = muu*aggucs_1+mur*aggrcs_1;    
    
    % ============== Total Costs ===================
    w1u_ss = sum(valueu_ss2.*distu_ss2);
    w0u_ss = sum(valueu_ss1.*distu_ss1);
    tlambda_u_ss = exp((w1u_ss-w0u_ss)*(1-beta)/(1+gamma+psi))-1;

    % ------------ Rural ----------------
    w1r_ss = sum(valuer_ss2.*distr_ss2);
    w0r_ss = sum(valuer_ss1.*distr_ss1);
    tlambda_r_ss = exp((w1r_ss-w0r_ss)*(1-beta)/(1+gamma+psi))-1;

    % ------------ Total ----------------
    w1t_ss = sum(valuet_ss2.*distt_ss2);
    w0t_ss = sum(valuet_ss1.*distt_ss1);
    tlambda_t_ss = exp((w1t_ss-w0t_ss)*(1-beta)/(1+gamma+psi))-1;    
    
    % ========= Aggregate and Distributional Components ===============
    % ------------ Urban ----------------
    alambda_u_ss = exp((log(agguca_1/agguca_0) +...
        gamma*log(aggucm_1/aggucm_0) + ...
        psi*log(aggucs_1/aggucs_0))/(1+gamma+psi) )-1;
    dlambda_u_ss = (1+tlambda_u_ss)/(1+alambda_u_ss)-1;

    % ------------ Rural ----------------
    alambda_r_ss = exp((log(aggrca_1/aggrca_0) +...
        gamma*log(aggrcm_1/aggrcm_0) + ...
        psi*log(aggrcs_1/aggrcs_0))/(1+gamma+psi) )-1;
    dlambda_r_ss = (1+tlambda_r_ss)/(1+alambda_r_ss)-1;

    % ------------ Total ----------------
    alambda_t_ss = exp((log(aggtca_1/aggtca_0) +...
        gamma*log(aggtcm_1/aggtcm_0) + ...
        psi*log(aggtcs_1/aggtcs_0))/(1+gamma+psi) )-1;
    dlambda_t_ss = (1+tlambda_t_ss)/(1+alambda_t_ss)-1;
    
    % Print Results to Terminal
    % Adjust for Percentage
    tlambda_t_ss = 100*tlambda_t_ss;
    tlambda_r_ss = 100*tlambda_r_ss;
    tlambda_u_ss = 100*tlambda_u_ss;
    alambda_t_ss = 100*alambda_t_ss;
    alambda_r_ss = 100*alambda_r_ss;
    alambda_u_ss = 100*alambda_u_ss;
    dlambda_t_ss = 100*dlambda_t_ss;
    dlambda_r_ss = 100*dlambda_r_ss;
    dlambda_u_ss = 100*dlambda_u_ss;
    
    tstring = ['Total          ' num2str(tlambda_r_ss,'%6.4f') '     ' ...
        '      ' num2str(tlambda_u_ss,'%6.4f') '            ' ...
        num2str(tlambda_t_ss,'%6.4f')];
    astring = ['Aggregate      ' num2str(alambda_r_ss,'%6.4f') '     ' ...
        '      ' num2str(alambda_u_ss,'%6.4f') '            ' ...
        num2str(alambda_t_ss,'%6.4f')];
    dstring = ['Distributional ' num2str(dlambda_r_ss,'%6.4f') '     ' ...
        '      ' num2str(dlambda_u_ss,'%6.4f') '            ' ...
        num2str(dlambda_t_ss,'%6.4f')];
    
    disp(' ============ % Change of Aggregate Variables ================ ')
    disp('                Rural             Urban              Whole     ')
    disp('---------------------------------------------------------------')
    disp(tstring);
    disp(astring);
    disp(dstring);
    disp('===============================================================')
    
    % Export Results to Excel
    string_exp = {
        'Total         ',tlambda_r_ss, tlambda_u_ss, tlambda_t_ss;
        'Aggregate     ',alambda_r_ss, alambda_u_ss, alambda_t_ss;
        'Distributional',dlambda_r_ss, dlambda_u_ss, dlambda_t_ss
        };
    cstring = {'Welfare','Rural','Urban','Whole'} ;
    cstring = [cstring;string_exp];
    
    filename = 'welf_decomp.xlsx';
    xlswrite(filename,cstring,1,'A1');
    
    disp('Welfare Decomposition exported to the New Steady State folder.')
    disp('File Name: welf_decomp.xlsx')
    end
end