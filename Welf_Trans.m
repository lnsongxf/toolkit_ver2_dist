%  ------------------------------------------------------------------------
%                      PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%    
%  Purpose:
%      - Welfare Decomposition Transitional Dynamics
%   
%  Author:
%      Xin Tang @ International Monetary Fund, Spring 2019
%   
%  Record of Revisions:
%          Date:                 Description of Changes
%      ===========        =================================
%       02/26/2019:                 Original Code
%  ------------------------------------------------------------------------

% Select Folder
msg = 'Select the Folder of the Results.';
fdname = uigetdir(pwd,msg);
if isequal(fdname,0)
    disp('User selected Cancel.')
else
    str = ['cd ''', fdname ''''];
    eval(str);

nvariable = 46;
beta = 0.96;    
disp('Please wait until the terminal reports results exported.')
% ============ Model Parameters ===================
load model_parameters.txt;
ntrans   = model_parameters(1);
abar     = model_parameters(2);
psi      = model_parameters(3); 
gamma    = model_parameters(4); 
sigmar   = model_parameters(5); 
sigmau   = model_parameters(6); 
muu      = model_parameters(7)/(model_parameters(7)+model_parameters(8)); 
mur      = model_parameters(8)/(model_parameters(7)+model_parameters(8));  
za       = model_parameters(9); 
zm       = model_parameters(10); 
z        = model_parameters(11); 
taua_ss1 = model_parameters(12); 
tauw_ss1 = model_parameters(13); 
taur_ss1 = model_parameters(14); 
taua_ss2 = model_parameters(15); 
tauw_ss2 = model_parameters(16); 
taur_ss2 = model_parameters(17);
clear model_parameters;

% ============ Distributions ===================
% ------------ Rural ----------------
% Initial Steady State
load dist_rural_ss1.txt;
valuer_ss1 = dist_rural_ss1(:,8);
distr_ss1 = dist_rural_ss1(:,2);
clear dist_rural_ss1;

% End Steady State
load dist_rural_ss2.txt;
valuer_ss2 = dist_rural_ss2(:,8);
distr_ss2 = dist_rural_ss2(:,2);
clear dist_rural_ss2;

% First Period in Transition
load dist_rural_nt1.txt;
valuer_nt1 = dist_rural_nt1(:,8);
distr_nt1 = dist_rural_nt1(:,2);
clear dist_rural_nt1;

% ------------ Urban ----------------
% Initial Steady State
load dist_urban_ss1.txt;
valueu_ss1 = dist_urban_ss1(:,8);
distu_ss1 = dist_urban_ss1(:,2);
clear dist_urban_ss1;

% End Steady State
load dist_urban_ss2.txt;
valueu_ss2 = dist_urban_ss2(:,8);
distu_ss2 = dist_urban_ss2(:,2);
clear dist_urban_ss2;

% First Period in Transition
load dist_urban_nt1.txt;
valueu_nt1 = dist_urban_nt1(:,8);
distu_nt1 = dist_urban_nt1(:,2);
clear dist_urban_nt1;

% ------------ Total ----------------
% Initial Steady State
valuet_ss1 = [valuer_ss1; valueu_ss1];
distt_ss1  = [distr_ss1*mur; distu_ss1*muu];

% End Steady State
valuet_ss2 = [valuer_ss2; valueu_ss2];
distt_ss2  = [distr_ss2*mur; distu_ss2*muu];

% First Period in Transition
valuet_nt1 = [valuer_nt1; valueu_nt1];
distt_nt1  = [distr_nt1*mur; distu_nt1*muu];

% ============== Macro Aggregates ===================
% Prices
load eprices_out.txt;
ps = eprices_out(:,1);
pa = eprices_out(:,2);
w  = eprices_out(:,3);
wf = eprices_out(:,4);
r  = eprices_out(:,5);
clear eprices_out;

% Aggregates
load moments.txt;
aggr = reshape(moments,[nvariable, ntrans]);

% Construct Aggregate Consumption
agguca = aggr(30,:)./pa';
aggrca = aggr(31,:)./pa';
aggtca = muu*agguca+mur*aggrca;
aggucm = aggr(33,:);
aggrcm = aggr(34,:);
aggtcm = muu*aggucm+mur*aggrcm;
aggucs = aggr(27,:)./ps';
aggrcs = aggr(28,:)./ps';
aggtcs = muu*aggucs+mur*aggrcs;
agguc = aggr(36,:);
aggrc = aggr(37,:);
aggtc = muu*agguc+mur*aggrc;    
    
% =========================================================================
%          Total Consumption Equivalence
% =========================================================================
% ============== Transition ===================
% ------------ Urban ----------------
w1u_trans = sum(valueu_nt1.*distu_ss1);
w0u_trans = sum(valueu_ss1.*distu_ss1);
tlambda_u_trans = exp((w1u_trans-w0u_trans)*(1-beta)/(1+gamma+psi))-1;

% ------------ Rural ----------------
w1r_trans = sum(valuer_nt1.*distr_ss1);
w0r_trans = sum(valuer_ss1.*distr_ss1);
tlambda_r_trans = exp((w1r_trans-w0r_trans)*(1-beta)/(1+gamma+psi))-1;

% ------------ Total ----------------
w1t_trans = sum(valuet_nt1.*distt_ss1);
w0t_trans = sum(valuet_ss1.*distt_ss1);
tlambda_t_trans = exp((w1t_trans-w0t_trans)*(1-beta)/(1+gamma+psi))-1;

% ============== Steady State ===================
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

% disp('Transition vs. SS vs. %')
% disp([[tlambda_u_trans;tlambda_r_trans;tlambda_t_trans],...
%     [tlambda_u_ss;tlambda_r_ss;tlambda_t_ss],...
%     [1-tlambda_u_ss/tlambda_u_trans;1-tlambda_r_ss/tlambda_r_trans;...
%     1-tlambda_t_ss/tlambda_t_trans]]);

% =========================================================================
%          Aggregate and Distributional Components
% =========================================================================
beta_trans = beta.^(0:ntrans-1);
% ============== Transition ===================
% ------------ Urban ----------------
valueu_hat_tmp = beta_trans.*...
    (log(agguca/agguca(1)) + gamma*log(aggucm/aggucm(1)) +...
    psi*log(aggucs/aggucs(1)));
valueu_hat_tmp = [valueu_hat_tmp, (beta^ntrans)/(1-beta)*...
    (log(agguca(ntrans)/agguca(1)) + ...
    gamma*log(aggucm(ntrans)/aggucm(1)) +...
    psi*log(aggucs(ntrans)/aggucs(1)))];
valueu_hat = sum(valueu_hat_tmp) + valueu_ss1;
w1u_trans_aggr = sum(valueu_hat.*distu_ss1);
w0u_trans_aggr = sum(valueu_ss1.*distu_ss1);
alambda_u_trans = exp((w1u_trans_aggr-w0u_trans_aggr)...
    *(1-beta)/(1+gamma+psi))-1;
dlambda_u_trans = (1+tlambda_u_trans)/(1+alambda_u_trans)-1;
% disp('Urban Welfare Decomposition Transition')
% disp([tlambda_u_trans, alambda_u_trans, dlambda_u_trans]);

% ------------ Rural ----------------
valuer_hat_tmp = beta_trans.*...
    (log(aggrca/aggrca(1)) + gamma*log(aggrcm/aggrcm(1)) +...
    psi*log(aggrcs/aggrcs(1)));
valuer_hat_tmp = [valuer_hat_tmp,(beta^ntrans)/(1-beta)*...
    (log(aggrca(ntrans)/aggrca(1)) + ...
    gamma*log(aggrcm(ntrans)/aggrcm(1)) +...
    psi*log(aggrcs(ntrans)/aggrcs(1)))];
valuer_hat = sum(valuer_hat_tmp) + valuer_ss1;
w1r_trans_aggr = sum(valuer_hat.*distr_ss1);
w0r_trans_aggr = sum(valuer_ss1.*distr_ss1);
alambda_r_trans = exp((w1r_trans_aggr-w0r_trans_aggr)...
    *(1-beta)/(1+gamma+psi))-1;
dlambda_r_trans = (1+tlambda_r_trans)/(1+alambda_r_trans)-1;
% disp('Rural Welfare Decomposition Transition')
% disp([tlambda_r_trans, alambda_r_trans, dlambda_r_trans]);

% ------------ Total ----------------
valuet_hat_tmp = beta_trans.*...
    (log(aggtca/aggtca(1)) + gamma*log(aggtcm/aggtcm(1)) +...
    psi*log(aggtcs/aggtcs(1)));
valuet_hat_tmp = [valuet_hat_tmp,(beta^ntrans)/(1-beta)*...
    (log(aggtca(ntrans)/aggtca(1)) + ...
    gamma*log(aggtcm(ntrans)/aggtcm(1)) +...
    psi*log(aggtcs(ntrans)/aggtcs(1)))];
valuet_hat = sum(valuet_hat_tmp) + valuet_ss1;
w1t_trans_aggr = sum(valuet_hat.*distt_ss1);
w0t_trans_aggr = sum(valuet_ss1.*distt_ss1);
alambda_t_trans = exp((w1t_trans_aggr-w0t_trans_aggr)...
    *(1-beta)/(1+gamma+psi))-1;
dlambda_t_trans = (1+tlambda_t_trans)/(1+alambda_t_trans)-1;
% disp('Total Welfare Decomposition Transition')
% disp([tlambda_t_trans, alambda_t_trans, dlambda_t_trans]);

% ============== Steady State ===================
% ------------ Urban ----------------
alambda_u_ss = exp((log(agguca(ntrans)/agguca(1)) +...
    gamma*log(aggucm(ntrans)/aggucm(1)) + ...
    psi*log(aggucs(ntrans)/aggucs(1)))/(1+gamma+psi) )-1;
dlambda_u_ss = (1+tlambda_u_ss)/(1+alambda_u_ss)-1;
% disp('Urban Welfare Decomposition SS')
% disp([tlambda_u_ss, alambda_u_ss, dlambda_u_ss]);

% ------------ Rural ----------------
alambda_r_ss = exp((log(aggrca(ntrans)/aggrca(1)) +...
    gamma*log(aggrcm(ntrans)/aggrcm(1)) + ...
    psi*log(aggrcs(ntrans)/aggrcs(1)))/(1+gamma+psi) )-1;
dlambda_r_ss = (1+tlambda_r_ss)/(1+alambda_r_ss)-1;
% disp('Rural Welfare Decomposition SS')
% disp([tlambda_r_ss, alambda_r_ss, dlambda_r_ss]);

% ------------ Total ----------------
alambda_t_ss = exp((log(aggtca(ntrans)/aggtca(1)) +...
    gamma*log(aggtcm(ntrans)/aggtcm(1)) + ...
    psi*log(aggtcs(ntrans)/aggtcs(1)))/(1+gamma+psi) )-1;
dlambda_t_ss = (1+tlambda_t_ss)/(1+alambda_t_ss)-1;
% disp('Total Welfare Decomposition SS')
% disp([tlambda_t_ss, alambda_t_ss, dlambda_t_ss]);

% Print results to terminal
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

tlambda_t_trans = 100*tlambda_t_trans;
tlambda_r_trans = 100*tlambda_r_trans;
tlambda_u_trans = 100*tlambda_u_trans;
alambda_t_trans = 100*alambda_t_trans;
alambda_r_trans = 100*alambda_r_trans;
alambda_u_trans = 100*alambda_u_trans;
dlambda_t_trans = 100*dlambda_t_trans;
dlambda_r_trans = 100*dlambda_r_trans;
dlambda_u_trans = 100*dlambda_u_trans;

tstring_trans = ['Total          ' ...
    num2str(tlambda_r_trans,'%6.4f') '     ' ...
    '      ' num2str(tlambda_u_trans,'%6.4f') '            ' ...
    num2str(tlambda_t_trans,'%6.4f')];
astring_trans = ['Aggregate      ' ...
    num2str(alambda_r_trans,'%6.4f') '     ' ...
    '      ' num2str(alambda_u_trans,'%6.4f') '            ' ...
    num2str(alambda_t_trans,'%6.4f')];
dstring_trans = ['Distributional ' ...
    num2str(dlambda_r_trans,'%6.4f') '     ' ...
    '      ' num2str(dlambda_u_trans,'%6.4f') '            ' ...
    num2str(dlambda_t_trans,'%6.4f')];

tstring_ss = ['Total          ' num2str(tlambda_r_ss,'%6.4f') '     ' ...
    '      ' num2str(tlambda_u_ss,'%6.4f') '            ' ...
    num2str(tlambda_t_ss,'%6.4f')];
astring_ss = ['Aggregate      ' num2str(alambda_r_ss,'%6.4f') '     ' ...
    '      ' num2str(alambda_u_ss,'%6.4f') '            ' ...
    num2str(alambda_t_ss,'%6.4f')];
dstring_ss = ['Distributional ' num2str(dlambda_r_ss,'%6.4f') '     ' ...
    '      ' num2str(dlambda_u_ss,'%6.4f') '            ' ...
    num2str(dlambda_t_ss,'%6.4f')];
    
disp(' ============ % Change of Aggregate Variables ================ ')
disp('                Rural             Urban              Whole     ')
disp('---------------------------------------------------------------')
disp('Transition')
disp(tstring_trans);
disp(astring_trans);
disp(dstring_trans);
disp('---------------------------------------------------------------')
disp('Steady State')
disp(tstring_ss);
disp(astring_ss);
disp(dstring_ss);
disp('===============================================================')
    
% Export Results to Excel
string_exp_ss = {
    'Total         ',tlambda_r_ss, tlambda_u_ss, tlambda_t_ss;
    'Aggregate     ',alambda_r_ss, alambda_u_ss, alambda_t_ss;
    'Distributional',dlambda_r_ss, dlambda_u_ss, dlambda_t_ss
    };
string_exp_trans = {
    'Total         ',tlambda_r_trans, tlambda_u_trans, tlambda_t_trans;
    'Aggregate     ',alambda_r_trans, alambda_u_trans, alambda_t_trans;
    'Distributional',dlambda_r_trans, dlambda_u_trans, dlambda_t_trans
    };
cstring = {'Welfare','Rural','Urban','Whole'} ;
cstring1 = {'Transition',' ',' ',' '};
cstring2 = {'Steady State',' ',' ',' '};
cstring = [cstring;cstring1;string_exp_trans;...
    cstring2;string_exp_ss];
   
filename = 'welf_decomposition.xlsx';
xlswrite(filename,cstring,1,'A1');
disp('Welfare Decomposition Exported!')
    
end