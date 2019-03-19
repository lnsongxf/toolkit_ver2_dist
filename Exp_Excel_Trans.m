%  ------------------------------------------------------------------------
%                      PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%    
%  Purpose:
%      - Export Results to Excel .xlsx binary format
%      - Transitional Dynamics.
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
    
    disp('Please wait until the terminal reports all results exported.')
    
    nvariable = 46;
    beta = 0.96;
    
    % ============ Model Parameters ===================
    load model_parameters.txt;
    pars = model_parameters;
    clear model_parameters ;
    ntrans   = pars(1);
    abar     = pars(2);
    psi      = pars(3); 
    gamma    = pars(4); 
    sigmar   = pars(5); 
    sigmau   = pars(6); 
    muu      = pars(7)/(pars(7)+pars(8)); 
    mur      = pars(8)/(pars(7)+pars(8));  
    za       = pars(9); 
    zm       = pars(10); 
    z        = pars(11); 
    taua_ss1 = pars(12); 
    tauw_ss1 = pars(13); 
    taur_ss1 = pars(14); 
    taua_ss2 = pars(15); 
    tauw_ss2 = pars(16); 
    taur_ss2 = pars(17);
    clear pars ;    
    
    string_par = {
      'Transition Periods          ', ntrans;...
      'Service Preference          ', psi;...
      'Manufacturing Preference    ', gamma;...
      'Rural Variance              ', sigmar;...
      'Urban Variance              ', sigmau;...
      'Normalized Urban Population ', muu;...
      'Normalized Urban Population ', mur;...
      'Agricultural Productivity   ', za;...
      'Manufacturing Productivity  ', zm;...
      'Export Productivity         ', z;...
      'VAT SS 1                    ', taua_ss1;...
      'PIT SS 1                    ', tauw_ss1;...
      'CIT SS 1                    ', taur_ss1;...
      'VAT SS 2                    ', taua_ss2;...
      'PIT SS 2                    ', tauw_ss2;...
      'CIT SS 2                    ', taur_ss2...     
    };

    cstring = {'Variables','Values'};
    cstring = [cstring;string_par];
    
    filename = 'model_parameters.xlsx';
    xlswrite(filename,cstring,1,'A1');
    
    disp('Model Parameters Exported!');
    
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
    
    % Prepare the variables
    ginicu   = aggr(1,:);
    ginicr   = aggr(2,:);     
    ginict   = aggr(3,:);    
    giniyu   = aggr(4,:);
    giniyr   = aggr(5,:);
    giniyt   = aggr(6,:);        
    giniku   = aggr(7,:);    
    ginikr   = aggr(8,:);    
    ginikt   = aggr(9,:);    
    yshare_m = aggr(10,:); 
    yshare_s = aggr(11,:); 
    yshare_x = aggr(12,:); 
    yshare_t = aggr(13,:);
    tshare_p = aggr(14,:);
    tshare_b = aggr(15,:);
    tshare_v = aggr(16,:);
%     loca/lofca = aggr(17,:);
    cshare_m = aggr(18,:);   
    cshare_s = aggr(19,:);   
    agghu    = aggr(20,:);     
    aggha    = aggr(21,:);     
    agghr    = aggr(22,:);     
    agghst   = aggr(23,:);     
    aggku    = aggr(24,:);     
    aggkr    = aggr(25,:);      
    aggkf    = aggr(26,:);     
    aggcus   = aggr(27,:);    
    aggcrs   = aggr(28,:);    
    aggcfs   = aggr(29,:);    
    aggcua   = aggr(30,:);    
    aggcra   = aggr(31,:);        
    aggcfa   = aggr(32,:); 
    aggcum   = aggr(33,:);        
    aggcrm   = aggr(34,:);        
    aggcfm   = aggr(35,:);  
    aggcu    = aggr(36,:);         
    aggcr    = aggr(37,:);         
    aggcf    = aggr(38,:);         
    aggcs    = aggr(39,:);         
    aggca    = aggr(40,:);         
    aggcm    = aggr(41,:);         
    gdp      = aggr(42,:);           
    aggcon   = aggr(43,:);        
    agginv   = aggr(44,:);        
    aggtax   = aggr(45,:);        
    export   = aggr(46,:);  
    
    % calculate more variables
    yshare_a = 1-yshare_m-yshare_s-yshare_x;
    ym = gdp.*yshare_m;
    ys = gdp.*yshare_s;
    ya = gdp.*yshare_a;
    
    vtax = aggtax.*tshare_v;
    btax = aggtax.*tshare_b;
    ptax = aggtax.*tshare_p;    
    
    aggkm  = muu*aggku+mur*aggkr;
    
    % for welfare decomposition only
    agguca = aggr(30,:)./pa';
    aggrca = aggr(31,:)./pa';
    aggtca = muu*agguca+mur*aggrca;
    aggucm = aggr(33,:);
    aggrcm = aggr(34,:);
    aggtcm = muu*aggucm+mur*aggrcm;
    aggucs = aggr(27,:)./ps';
    aggrcs = aggr(28,:)./ps';
    aggtcs = muu*aggucs+mur*aggrcs;
    agguc  = aggr(36,:);
    aggrc  = aggr(37,:);
    aggtc  = muu*agguc+mur*aggrc;   

    string = {
        'Time Periods              ', 1:ntrans;...
        'Total Output              ', gdp;...
        'Total Consumption         ', aggcon;...
        'Total Investment          ', agginv;...
        'Total Tax Revenue         ', aggtax;...
        'Total Export              ', export;...
        'Agriculture Output        ', ya;...
        'Manufacturing Output      ', ym;...
        'Service Output            ', ys;...
        'Agricultural Output Share ', yshare_a;...
        'Manufacturing Output Share', yshare_m;...
        'Service Output Share      ', yshare_s;...
        'Export Output Share       ', yshare_x;...
        'Agricultural Consumption  ', aggca;...
        'Manufacturing Consumption ', aggcm;...
        'Service Consumption       ', aggcs;...
        'Urban Total Consumption   ', aggcu;...
        'Rural Total Consumption   ', aggcr;...
        'Urban Agri. Consumption   ', aggcua;...
        'Urban Manu. Consumption   ', aggcum;...
        'Urban Serv. Consumption   ', aggcus;...
        'Rural Agri. Consumption   ', aggcra;...
        'Rural Manu. Consumption   ', aggcrm;...
        'Rural Serv. Consumption   ', aggcrs;...        
        'Urban Formal Labor Share  ', agghu;...
        'Rural Formal Labor Share  ', aggha;...
        'Manufacturing Capital     ', aggkm;...
        'Export Sector Capital     ', aggkf;...
        'Value Added Tax           ', vtax;...
        'Business Tax              ', btax;...
        'Personal Income Tax       ', ptax;...
        'Value Added Tax Share     ', tshare_v;...
        'Business Tax Share        ', tshare_b;...
        'Peronal Income Tax Share  ', tshare_p;...     
        'Agricultural Price        ', pa;...
        'Service Price             ', ps;...
        'Urban Wage                ', w;...
        'Rural Wage                ', wf;...
        'Interest Rate             ', r;...
        'Urban Consumption Gini    ', ginicu;...
        'Rural Consumption Gini    ', ginicr;...
        'Urban Income Gini         ', giniyu;...
        'Rural Income Gini         ', giniyr;...
        'Urban Wealth Gini         ', giniku;...
        'Rural Wealth Gini         ', ginikr;...
        'Total Consumption Gini    ', ginict;...
        'Total Income Gini         ', giniyt;...
        'Total Wealth Gini         ', ginikt...
        };
    
%     write to Excel
    filename = 'Macro_Aggregates.xlsx';
    T = cell2table(string);
    writetable(T,filename,'Sheet',1,'Range','A1',...
        'WriteVariableNames',false);
    disp('Macro Aggregates Exported!');

    % ============== Welfare Distributions ===================
    % Construct xk2pts grids
%     load 'compu_matlab.txt'
    nk2pts = 10001;
    nshock = 15;
    nkpts = 481;
    
    xkpts = zeros(nkpts,1);
    xkpts(1) = 0.00015;
    xkinc = 0;
    for j = 1:1:60
        xkinc = xkinc + 0.0065*j;
        for i = (j-1)*8+2:1:j*8+1
          xkpts(i) = xkpts(i-1)+xkinc;
        end
    end

    xk2pts = zeros(nk2pts,1);
    xk2pts(1) = xkpts(1);
    xkinc = (xkpts(nkpts)-xkpts(1))/(nk2pts-1);
    for i = 2:1:nk2pts
        xk2pts(i) = xk2pts(i-1)+xkinc ;
    end
    
    % Construct k_i and shocks for f2(i,j)
    aux1 = ones(nshock,1);
    kgrid = kron(aux1,xk2pts);
    aux2 = ones(nk2pts,1);
    aux_shock = 1:1:nshock;
    shock_grid = kron(aux_shock',aux2);   
    
    dist_welfare = zeros(nk2pts*nshock,12);
    dist_welfare(:,1:2) = [kgrid, shock_grid];

    % Urban distribution
    dist_urban_head = {'Saving','Income Shock','Measure',...
        'Food Consumption','Service Consumption',...
        'Manufacturing Consumption','Hours in Formal Sector',...
        'Indirect Utility'};    
    % SS 1
    load 'dist_urban_ss1.txt'
    dist_welfare(:,3) = dist_urban_ss1(:,2);
    dist_welfare(:,4) = dist_urban_ss1(:,8);
    dist_urban_ss1 = [kgrid, shock_grid, dist_urban_ss1(:,2:6),...
        dist_urban_ss1(:,8)];
    dist_urban_cell = num2cell(dist_urban_ss1);
    dist_urban_exp = [dist_urban_head;dist_urban_cell];
    
    filename = 'Urban_Distribution_SS1.xlsx';
    xlswrite(filename,dist_urban_exp,1,'A1');    
    clear dist_urban_ss1 ;
    
    % SS 2
    load 'dist_urban_ss2.txt'
    dist_welfare(:,6) = dist_urban_ss2(:,2);
    dist_welfare(:,7) = dist_urban_ss2(:,8);
    dist_urban_ss2 = [kgrid, shock_grid, dist_urban_ss2(:,2:6),...
        dist_urban_ss2(:,8)];
    dist_urban_cell = num2cell(dist_urban_ss2);
    dist_urban_exp = [dist_urban_head;dist_urban_cell];
    
    filename = 'Urban_Distribution_SS2.xlsx';
    xlswrite(filename,dist_urban_exp,1,'A1');        
    clear dist_urban_ss2 ;   
    
    % NT 1
    load 'dist_urban_nt1.txt'
    dist_urban_nt1 = [kgrid, shock_grid, dist_urban_nt1(:,2:6),...
        dist_urban_nt1(:,8)];
    dist_welfare(:,5) = dist_urban_nt1(:,8);
    dist_urban_cell = num2cell(dist_urban_nt1);
    dist_urban_exp = [dist_urban_head;dist_urban_cell];
    
    filename = 'Urban_Distribution_NT1.xlsx';
    xlswrite(filename,dist_urban_exp,1,'A1');   
    clear dist_urban_nt1 ;
        
    clear dist_urban_head dist_urban_cell dist_urban_exp;
    disp('Urban Distribution Exported!')    
    
    % Rural distribution
    dist_rural_head = {'Saving','Income Shock','Measure',...
        'Food Consumption','Service Consumption',...
        'Manufacturing Consumption','Hours in Large Farms',...
        'Indirect Utility'};
    % SS 1
    load 'dist_rural_ss1.txt'
    dist_welfare(:,8) = dist_rural_ss1(:,2);
    dist_welfare(:,9) = dist_rural_ss1(:,8);
    dist_rural_ss1 = [kgrid, shock_grid, dist_rural_ss1(:,2:6),...
        dist_rural_ss1(:,8)];
    dist_rural_cell = num2cell(dist_rural_ss1);
    dist_rural_exp = [dist_rural_head;dist_rural_cell];
    
    filename = 'Rural_Distribution_SS1.xlsx';
    xlswrite(filename,dist_rural_exp,1,'A1');    
    clear dist_rural_ss1 ;
    
    % SS 2
    load 'dist_rural_ss2.txt'
    dist_welfare(:,11) = dist_rural_ss2(:,2);
    dist_welfare(:,12) = dist_rural_ss2(:,8);    
    dist_rural_ss2 = [kgrid, shock_grid, dist_rural_ss2(:,2:6),...
        dist_rural_ss2(:,8)];
    dist_rural_cell = num2cell(dist_rural_ss2);
    dist_rural_exp = [dist_rural_head;dist_rural_cell];
    
    filename = 'Rural_Distribution_SS2.xlsx';
    xlswrite(filename,dist_rural_exp,1,'A1');        
    clear dist_rural_ss2;
    
    % NT 1
    load 'dist_rural_nt1.txt'
    dist_welfare(:,10) = dist_rural_nt1(:,8);
    dist_rural_nt1 = [kgrid, shock_grid, dist_rural_nt1(:,2:6),...
        dist_rural_nt1(:,8)];
    dist_rural_cell = num2cell(dist_rural_nt1);
    dist_rural_exp = [dist_rural_head;dist_rural_cell];
    
    filename = 'Rural_Distribution_NT1.xlsx';
    xlswrite(filename,dist_rural_exp,1,'A1');            
    clear dist_rural_nt1 ;
        
    clear dist_rural_head dist_rural_cell dist_rural_exp;
    disp('Rural Distribution Exported!')    
    
    % Export Welfare
    dist_welf_head = {'Saving','Income Shock',...
        'Measure Urban SS1','Value Urban SS 1','Value Urban NT 1',...
        'Measure Urban SS2','Value Urban SS 2',...
        'Measure Rural SS1','Value Rural SS 1','Value Rural NT 1',...
        'Measure Rural SS2','Value Rural SS 2'};   
    dist_welf_cell = num2cell(dist_welfare);
    dist_welf_exp = [dist_welf_head;dist_welf_cell];    
    
    filename = 'Welfare_Distribution.xlsx';
    xlswrite(filename,dist_welf_exp,1,'A1');            
    clear dist_welf_head dist_welf_cell dist_welf_exp dist_welfare ;
   
    disp('All Results Exported!')
end