%  ------------------------------------------------------------------------
%                      PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%    
%  Purpose:
%      - Export Results to Excel .xlsx binary format
%      - Steady State Results
%   
%  Author:
%      Xin Tang @ IMF and Wuhan University, Spring 2017
%   
%  Record of Revisions:
%          Date:                 Description of Changes
%      ===========        =================================
%       08/05/2017:                 Original Code
%  ------------------------------------------------------------------------

% Select Folder
msg = 'Select the Folder of the Results.';
fdname = uigetdir(pwd,msg);
if isequal(fdname,0)
    disp('User selected Cancel.')
else
    str = ['cd ''', fdname ''''];
    eval(str);
    
    % Export Macro Aggregates
    load 'moments.txt';
    load 'price_table.txt'
    load 'param_matlab.txt'
    
    ps = price_table(1);
    pa = price_table(2);
    
    muu = param_matlab(11);
    mur = param_matlab(12);
    muf = param_matlab(13);
    
    gdp = moments(51);
    aggcon = moments(52);
    agginv = moments(53);
    aggtax = moments(54);
    export = moments(55);
    
    yshare_m = moments(19);
    yshare_s = moments(20);
    yshare_x = moments(21);
    yshare_a = 1-yshare_m-yshare_s-yshare_x;
    
    ym = gdp*yshare_m;
    ys = gdp*yshare_s;
    ya = gdp*yshare_a;
    yx = gdp*yshare_x;
   
    aggca = moments(49);
    aggcm = moments(50);
    aggcs = moments(48);
    
    aggcu = muu*moments(45);
    aggcr = mur*moments(46);
    
    aggcua = muu*pa*moments(39);
    aggcum = muu*moments(42);
    aggcus = muu*ps*moments(36);
    aggcra = mur*pa*moments(40);
    aggcrm = mur*moments(43);
    aggcrs = mur*ps*moments(37);
    
    aggkm = muu*moments(33) + mur*moments(34);
    aggkf = muf*moments(35);
    
    agghu  = moments(29);
    aggha  = moments(30);
%     agghr  = moments(31); % rural labor demand for food production
%     agghst = moments(32); % rural labor demand for cash production
    
    tshare_v = moments(25);
    tshare_b = moments(24);
    tshare_p = moments(23);
    
    vtax = aggtax*tshare_v;
    btax = aggtax*tshare_b;
    ptax = aggtax*tshare_p;
    
    ginicu = moments(10);
    ginicr = moments(11);
    giniyu = moments(12);
    giniyr = moments(13);
    giniku = moments(14);
    ginikr = moments(15);
    
    aggginic = moments(16);
    aggginiy = moments(17);
    aggginik = moments(18);
    
    string = {
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
        'Urban Consumption Gini    ', ginicu;...
        'Rural Consumption Gini    ', ginicr;...
        'Urban Income Gini         ', giniyu;...
        'Rural Income Gini         ', giniyr;...
        'Urban Wealth Gini         ', giniku;...
        'Rural Wealth Gini         ', ginikr;...
        'Total Consumption Gini    ', aggginic;...
        'Total Income Gini         ', aggginiy;...
        'Total Wealth Gini         ', aggginik...
          };
    
    cstring = {'Statistics', 'Value'} ;
    cmtstring = {'All Variales in Nominal when Applicable',' '};
    cstring = [cstring;cmtstring;string];
          
    filename = 'Macro_Aggregates.xlsx';
    xlswrite(filename,cstring,1,'A1');
    
    disp('Macro Aggregates Exported!')
    
    % Export Micro-level Distribution
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
    
    % Urban Distribution f2(i,j)
    % i-saving grid, j-shock
    load 'dist_urban.txt'
    dist_urban = [kgrid, shock_grid, dist_urban];
    dist_urban_head = {'Saving','Income Shock','Measure',...
        'Food Consumption','Service Consumption',...
        'Manufacturing Consumption','Hours in Formal Sector',...
        'Indirect Utility'};
    dist_urban_cell = num2cell(dist_urban);
    dist_urban_exp = [dist_urban_head;dist_urban_cell];
    
    filename = 'Urban_Distribution_Saving.xlsx';
    xlswrite(filename,dist_urban_exp,1,'A1');
    
    clear dist_urban dist_urban_head dist_urban_cell dist_urban_exp
    disp('Urban Distribution on Saving Exported!')
    
    % Rural Distribution f2(i,j)
    % i-saving grid, j-shock
    load 'dist_rural.txt'
%     load 'dist_rural_labor.txt'
%     dist_rural_labor = dist_rural_labor(:,2);
    dist_rural = [kgrid, shock_grid, dist_rural];
    dist_rural_head = {'Saving','Income Shock','Measure',...
        'Food Consumption','Service Consumption',...
        'Manufacturing Consumption','Hours in Large Farms',...
        'Indirect Utility'};
    dist_rural_cell = num2cell(dist_rural);
    dist_rural_exp = [dist_rural_head;dist_rural_cell];
    
    filename = 'Rural_Distribution_Saving.xlsx';
    xlswrite(filename,dist_rural_exp,1,'A1');
    
    clear dist_rural dist_rural_head dist_rural_cell dist_rural_exp
    
    disp('Rural Distribution on Saving Exported!')
    
    % Urban Consumption Distribution
    load 'furban.txt'
    furban_head = {'Consumption','Measure','Total Income',...
        'Hours in Formal Sector','Saving','Non-wage Income',...
        'Wage Income','Interst Income'};
    furban_cell = num2cell(furban);
    furban_exp = [furban_head;furban_cell];
    
    filename = 'Urban_Distribution_Consumption.xlsx';
    xlswrite(filename,furban_exp,1,'A1');
    clear furban furban_cell furban_exp
    
    % Rural Consumption Distribution
    load 'frural.txt'
    frural_head = {'Consumption','Measure','Total Income',...
        'Hours in Formal Sector','Saving','Non-wage Income',...
        'Wage Income','Interst Income'};
    frural_cell = num2cell(frural);
    frural_exp = [frural_head;frural_cell];
    
    filename = 'Rural_Distribution_Consumption.xlsx';
    xlswrite(filename,frural_exp,1,'A1');    
    
    clear frural frural_cell frural_exp
    
    disp('Results Export Complete!')
end