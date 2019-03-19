% -------------------------------------------------------------------------
%                           PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%  Purpose:
%   - The main UI for solving the transitional dynamics.
% 
%  Functions:
%   - Buttons :
%       - Solve and Save : call .exe files to solve the model and save the
%               the results.
%       - Solve          : call .exe files to solve the model.
%       - Export         : export the results in Excel format.
%       - Welfare        : welfare decomposition for transitional dynamics
%   - Tables :
%       - Parameter values to be passed to the programs.
%   - Radio Button:
%       - Solve for prices or evaluate equilibrium at given prices.
%       - Extend path or not.
%  Author:
%      Xin Tang @ International Monetary Fund, Spring 2019
% 
%  Record of Revisions:
%        Date                Description of Changes
%    ===========        =================================
%     02/26/2019                 Original Code
%  ========================================================================

function toolkit_trans

%  ========================================================================
%                       Layout of the GUI
%  ========================================================================
f = figure('Visible','off','NumberTitle','off',...
    'MenuBar','none','Toolbar','none',...
    'Name','Tookit Version 2.0: Transition',...
    'Position',[17 20 911 700]);
% The Menu Bar
mh = uimenu('Parent',f,'Label','Uers''s Guide',...
    'Callback',@menudocu_callback);
% mh_1 = uimenu('Parent',mh,'Label','Quick Start',...
%     'Callback',@menudocu_callback);
% mh_2 = uimenu('Parent',mh,'Label','Tutorial',...
%     'Callback',@menututorial_callback);
% mh_3 = uimenu('Parent',mh,'Label','Release Notes',...
%     'Callback',@menumanual_callback);

% -------------------------------------------------------------------------
%                       Initialization
% -------------------------------------------------------------------------
% Set Base Directory
basedir = pwd;
setappdata(f,'BaseDir',basedir);

% ================ Initialize the Data Table ==============================
endo_flag = 0;
% endo_flag = 0: evaluate at equilibrium price, export dist_xx_ss1/ss2/nt1
% endo_flag = 1: solve equilibrium price, do not export dist**
extend_flag = 0;
% extend_flag = 0: use linear guess (endo = 0
% extend_flag = 1: read in existing path, extend the end linearly
cd(basedir);
cd bin_trans;
fid = fopen('endo_flag.txt','w');
fprintf(fid,'%d\n',endo_flag);
fclose(fid); 
fid = fopen('extend_flag.txt','w');
fprintf(fid,'%d\n',extend_flag);
fclose(fid); 
cd(basedir);

% ------------- Model Parameters ------------------------------------------
% - Ethiopia as Benchmark
abar   = 0.0008314562;
psi    = 0.4944552241;
gamma  = 0.8167964379;
sigmar = 0.2338952483;
sigmau = 0.6250133420;
za     = 0.7434502896;
zm     = 10.1858001797;
z      = 0.6845109209;
xtol   = 1e-5;
% Urban Population share fixed during calibration
muu    = 0.28;
mur    = 0.69;
muf    = 0.03;
constant_param =...
    {'Service Preference',         psi;...
     'Manufacturing Preference',   gamma;...
     'Rural Variance',             sigmar;...
     'Urban Variance',             sigmau;...
     'Agricultural Productivity',  za;...
     'Manufacturing Productivity', zm;...
     'Exporting Productivity',     z;...
     'Urban Popu (Exogenous)',     muu;...
     'Rural Popu (Exogenous)',     mur;...
     'Farmer Popu (Exogenous)',    muf;...
     'Subsistence Level',          abar;...
     'Convergence Criterion',      xtol};

% ------------- Equilibrium Prices for SS1 and 2 ----------------------
% Steady State 1
eprice_ss1 = [20.5133438772;...
          18.8391036004;...
          6.0878980669;...
          3.8278637784;...
          0.0074103014];
price1_param =...
    {'Service',         eprice_ss1(1);...
     'Food',            eprice_ss1(2);...
     'Urban Labor',     eprice_ss1(3);...
     'Rural Labor',     eprice_ss1(4);...
     'Interest Rate',   eprice_ss1(5)}; 
% Steady State 2
eprice_ss2 = [20.2107926451;...
          17.8504562279;...
          6.1392647747;...
          3.7113439115;...
          0.0075659625];
price2_param =...
    {'Service',         eprice_ss2(1);...
     'Food',            eprice_ss2(2);...
     'Urban Labor',     eprice_ss2(3);...
     'Rural Labor',     eprice_ss2(4);...
     'Interest Rate',   eprice_ss2(5)}; 

% ------------- Fiscal Policies for SS1 and 2 ----------------------
% Steady State 1
taua_ss1 = 0.0645361532;
taur_ss1 = 0.1155349787;
tauw_ss1 = 0.0555203613;
tu_ss1   = 0.0;
tr_ss1   = 0.0;
policy1_param = ...
    {'VAT',             taua_ss1;...
     'CIT',             taur_ss1;...
     'PIT',             tauw_ss1;...
     'Urban Transfer',  tu_ss1;
     'Rural Transfer',  tr_ss1};

% Steady State 2
taua_ss2 = 0.1045361532;
taur_ss2 = 0.1155349787;
tauw_ss2 = 0.0555203613;
tu_ss2   = 0.0;
tr_ss2   = 0.0;
policy2_param = ...
    {'VAT',             taua_ss2;...
     'CIT',             taur_ss2;...
     'PIT',             tauw_ss2;...
     'Urban Transfer',  tu_ss2;
     'Rural Transfer',  tr_ss2};

% ------------- Transition Length ---------------------- 
ntran = 7;
ntran_in = 7;
length_param = ...
    {'Length to Solve', ntran;...
     'Length of Guess', ntran_in};
 
% % ------------- Generate a Guess -----------------------
% eprices_out_param = [eprice_ss1';ones(ntran-1,1)*eprice_ss2'];
% cd(basedir);
% cd bin_trans;
% dlmwrite('eprices_out.txt',eprices_out_param,...
%     'newline','pc','precision','%16.10f','delimiter',' '); 
% cd(basedir);

% Keep a mirror of all default parameters
constant_default = constant_param;
price1_default   = price1_param;
price2_default   = price2_param;
policy1_default  = policy1_param;
policy2_default  = policy2_param; 
length_default   = length_param;

% -------------------------------------------------------------------------
%                   Construct GUI Components
% -------------------------------------------------------------------------
% --------------- Push Buttons --------------------------------------------
% Solve the Model and save the results
hsolve = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Solve and Save','Position',[720,625,125,35],...
    'Callback',@hsolve_callback);
% Quickly Solve the Model
hquick = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Solve','Position',[720,580,125,35],...
    'Callback',@hquick_callback);
% Export Results
hexport = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Export','Position',[720,535,125,35],...
    'Callback',@hexport_callback);
% Welfare Decomposition
hwelf = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Welfare','Position',[720,490,125,35],...
    'Callback',@hwelf_callback);

% Set All Parameterizations to Default
% Default set as Ethiopia
htdefault = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Reset to Default','Position',[40,50,125,35],...
    'Callback',@hdflt_callback);
% Save the Current Parameterization
hsave = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Save','Position',[175,50,125,35],...
    'Callback',@hsave_callback);
% Load Previous Parameterization
hload = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Load','Position',[310,50,125,35],...
    'Callback',@hload_callback);
% Update Current Parameterizations to Text Files
hupdate = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Update','Position',[445,50,125,35],...
    'Callback',@hupdate_callback);
% Generate an initial path
hpath0 = uicontrol('Parent',f,'Style','pushbutton',...
    'String','Generate Initial Path','Position',[520,540,125,35],...
    'Callback',@hpath0_callback);

% --------------- Ratio Buttons -------------------------------------------
% Controlling for whether to solve for equilibrium price paths
hbgh_endo = uibuttongroup('Parent',f,...
    'Title','Solving for Market Clearing Price Paths?',...
    'Position',[.7 .47 .25 .2],...
    'SelectionChangedFcn',@solveprice);
hrbh1_endo = uicontrol('Parent',hbgh_endo,...
    'Style','radiobutton',...
    'String','Exogenous',...
    'Units','normalized',...
    'Position',[.1 .6 .8 .2]);
hrbh2_endo = uicontrol('Parent',hbgh_endo,...
    'Style','radiobutton',...
    'String','Endogenous',...
    'Units','normalized',...
    'Position',[.1 .2 .8 .2]);

% Controlling for whether to extend current guess
hbgh_ext = uibuttongroup('Parent',f,...
    'Title','Extrapolate Path?',...
    'Position',[.7 .25 .25 .2],...
    'SelectionChangedFcn',@extendpath);
hrbh1_ext = uicontrol('Parent',hbgh_ext,...
    'Style','radiobutton',...
    'String','Yes',...
    'Units','normalized',...
    'Value',0,...
    'Position',[.1 .6 .8 .2]);
hrbh2_ext = uicontrol('Parent',hbgh_ext,...
    'Style','radiobutton',...
    'String','No',...
    'Units','normalized',...
    'Value',1,...
    'Position',[.1 .2 .8 .2]);

% --------------- Data Tables ---------------------------------------------
% With Toggle Button to control for whether tables are editable
% Model Parameters
str_constant = uicontrol('Parent',f,...
    'Style','text','String','Model Parameters',...
    'Position', [20 654 150 30], 'FontSize',10);
htconstant = uitable('Parent',f,...
            'Position', [40 385 308 284], ...
            'Data',constant_param,...
            'ColumnEditable', [false true], ...
            'ColumnName', {'Parameters','Values'}, ...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{220 86},...
            'CellEditCallback',@const_callback);
hconstant_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[40 350 125 30],...
    'Callback',@hconstfz_callback);

% Fiscal Policies
str_policy1 = uicontrol('Parent',f,...
    'Style','text','String','Fiscal SS 1',...
    'Position', [360 654 80 30], 'FontSize',10);
htpolicy1 = uitable('Parent',f,...
            'Position', [360 540 155 125], ...
            'Data',policy1_param,...
            'ColumnEditable',[false true],...
            'ColumnName',{'Moments', 'Targets'},...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{100 50},...
            'CellEditCallback',@policy1_callback);

str_policy2 = uicontrol('Parent',f,...
    'Style','text','String','Fiscal SS 2',...
    'Position', [360 500 80 30], 'FontSize',10);
htpolicy2 = uitable('Parent',f,...
            'Position', [360 385 155 125], ...
            'Data',policy2_param,...
            'ColumnEditable',[false true],...
            'ColumnName',{'Parameters', 'Values'},...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{100 50},...
            'CellEditCallback',@policy2_callback);

% Transition Periods        
str_length = uicontrol('Parent',f,...
    'Style','text','String','Length of Path',...
    'Position', [520 655 90 30], 'FontSize',10);
htlength = uitable('Parent',f,...
            'Position', [520 590 155 75], ...
            'Data', length_param,...
            'ColumnEditable',[false true],...
            'ColumnName',{'Parameters', 'Values'},...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{100 50},...
            'CellEditCallback',@length_callback);       

% Initial Guess for Prices
str_eps1 = uicontrol('Parent',f,...
    'Style','text','String','Prices SS 1',...
    'Position', [35 290 80 30], 'FontSize',10);
htprice1 = uitable('Parent',f,...
    'Position', [40 170 185 130], ...
    'Data', price1_param,...
    'ColumnEditable',[false true],...
    'ColumnName',{'Markets','Prices'},...
    'ColumnFormat',{'char','numeric'},...
    'FontSize',10,...
    'RowName',{},...
    'ColumnWidth',{120 63},...
    'CellEditCallback',@price1_callback);
hprice1_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[40 130 125 30],...
    'Callback',@hprice1fz_callback);   

str_eps2 = uicontrol('Parent',f,...
    'Style','text','String','Prices SS 2',...
    'Position', [245 290 80 30], 'FontSize',10);
htprice2 = uitable('Parent',f,...
    'Position',[250 170 185 130],...
    'Data', price2_param,...
    'RowName',{},...
    'ColumnFormat',{'char','numeric'},...
    'ColumnWidth',{120 63},...
    'ColumnName',{'Markets','Prices'},...
    'FontSize',10,...
    'ColumnEditable',[false true],...
    'CellEditCallback',@price2_callback);
hprice2_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[250 130 125 30],...
    'Callback',@hprice2fz_callback);   

% Change units to normalized so components resize automatically.
f.Units             = 'normalized';
hsolve.Units        = 'normalized';
hquick.Units        = 'normalized';
hexport.Units       = 'normalized';
hwelf.Units         = 'normalized';
hbgh_endo.Units     = 'normalized';
hbgh_ext.Units      = 'normalized';
htconstant.Units    = 'normalized';
htpolicy1.Units     = 'normalized';
htpolicy2.Units     = 'normalized';
htprice1.Units      = 'normalized';
htprice2.Units      = 'normalized';
htlength.Units      = 'normalized';
% htaccu.Units    = 'normalized';
hcali_fz.Units      = 'normalized';
hprice1_fz.Units    = 'normalized';
hprice2_fz.Units    = 'normalized';
str_constant.Units  = 'normalized';
str_policy1.Units   = 'normalized';
str_policy2.Units   = 'normalized';
str_eps1.Units      = 'normalized';
str_eps2.Units      = 'normalized';
str_length.Units    = 'normalized';
htdefault.Units     = 'normalized';
hsave.Units         = 'normalized';
hload.Units         = 'normalized';
hupdate.Units       = 'normalized';
hpath0.Units        = 'normalized';

% Move the GUI to the center of the screen.
movegui(f,'center')
f.Visible = 'on';

%  ========================================================================
%                   Nested Callback Functions
%  ========================================================================
% Callback functions with a shared namespace
% -------------------- Table Editing --------------------------------------
    % Model Parameters
    function const_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    function policy1_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('fiscal1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    function policy2_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('fiscal2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    function price1_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    function price2_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    function length_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_trans;
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);
        cd(basedir);
    end

% ---------- Save and Load Previous Parameterizations ---------------------
    function hdflt_callback(hObject, eventdata)
        % Set all parameters to default Ethiopia level
        htconstant.Data  = constant_default;
        htpolicy1.Data   = policy1_default;
        htpolicy2.Data   = policy2_default;
        htprice1.Data    = price1_default;
        htprice2.Data    = price2_default;
        htlength.Data    = length_default;
 
        % Write default data to local files
        cd(basedir)
        cd bin_trans;
        % Model Parameters
        junk = htconstant.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        % Fiscal SS 1
        junk = htpolicy1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Fiscal SS 2
        junk = htpolicy2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Price SS 1
        junk = htprice1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Price SS 2
        junk = htprice2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Length of Transition
        junk = htlength.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);

        % ------------- Generate a Guess -----------------------
%         junk = htprice1.Data ;
%         ep_tmp1 = cell2mat(junk(:,2));
%         junk = htprice2.Data ;
%         ep_tmp2 = cell2mat(junk(:,2));
%         junk = htlength.Data ;
%         nt_tmp  = cell2mat(junk(1,2));
%         ep_out_param = [ep_tmp1';ones(nt_tmp-1,1)*ep_tmp2'];
%         dlmwrite('eprices_out.txt',ep_out_param,...
%             'newline','pc','precision','%16.10f','delimiter',' ');         
        dos('lin_gen.exe');

        % Return control to base directory
        cd(basedir);
    end

    function hsave_callback(hObject, eventdata)
        % Save current parameterizations to local files
        constant_param  = htconstant.Data;
        policy1_param   = htpolicy1.Data;
        policy2_param   = htpolicy2.Data;
        price1_param    = htprice1.Data;
        price2_param    = htprice2.Data;
        length_param    = htlength.Data;
        uisave({'constant_param','policy1_param','policy2_param',...
            'price1_param','price2_param','length_param'}); 
        cd(basedir);
    end

    function hload_callback(hObject, eventdata)
        % Load previous parameterizations
        [filename, pathname] = uigetfile('*.mat',...
            'Select Previous Data to Load');
        if isequal(filename,0)
             disp('User selected Cancel.')
        else
           str = ['cd ''', pathname ''''];
           eval(str);
           varlist = {'constant_param','policy1_param','policy2_param',...
               'price1_param','price2_param','length_param'};
           load(filename,varlist{:});
           htconstant.Data  = constant_param;
           htpolicy1.Data  = policy1_param;
           htpolicy2.Data  = policy2_param;
           htprice1.Data = price1_param;
           htprice2.Data = price2_param;
           htlength.Data = length_param;
       end
       cd(basedir);
    end

    function hpath0_callback(hObject, eventdata)
        % ------------- Generate a Guess -----------------------
%         junk = htprice1.Data ;
%         ep_tmp1 = cell2mat(junk(:,2));
%         junk = htprice2.Data ;
%         ep_tmp2 = cell2mat(junk(:,2));
%         junk = htlength.Data ;
%         nt_tmp  = cell2mat(junk(1,2));
%         ep_out_param = [ep_tmp1';ones(nt_tmp-1,1)*ep_tmp2'];
%         
%         cd(basedir)
%         cd bin_trans
%         dlmwrite('eprices_out.txt',ep_out_param,...
%             'newline','pc','precision','%16.10f','delimiter',' ');         
%         cd(basedir)

        cd(basedir);
        cd bin_trans;
        
        % Price SS 1
        junk = htprice1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Price SS 2
        junk = htprice2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Length of Transition
        junk = htlength.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);        
        
        dos('lin_gen.exe');
        cd(basedir);        
    end

    function hupdate_callback(hObject, eventdata)
        % Update Current Parameterizations to Text Files
        cd(basedir);
        cd bin_trans;
        
        % Write current data to local files
        % Model Parameters
        junk = htconstant.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        % Fiscal SS 1
        junk = htpolicy1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Fiscal SS 2
        junk = htpolicy2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Price SS 1
        junk = htprice1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Price SS 2
        junk = htprice2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Length of Transition
        junk = htlength.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);

        % ------------- Generate a Guess -----------------------
%         junk = htprice1.Data ;
%         ep_tmp1 = cell2mat(junk(:,2));
%         junk = htprice2.Data ;
%         ep_tmp2 = cell2mat(junk(:,2));
%         junk = htlength.Data ;
%         nt_tmp  = cell2mat(junk(1,2));
%         ep_out_param = [ep_tmp1';ones(nt_tmp-1,1)*ep_tmp2'];
%         dlmwrite('eprices_out.txt',ep_out_param,...
%             'newline','pc','precision','%16.10f','delimiter',' '); 

        dos('lin_gen.exe');

        % Return control to base directory
        cd(basedir);
    end

% ---------- Error-proofing Features --------------------------------------
    function hconstfz_callback(hObject,eventdata)
        % Freeze Model Parameters Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Parameter Table Editing.')
            htconstant.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('Unfreeze Parameter Table Editing.')
            htconstant.ColumnEditable = [false true];
        end
    end

    function hprice1fz_callback(hObject,eventdata)
        % Freeze Calibration Targets Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Price 1 Editing.')
            htprice1.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('Unfreeze Price 1 Editing.')
            htprice1.ColumnEditable = [false true];
        end
    end

    function hprice2fz_callback(hObject,eventdata)
        % Freeze Intial Guess of Price Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Price 2 Table Editing.')
            htprice2.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('UnFreeze Price 2 Table Editing.')
            htprice2.ColumnEditable = [false true];
        end
    end

% ---------- Model Switching Controls -------------------------------------
    function solveprice(source,event)
        % Switch between Exogenous and Endogenous Price
        if strcmp(event.NewValue.String,'Exogenous') == 1
            endo_flag = 0;
            cd(basedir);
            cd bin_trans;
            fid = fopen('endo_flag.txt','w');
            fprintf(fid,'%d\n',endo_flag);
            fclose(fid); 
            cd(basedir);
        elseif strcmp(event.NewValue.String,'Endogenous') == 1
            endo_flag = 1;
            cd(basedir);
            cd bin_trans;
            fid = fopen('endo_flag.txt','w');
            fprintf(fid,'%d\n',endo_flag);
            fclose(fid); 
            cd(basedir);
        end
        display(['Solve for Price: ' event.NewValue.String]);
    end

    function extendpath(source,event)
        % Switch between extending path or not
        if strcmp(event.NewValue.String,'No') == 1
            extend_flag = 0;
            cd(basedir);
            cd bin_trans;
            fid = fopen('extend_flag.txt','w');
            fprintf(fid,'%d\n',extend_flag);
            fclose(fid); 
            cd(basedir);
        elseif strcmp(event.NewValue.String,'Yes') == 1
            extend_flag = 1;
            cd(basedir);
            cd bin_trans;
            fid = fopen('extend_flag.txt','w');
            fprintf(fid,'%d\n',extend_flag);
            fclose(fid); 
            cd(basedir);
        end
        display(['Extend Path: ' event.NewValue.String]);
    end

% ---------- Calling Executables to Solve the Model ----------------------
    function hsolve_callback(hObject, eventdata)
        cd(basedir);
        cd bin_trans;
        
        % Write current data to local files
        % Model Parameters
        junk = htconstant.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        % Fiscal SS 1
        junk = htpolicy1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Fiscal SS 2
        junk = htpolicy2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Price SS 1
        junk = htprice1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Price SS 2
        junk = htprice2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Length of Transition
        junk = htlength.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);
        
        % Calling Executables
        dos('trfs_trans.exe');
        
        msg = ['Select a folder to save simulation results. '...
            'Path and name cannot contain space.'];
        fdname = uigetdir(basedir,...
            msg);
        doscmd = ['copy *.txt ' fdname];
        [junka, junkb] = dos(doscmd);
        if (junka == 1)
            disp('Path and filename cannot contain space.')
        end
        cd(basedir);
    end

    function hquick_callback(hObject, eventdata)
        cd(basedir);
        dirstatus = exist('Quick_Solve_Trans','dir');
        if dirstatus == 0
            mkdir('Quick_Solve_Trans');
        elseif dirstatus == 7
            disp('Folder Quick_Solve_Trans Already Exists.');
        else
        end        
        cd bin_trans;
        
        % Write current data to local files
        % Model Parameters
        junk = htconstant.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);
        % Fiscal SS 1
        junk = htpolicy1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Fiscal SS 2
        junk = htpolicy2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('fiscal2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Price SS 1
        junk = htprice1.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price1_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);        
        % Price SS 2
        junk = htprice2.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('price2_matlab.txt','w');
        fprintf(fid,'%16.10f\n',prt);
        fclose(fid);                
        % Length of Transition
        junk = htlength.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('length_matlab.txt','w');
        fprintf(fid,'%d\n',prt);
        fclose(fid);
        
        % Calling Fortran Routines
        dos('trfs_trans.exe');
                
        [junka, junkb] = dos('copy *.txt ..\Quick_Solve_Trans');
        cd(basedir);
    end

% ---------- Calling System PDF Viewer to Show Documentations--------------
    function menudocu_callback(hObject, eventdata, handles)
        % Documentation
        cd(basedir);
        cd documentation;
        winopen Guide.pdf;
        cd(basedir);
    end

%     function menututorial_callback(hObject, eventdata, handles)
%         % Tutorial
%         cd(basedir);
%         cd documentation;
%         winopen tutorial.pdf;
%         cd(basedir);
%     end
% 
%     function menumanual_callback(hObject, eventdata, handles)
%         % Tutorial
%         cd(basedir);
%         cd documentation;
%         winopen release_notes.pdf;
%         cd(basedir);
%     end
end

%  ========================================================================
%               External Callback Functions     
%  ========================================================================
% Loading External Variables is Required
% Presenting Comparison of Steady States
    function hexport_callback(hObject, eventdata)
        % Changes in macro aggregates
        basedir = getappdata(hObject.Parent,'BaseDir');
        cd(basedir);
        Exp_Excel_Trans;
        cd(basedir);
    end    
    
    function hwelf_callback(hObject, eventdata)
        % Changes in macro aggregates
        basedir = getappdata(hObject.Parent,'BaseDir');
        cd(basedir);
        Welf_Trans;
        cd(basedir);
    end        
