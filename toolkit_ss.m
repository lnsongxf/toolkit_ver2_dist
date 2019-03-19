% -------------------------------------------------------------------------
%                           PROGRAM DESCRIPTION
%  ------------------------------------------------------------------------
%  Purpose:
%   - The main UI for solving the steady state equilibrium.
% 
%  Functions:
%   - Buttons :
%       - Solve and Save : call ss_infra.exe to solve the model and 
%               save the the results.
%       - Solve          : call ss_infra.exe to solve the model.
%       - Export         : export the results in Excel format.
%       - Welfare        : welfare decomposition for steady state.
%   - Tables :
%       - Parameter values to be passed to the programs.
%   - Radio Button:
%       - Solve for prices or evaluate equilibrium at given prices.
%  Author:
%      Xin Tang @ International Monetary Fund, Spring 2019
% 
%  Record of Revisions:
%        Date                Description of Changes
%    ===========        =================================
%     01/23/2019                 Original Code
%  ========================================================================

function toolkit_ss

%  ========================================================================
%                       Layout of the GUI
%  ========================================================================
f = figure('Visible','off','NumberTitle','off',...
    'MenuBar','none','Toolbar','none',...
    'Name','Tookit Version 2.0: Steady State',...
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
% Model Selection
% Set uniform cash transfer as default
% model = 1;

% ================ Initialize the Data Table ==============================
cali_flag = 0;             % Endogenous Price Solving Flag
cd(basedir);
cd bin_ss;
fid = fopen('flag.txt','w');
fprintf(fid,'%d\n',cali_flag);
fclose(fid); 
cd(basedir);

% ------------- Model Parameters ------------------------------------------
%   - Ethiopia as Benchmark
abar   = 0.0008;
psi    = 0.4945;
gamma  = 0.8168;
sigmar = 0.2338;
sigmau = 0.7500;
taua   = 0.0545;
taur   = 0.1522;
tauw   = 0.0555;
za     = 0.7434;
zm     = 8.2858;
z      = 0.6845;
% Urban Population share fixed during calibration
muu    = 0.28;
mur    = 0.69;
muf    = 0.03;
cali_param =...
    {'Service Preference',         psi;...
     'Manufacturing Preference',   gamma;...
     'Rural Variance',             sigmar;...
     'Urban Variance',             sigmau;...
     'Value Added Tax',            taua;...
     'Profit Tax',                 taur;...
     'Wage Tax',                   tauw;...
     'Agricultural Productivity',  za;...
     'Manufacturing Productivity', zm;...
     'Exporting Productivity',     z;...
     'Urban Popu (Exogenous)',     muu;...
     'Rural Popu (Exogenous)',     mur;...
     'Farmer Popu (Exogenous)',    muf;...
     'Subsistence Level',          abar};

% ------------- Model Targets----------------------------------------------
cs_c        = 0.21;     % Service share in consumption
cm_c        = 0.33;     % Manufacturing share in consumption
gini_r      = 0.26;     % Rural Gini
gini_u      = 0.40;     % Urban Gini
tax_gdp     = 0.08;     % Tax to GDP ratio
corp_tax    = 0.30;     % Share of Corporate Tax in Total Tax
inc_tax     = 0.17;     % Share of Income Tax in Total Tax
cs_y        = 0.16;     % Service Share in GDP
cm_y        = 0.33;     % Manufacturing Share in GDP
ex_y        = 0.083;    % Export Share in GDP
data_param = ...
    {'Service in Consumption',           cs_c;...
     'Manufacturing in Consumption',     cm_c;...
     'Rural Consumtpion Gini',           gini_r;...
     'Urban Consumption Gini',           gini_u;...
     'Tax to GDP ratio',                 tax_gdp;...
     'Corporate Tax in Tax',             corp_tax;...
     'Income Tax in Tax',                inc_tax;...
     'Service in GDP',                   cs_y;...
     'Manufacturing in GDP',             cm_y;...
     'Exporting in GDP',                 ex_y}; 

% ------------- Initial Guess for Equilibrium Prices ----------------------
eprice = [12.6424418446;...
          13.0389498488;...
          1.9851162474;...
          3.0710397704;...
          0.0026516396];
price_param =...
    {'Service',         eprice(1);...
     'Food',            eprice(2);...
     'Urban Labor',     eprice(3);...
     'Rural Labor',     eprice(4);...
     'Interest Rate',   eprice(5)};

% ------------- Transfers -------------------------------------------------
tu = 0;
tr = 0;
transfer_param =...
    {'Urban Transfer',tu;...
     'Rural Transfer',tr};

% ------------- Infrastructure---------------------------------------------
infra_rural = 0;
infra_urban = 0;
infra_alpha = 0;
infra_param = ...
    {'Infra Invest Rural'       , infra_rural;...
     'Infra Invest Urban'       , infra_urban;...
     'Output Elasticity'        , infra_alpha};

% Keep a mirror of all default parameters
cali_default = cali_param;
data_default = data_param;
price_default = price_param;
transfer_default = transfer_param;
infra_default = infra_param;

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

% % --------------- Pop-up Menu ---------------------------------------------
% % Select the Type of Policy Experiments
% hmodel = uicontrol('Parent',f,'Style','popupmenu',...
%     'String',{'UBI','Infrastructure'},...
%     'Position',[720,615,125,50],...
%     'FontSize',10,'Callback',@model_sel_callback);

% --------------- Ratio Button --------------------------------------------
% Controlling for whether to solve for equilibrium prices
hbgh = uibuttongroup('Parent',f,...
    'Title','Solving for Market Clearing Prices?',...
    'Position',[.7 .24 .25 .2],...
    'SelectionChangedFcn',@solveprice);
hrbh1 = uicontrol('Parent',hbgh,...
    'Style','radiobutton',...
    'String','Exogenous',...
    'Units','normalized',...
    'Position',[.1 .6 .8 .2]);
hrbh2 = uicontrol('Parent',hbgh,...
    'Style','radiobutton',...
    'String','Endogenous',...
    'Units','normalized',...
    'Position',[.1 .2 .8 .2]);

% --------------- Data Tables ---------------------------------------------
% With Toggle Button to control for whether tables are editable
% Model Parameters
str_cali = uicontrol('Parent',f,...
    'Style','text','String','Model Parameters',...
    'Position', [20 660 150 30], 'FontSize',10);
htcali = uitable('Parent',f,...
            'Position', [40 385 308 284], ...
            'Data',cali_param,...
            'ColumnEditable', [false true], ...
            'ColumnName', {'Parameters','Values'}, ...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{220 86},...
            'CellEditCallback',@cali_callback);
hcali_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[40 350 125 30],...
    'Callback',@hcalifz_callback);

% Model Targets
str_data = uicontrol('Parent',f,...
    'Style','text','String','Data Targets',...
    'Position', [360 660 80 30], 'FontSize',10);
htdata = uitable('Parent',f,...
            'Position', [360 385 300 284], ...
            'Data',data_param,...
            'ColumnEditable',[false true],...
            'ColumnName',{'Moments', 'Targets'},...
            'ColumnFormat',{'char','numeric'},...
            'FontSize',10,...
            'RowName',{},...
            'ColumnWidth',{220 78},...
            'CellEditCallback',@data_callback);
hdata_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[360 350 125 30],...
    'Callback',@hdatafz_callback);

% Initial Guess for Prices
str_eps = uicontrol('Parent',f,...
    'Style','text','String','Guess for Prices',...
    'Position', [35 295 110 25], 'FontSize',10);
htprice = uitable('Parent',f,...
    'Position', [40 170 185 130], ...
    'Data', price_default,...
    'ColumnEditable',[false true],...
    'ColumnName',{'Markets','Prices'},...
    'ColumnFormat',{'char','numeric'},...
    'FontSize',10,...
    'RowName',{},...
    'ColumnWidth',{120 63},...
    'CellEditCallback',@price_callback);
hprice_fz = uicontrol('Parent',f,'Style','togglebutton',...
    'String','Freeze Editing','Value',0,...
    'BackgroundColor',[.9 .9 .9],'Position',[40 130 125 30],...
    'Callback',@hpricefz_callback);

% Transfers
str_trans = uicontrol('Parent',f,...
    'Style','text','String','Transfers',...
    'Position', [230 295 100 25], 'FontSize',10);
httrans = uitable('Parent',f,...
    'Position',[250 170 170 130],...
    'Data', transfer_param,...
    'RowName',{},...
    'ColumnFormat',{'char','numeric'},...
    'ColumnWidth',{125 53},...
    'ColumnName',{'Variables','Values'},...
    'FontSize',10,...
    'ColumnEditable',[false true],...
    'CellEditCallback',@transfer_callback);

% Infrastructure
str_infra = uicontrol('Parent',f,...
    'Style','text','String','Infrastructure',...
    'Position', [430 295 100 25], 'FontSize',10);
htinfra = uitable('Parent',f,...
    'Position',[440 170 170 130],...
    'Data', infra_param,...
    'RowName',{},...
    'FontSize',10,...
    'ColumnWidth',{150,53},...
    'ColumnFormat',{'char','numeric'},...
    'ColumnName',{'Variables','Values'},...
    'ColumnEditable',[false true],...
    'CellEditCallback',@infrastructure_callback);

% Change units to normalized so components resize automatically.
f.Units         = 'normalized';
hsolve.Units    = 'normalized';
hquick.Units    = 'normalized';
hexport.Units   = 'normalized';
hwelf.Units     = 'normalized';
hbgh.Units      = 'normalized';
% hmodel.Units    = 'normalized';
htcali.Units    = 'normalized';
htprice.Units   = 'normalized';
htdata.Units    = 'normalized';
% htaccu.Units    = 'normalized';
htdefault.Units = 'normalized';
hcali_fz.Units  = 'normalized';
hdata_fz.Units  = 'normalized';
hprice_fz.Units = 'normalized';
httrans.Units   = 'normalized';
htinfra.Units   = 'normalized';
hsave.Units     = 'normalized';
hload.Units     = 'normalized';
hupdate.Units   = 'normalized';
str_cali.Units  = 'normalized';
str_data.Units  = 'normalized';
str_eps.Units  = 'normalized';
str_trans.Units  = 'normalized';
str_infra.Units  = 'normalized';

% Vertially Align Buttons
% align([hsolve,hplot,haggr,hquick],'Center','None');
align([hsolve,hquick,hexport,hwelf],'Center','None');

% Move the GUI to the center of the screen.
movegui(f,'center')
f.Visible = 'on';

%  ========================================================================
%                   Nested Callback Functions
%  ========================================================================
% Callback functions with a shared namespace
% -------------------- Table Editing --------------------------------------
    % Model Parameters
    function cali_callback(hObject,callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval;
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_ss;
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    % Calibration Targets
    function data_callback(hObject, callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval; 
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_ss;
        fid = fopen('targets_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    % Initial Guess for Equilibrium Price
    function price_callback(hObject, callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval; 
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_ss;
        fid = fopen('ep_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    % Transfers
    function transfer_callback(hObject, callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval; 
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_ss;
        fid = fopen('transfer_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        cd(basedir);
    end

    % Infrastructure Investment
    function infrastructure_callback(hObject, callbackdata)
        cd(basedir);
        numval = eval(callbackdata.EditData);
        r = callbackdata.Indices(1);
        c = callbackdata.Indices(2);
        hObject.Data{r,c} = numval; 
        junk = hObject.Data;
        prt = cell2mat(junk(:,2));
        cd bin_ss;
        fid = fopen('infra_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        cd(basedir);
    end

% ---------- Save and Load Previous Parameterizations ---------------------
    function hdflt_callback(hObject, eventdata)
        % Set all parameters to default Ethiopia level
        htcali.Data     = cali_default;
        htdata.Data     = data_default;
        htprice.Data    = price_default;
        httrans.Data    = transfer_default;
        htinfra.Data    = infra_default;
 
        % Write default data to local files
        cd(basedir)
        cd bin_ss;
        % Model Parameters
        junk = htcali.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Calibration Targets
        junk = htdata.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('targets_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);        
        % Initial Guess of Prices
        junk = htprice.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('ep_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);        
        % Transfers
        junk = httrans.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('transfer_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Infrastructure
        junk = htinfra.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('infra_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);

        % Return control to base directory
        cd(basedir);
    end

    function hsave_callback(hObject, eventdata)
        % Save current parameterizations to local files
        cali_param      = htcali.Data;
        data_param      = htdata.Data;
        price_param     = htprice.Data;
        transfer_param  = httrans.Data;
        infra_param     = htinfra.Data;
        uisave({'cali_param','data_param','price_param',...
            'transfer_param','infra_param'}); 
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
           varlist = {'cali_param','data_param','price_param',...
               'transfer_param','infra_param'};
           load(filename,varlist{:});
           htcali.Data  = cali_param;
           htdata.Data  = data_param;
           htprice.Data = price_param;
           httrans.Data = transfer_param;
           htinfra.Data = infra_param;
       end
       cd(basedir);
    end    

    function hupdate_callback(hObject, eventdata)
        % Update Current Parameterizations to Text Files
        cd(basedir);
        cd bin_ss;
        
        % Write current data to local files
        % Model Parameters
        junk = htcali.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Calibration Targets
        junk = htdata.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('targets_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Initial Guess of Prices
        junk = htprice.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('ep_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);        
        % Transfers
        junk = httrans.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('transfer_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Infrastructure
        junk = htinfra.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('infra_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        
        cd(basedir);
    end

% ---------- Error-proofing Features --------------------------------------
    function hcalifz_callback(hObject,eventdata)
        % Freeze Model Parameters Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Parameter Table Editing.')
            htcali.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('Unfreeze Parameter Table Editing.')
            htcali.ColumnEditable = [false true];
        end
    end

    function hdatafz_callback(hObject,eventdata)
        % Freeze Calibration Targets Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Targets Table Editing.')
            htdata.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('Unfreeze Targets Table Editing.')
            htdata.ColumnEditable = [false true];
        end
    end

    function hpricefz_callback(hObject,eventdata)
        % Freeze Intial Guess of Price Editing
        button_state = get(hObject,'Value');
        if button_state == get(hObject, 'Max')
            disp('Freeze Prices Table Editing.')
            htprice.ColumnEditable = [false false];
        elseif button_state == get(hObject,'Min')
            disp('UnFreeze Prices Table Editing.')
            htprice.ColumnEditable = [false true];
        end
    end

% ---------- Model Switching Controls -------------------------------------
    function solveprice(source,event)
        % Switch between Exogenous and Endogenous Price
        if strcmp(event.NewValue.String,'Exogenous') == 1
            cali_flag = 0;
            cd(basedir);
            cd bin_ss;
            fid = fopen('flag.txt','w');
            fprintf(fid,'%d\n',cali_flag);
            fclose(fid); 
            cd(basedir);
        elseif strcmp(event.NewValue.String,'Endogenous') == 1
            cali_flag = 1;
            cd(basedir);
            cd bin_ss;
            fid = fopen('flag.txt','w');
            fprintf(fid,'%d\n',cali_flag);
            fclose(fid); 
            cd(basedir);
        end
        display(['Current: ' event.NewValue.String]);
    end

%     function model_sel_callback(source, eventdata)
%         % Switch between policy experiments with cash transfers and/or
%         %   infrastructure building.
%         str = source.String;
%         val = source.Value;
%         switch str{val};
%             case 'UBI'
%                 model = 1;
%                 tmpmsg = ['The model takes only cash transfer '...
%                     'policy parameters. Editing of Infrastructure '...
%                     'parameters is disabled.'];
%                 disp(tmpmsg);
%                 disp('Universal Basic Income.');
%                 httrans.ColumnEditable = [false true];
%                 htinfra.ColumnEditable = [false false];
% 
%             case 'Infrastructure'
%                 model = 2;
%                 tmpmsg = ['The model takes only infrastructure '...
%                     'policy parameters. Editing of cash transfer '...
%                     'parameters is disabled.'];
%                 disp(tmpmsg);
%                 disp('Infrastructure Investment.')
%                 httrans.ColumnEditable = [false false];
%                 htinfra.ColumnEditable = [false true];                
%         end       
%     end

% ---------- Calling Executables to Solve the Model ----------------------
    function hsolve_callback(hObject, eventdata)
        cd(basedir);
        cd bin_ss;
        
        % Write current data to local files
        % Model Parameters
        junk = htcali.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Calibration Targets
        junk = htdata.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('targets_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Initial Guess of Prices
        junk = htprice.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('ep_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);        
        % Transfers
        junk = httrans.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('transfer_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Infrastructure
        junk = htinfra.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('infra_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        
        % Calling Executables
        dos('ss_infra.exe');

%         switch model
%             case 1
%                 dos('SS_Ubi.exe');
%             case 2
%                 dos('Infrastructure.exe')
%             otherwise
%                 disp('Error! Model not selected appropriately.')
%         end
        
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
        dirstatus = exist('Quick_Solve','dir');
        if dirstatus == 0
            mkdir('Quick_Solve');
        elseif dirstatus == 7
            disp('Folder Quick_Solve Already Exists.');
        else
        end        
        cd bin_ss;
        
        % Write current data to local files
        % Model Parameters
        junk = htcali.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('param_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Calibration Targets
        junk = htdata.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('targets_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Initial Guess of Prices
        junk = htprice.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('ep_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);        
        % Transfers
        junk = httrans.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('transfer_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        % Infrastructure
        junk = htinfra.Data;
        prt = cell2mat(junk(:,2));
        fid = fopen('infra_matlab.txt','w');
        fprintf(fid,'%14.9f\n',prt);
        fclose(fid);
        
        % Calling Fortran Routines
        dos('ss_infra.exe');
        
%         switch model
%             case 1
%                 dos('SS_Ubi.exe');
%             case 2
%                 dos('Infrastructure.exe')
%             otherwise
%                 disp('Error! Model not selected appropriately.')
%         end
        
        [junka, junkb] = dos('copy *.txt ..\Quick_Solve');
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
        Exp_Excel_SS;
        cd(basedir);
    end    
    
    function hwelf_callback(hObject, eventdata)
        % Changes in macro aggregates
        basedir = getappdata(hObject.Parent,'BaseDir');
        cd(basedir);
        Welf_SS;
        cd(basedir);
    end        