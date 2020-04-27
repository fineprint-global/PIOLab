function [RegMap,IndMap,ProdMap]=Ind30Pro40v1_InitialEstimate(handles)

    disp('Launching initial estimate for the extended PIOT version 1');
    
    % Write handles variables into mat-file for R to read
    % Working directory
    filename = [handles.processeddatadir,'WorkingDirectory4R.mat'];
    out = handles.motherALANGdir;
    save(filename,'out');
    
    % Write region aggregator to file
    filename = [handles.processeddatadir,'RegionAggFile4R.mat'];
    out = handles.regionaggfile;
    save(filename,'out');
    
    % Region aggregator
    RegMap = csvread(handles.regionagg);
    
    if size(RegMap,1) > size(RegMap,2) % check orientation of aggregator
        RegMap  = RegMap';
    end
    
    reg_proxy = ones(size(RegMap,2),1);
    RegMap = prorate(RegMap,'col_proxy',reg_proxy);
    
    % Product aggregator
    ProdMap = csvread(handles.sectoraggprod);
    
    if size(ProdMap,1) > size(ProdMap,2) % check orientation of aggregator
        ProdMap  = ProdMap';
    end
    
    prod_proxy = ones(size(ProdMap,2),1);
    ProdMap = prorate(ProdMap,'col_proxy',prod_proxy);
    
    % Industry aggregator 
    IndMap = csvread(handles.sectoragg);
    
    if size(IndMap,1) > size(IndMap,2) % check orientation of aggregator
        IndMap  = IndMap';
    end
    
    ind_proxy = ones(size(IndMap,2),1);
    IndMap = prorate(IndMap,'col_proxy',ind_proxy);
    
    command = 'Rscript /import/emily1/isa/IELab/Roots/PIOLab/Rscripts/IEfeeds_code/Ind30Pro40v1_InitialEstimate.R';
    system(command,'-echo');
        
    end    