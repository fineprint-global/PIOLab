function [RegMap,IndMap,ProdMap] = datafeed_PIOLab_InitialEstimate(handles)

% Master file for IE feeds

    if handles.nonsurvey == 1
    
       [RegMap,IndMap,ProdMap]=Ind30Pro39v1_InitialEstimate(handles);

    end

end