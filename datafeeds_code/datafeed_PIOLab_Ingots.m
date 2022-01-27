function datafeed_PIOLab_Ingots(handles)

if isfolder('/import/emily1/isa/IELab/Roots/PIOLab/')
system('Rscript /import/emily1/isa/IELab/Roots/PIOLab/Rscripts/datafeeds_code/datafeed_PIOLab_Ingots.R','-echo');
end

if isfolder('/data/WULab/Roots/PIOLab/')
system('Rscript /data/WULab/Roots/PIOLab/Rscripts/datafeeds_code/datafeed_PIOLab_Ingots.R','-echo');
end

end
