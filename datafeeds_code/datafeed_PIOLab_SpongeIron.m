function datafeed_PIOLab_SpongeIron(handles)

if isfolder('/import/emily1/isa/IELab/Roots/PIOLab/')
system('Rscript /import/emily1/isa/IELab/Roots/PIOLab/Rscripts/datafeeds_code/datafeed_PIOLab_SpongeIron.R','-echo');
end

if isfolder('/data/WULab/Roots/PIOLab/')
system('Rscript /data/WULab/Roots/PIOLab/Rscripts/datafeeds_code/datafeed_PIOLab_SpongeIron.R','-echo');
end

end
