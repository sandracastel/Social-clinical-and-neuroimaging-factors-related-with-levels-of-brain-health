clear
clc

% -------------------- Definición de directorios
direc = 'H:\RedLat_new\REDLAT_BASE_DEFINITIVA\T1';
xml_direc = fullfile(direc,'report');
results_direc = fullfile('H:\RedLat_new\REDLAT_BASE_DEFINITIVA', 'GM_TIV_AAL');
append = 'Redlat';

% -------------------- Definición de directorios




if not(exist(results_direc)),mkdir(results_direc);end

% Verificar si el directorio XML contiene archivos
cd(xml_direc);
sublist = dir('*.xml');

if isempty(sublist)
    error('No se encontraron archivos XML en el directorio %s.', xml_direc);
end

% Crear lista de rutas completas de archivos XML
Images = {};
for iSub = 1:length(sublist)
    disp(['Listing subject ', num2str(iSub), ', ', sublist(iSub).name]);
    Images{iSub} = fullfile(sublist(iSub).folder, sublist(iSub).name);
end
save(fullfile(results_direc,['namelist_',append]),'sublist')

%% Configuración del batch en SPM
matlabbatch{1}.spm.tools.cat.tools.calcvol.data_xml = Images';
matlabbatch{1}.spm.tools.cat.tools.calcvol.calcvol_TIV = 1;
matlabbatch{1}.spm.tools.cat.tools.calcvol.calcvol_savenames = 0;
matlabbatch{1}.spm.tools.cat.tools.calcvol.calcvol_name = fullfile(results_direc, [append, '_TIV.txt']);

%% Ejecución del batch con manejo de errores
spm('defaults', 'FMRI');
spm_jobman('run', matlabbatch);

% Verificar si el archivo de resultados fue generado
result_file = fullfile(results_direc, [append, '_TIV.txt']);
if isfile(result_file)
    disp('El archivo de TIV se generó correctamente.');
else
    warning('El archivo de TIV no se generó correctamente.');
end
