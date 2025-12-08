clear
clc

% -------------------- Definición de directorios
direc = 'H:\RedLat_new\REDLAT_BASE_DEFINITIVA\T1';
label_direc = fullfile(direc,'label');
results_direc = fullfile('H:\RedLat_new\REDLAT_BASE_DEFINITIVA', 'GM_TIV_AAL');
append = 'Redlat';
% -------------------- Definición de directorios


% Verificación y carga de la lista de sujetos
if ~isfile(fullfile(results_direc, ['namelist_', append, '.mat']))
    error('El archivo de namelist no se encuentra en: %s', results_direc);
end
subjects = load(fullfile(results_direc, ['namelist_', append]), 'sublist');

% Verificación y carga de TIV
if ~isfile(fullfile(results_direc, [append, '_TIV.txt']))
    error('El archivo TIV no se encuentra en: %s', results_direc);
end
TIV = readtable(fullfile(results_direc, [append, '_TIV.txt']));
TIV = table2array(TIV);

% Verificación del tamaño de TIV y lista de sujetos
if length(TIV) ~= length(subjects.sublist)
    error('El tamaño de TIV no coincide con la cantidad de sujetos.');
end

cd(label_direc)

% Inicialización de matrices
GM_AAL1 = [];
GM_HO1 = [];
GM_hcp1 = [];
data = {};

% Iteración sobre la lista de sujetos


for i = 1:length(subjects.sublist)
    temp = dir(['catROI_', subjects.sublist(i).name(5:end-4), '.mat']);
    
    % Verificación de existencia de archivo .mat
    if isempty(temp)
        disp(['No se encontró archivo para: ', subjects.sublist(i).name]);
        continue;
    elseif length(temp) > 1
        error(['Se encontraron múltiples archivos para: ', subjects.sublist(i).name]);
    end
    
    disp([subjects.sublist(i).name, '     ', temp.name]);
    load(temp.name);  % Cargar el archivo correspondiente
    
    % Almacenar la información normalizada
    data{i, 1} = subjects.sublist(i).name;
    GM_AAL1(i, :) = S.AAL1.data.Vgm(1:116, :)' ./ TIV(i, 1);
    GM_HO1(i, :) = S.Oxford_Harvard.data.Vgm(1:132, :)' ./ TIV(i, 1);
    GM_hcp1(i, :) = S.hcp.data.Vgm(1:360, :)' ./ TIV(i, 1);
end

% Crear tablas con los resultados
GM_AAL = [data, num2cell(GM_AAL1)];
GM_HO = [data, num2cell(GM_HO1)];
GM_hcp = [data, num2cell(GM_hcp1)];

% Guardar los resultados como archivos CSV
cd(results_direc);
writetable(cell2table(GM_AAL), [append, '_VGM_AAL_.csv'], 'WriteVariableNames', false);
% writetable(cell2table(GM_HO), 'Atrophy_HO.csv', 'WriteVariableNames', true);
% writetable(cell2table(GM_hcp), 'Atrophy_hcp.csv', 'WriteVariableNames', true);
