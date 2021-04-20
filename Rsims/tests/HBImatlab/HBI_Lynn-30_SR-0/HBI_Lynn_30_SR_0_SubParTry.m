% Add CBM toolbox
addpath('/home/sven/Documents/ModularGraphs-RL/Rsims/tests/HBImatlab/codes');
% Load Data
multichoice = readtable('LynnExp_pp-30.csv');
% Coerce into struct
data = {};
for i = subnr
    curpp.SR_choice = multichoice.Lynn_choice(multichoice.pp == i);
    curpp.tr          = multichoice.tr(multichoice.pp==i);
    curpp.v           = multichoice.v(multichoice.pp==i);
    curpp.goal        = multichoice.goal(multichoice.pp==i);
    curpp.stepsleft   = multichoice.stepsleft(multichoice.pp==i);
    data{end+1} = curpp;
end
% Set Priors
prior_SR   = struct('mean', zeros(5,1), 'variance',10);
prior_Lynn = struct('mean', zeros(3,1), 'variance',10);

mkdir('lap_subjects');
fname_lynn_subj = fullfile('lap_subjects',['lap_lynn_',num2str(subnr),'.mat']);
cbm_lap(data, @Lynn_choiceLL, prior_Lynn, fname_lynn_subj);