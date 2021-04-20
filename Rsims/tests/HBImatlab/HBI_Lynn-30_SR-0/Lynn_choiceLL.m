function[loglik] = Lynn_choiceLL(parameters, subj)
% Function that takes in Independent Variables:
%   - Choices (DV)
%   - Trial number
%   - Node ID (v)
%   - Goal ID
%   - Stepsleft
% Parameters:
%   - betap
%   - Slope (alpha)
%   - Regression weight (beta)
%% Get parameters and data vectors
betap = exp(parameters(1));
alpha = parameters(2);
beta  = parameters(3);

choices   = subj.SR_choice;
tr        = subj.tr;
v         = subj.v;
goal      = subj.goal;
stepsleft = subj.stepsleft-1;
%% Initialize variables for learning
Ahat    = zeros(20);
ntilde  = zeros(20);
Lynn_EV = zeros(length(v),1);
tridx   = 1;

%% Compute Ahat and related EV
for trial = 1:max(tr)
    % Store current episode (trial)
    ep = v(tr == trial);
    % Get EV for first trial in episode
    I = zeros(1,20);   % State occupation vector
    I(ep(1)) = 1;
    Ahat_goal =  Ahat; % Transition matrix with absorbing goal state
    Ahat_goal(goal(tridx),:) = 0;
    hitVec = zeros(1,15); % Hitting chances
    for s = stepsleft(tridx):-1:1
        hitC = I * Ahat_goal^s;
        hitVec(s) = hitC(goal(tridx));
    end
    Lynn_EV(tridx) = sum(hitVec) * 5 + (1-sum(hitVec)) * -1;
    tridx = tridx+1;
    % Run through rest of the trial
    for expr = 2:length(ep)
        % Update ntilde and Ahat
        %   Get boltzmann dist.
        Z = exp(-betap * [1:(expr-1)]);
        Pdt = Z / sum(Z);
        for epdt = 1:(expr-1)
            ntilde(ep(expr-epdt), ep(expr)) = ntilde(ep(expr-epdt), ep(expr)) + Pdt(epdt);
            Ahat(ep(expr-epdt),:) = ntilde(ep(expr-epdt),:) / sum(ntilde(ep(expr-epdt),:));
        end
        % Get Lynn EV
        I = zeros(1,20);   % State occupation vector
        I(ep(expr)) = 1;
        Ahat_goal =  Ahat; % Transition matrix with absorbing goal state
        Ahat_goal(goal(tridx),:) = 0;
        hitVec = zeros(1,15); % Hitting chances
        for s = stepsleft(tridx):-1:1
            hitC = I * Ahat_goal^s;
            hitVec(s) = hitC(goal(tridx));
        end
        Lynn_EV(tridx) = sum(hitVec) * 5 + (1-sum(hitVec)) * -1;
        tridx = tridx+1;
    end
end
%% Get binary choice likelihood based on Lynn EV
ll = 0; % Track log-likelihood
for t = 1:length(v)
    binp = 1/(1+exp(-(alpha + beta*Lynn_EV(t))));
    ll = ll + log(binopdf(choices(t), 1, binp)+eps);
end
loglik = ll;
end
        












