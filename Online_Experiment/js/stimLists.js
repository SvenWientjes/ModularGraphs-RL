// List of room images (idx = node nr)
var roomsdef = ['img/Balloon.png', 'img/Basket.png', 'img/Book.png', 'img/Boot.png', 'img/Cap.png', 
'img/Clipboard.png', 'img/Floppy.png', 'img/Fryingpan.png', 'img/Jar.png',
'img/Lightbulb.png', 'img/Mug.png', 'img/Ovenmitt.png', 'img/Soapdispenser.png',
'img/Toaster.png', 'img/Toiletpaper.png'];

// List of goal images (idx = node nr -> correspond to rooms!)
var goalsdef = ['img/Balloon_goal.png', 'img/Basket_goal.png', 'img/Book_goal.png', 'img/Boot_goal.png', 'img/Cap_goal.png', 
'img/Clipboard_goal.png', 'img/Floppy_goal.png', 'img/Fryingpan_goal.png', 'img/Jar_goal.png',
'img/Lightbulb_goal.png', 'img/Mug_goal.png', 'img/Ovenmitt_goal.png', 'img/Soapdispenser_goal.png',
'img/Toaster_goal.png', 'img/Toiletpaper_goal.png'];

// List of room images for practice trials
var pracRooms = ['img/coinIllustrate.png', 'img/Iron-prac.png', 'img/Lantern-prac.png', 'img/Pot-prac.png', 'img/Calculator-prac.png'];

/* ---- JSpsych Trial Variables ---- */
// Define the goalcue by getting from corresponding goalorder idx
var goalcue = {
    type: 'html-keyboard-response',
    //stimulus_width: 1920,
    choices: ['z','m'],
    stimulus: function(){return '<div id="GoalcueDiv" style="margin:0 auto;"><canvas id="GoalcueCv" width="1920" height="1080" style="background:url('+jsPsych.timelineVariable('thegoal',true)+')"></canvas></div>'},
    on_load: function(){
        var canvas = document.getElementById('GoalcueCv');
        cst = canvas.getContext("2d");
        cst.font = '40px VideoGame';
        cst.textAlign = 'center';
        cst.fillStyle = 'black';
        cst.fillText(total_reward, canvas.width/2, 50);
    }
    //stimulus: goals[goalorder[i]],
}

var trial = {
    type: 'html-keyboard-response',
    trial_duration: 1500,
    //stimulus_width: 1920,
    choices: ['z','m'], //down, up,
    response_ends_trial: false,
    stimulus: function(){var html='<div id="StimulusDiv" style="margin:0 auto;">';
        html+='<canvas id="StimulusCv" width="1920" height="1080" style="background:url('+jsPsych.timelineVariable('stimulus',true)+')"></canvas></div>';
        return html;}, //node_imgs in order determined by trialorder
    data: {
        miniblock: jsPsych.timelineVariable('mbidx'),
        nSteps: jsPsych.timelineVariable('trialidx'),
        totRew: function(){
            return total_reward;
        },
        trRew: function(){
            return trRew;
        },
        trBet: function(){
            return trBet;
        },
        mbPunish: function(){
            return mbPunish;
        },
        node: jsPsych.timelineVariable('nodeid'), //node nr determined by idx of rooms
        goalnode: jsPsych.timelineVariable('goalidx'),
    },
    on_load: function(){StimC(trBet, total_reward)},
    on_finish: function(data) {
        if (data.key_press == 90 && data.node == data.goalnode) { // Bet down on goal
            nWin += 1;                             // Increment nWin
            trBet = Math.max(trBet,0);             // Do not increase bet by one
            data.trBet = trBet;                    // Correct current trial bet
            next_total_reward = Math.max(total_reward + trBet,0);
            data.totRew = next_total_reward;            // Correct current trial reward
            trRew = trBet;                         // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'downgoal';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = false;                          // Prevent carryover misses
            data.choiceVar = 0;                    // Integer coded variable checking responses
            jsPsych.endCurrentTimeline();
        };
        if (data.key_press == 77 && data.node == data.goalnode) { // Bet up on goal
            nWin += 1;                             // Increment nWin
            trBet = Math.max(trBet+1,0);           // Increase bet by one
            data.trBet = trBet;                    // Correct current trial bet
            next_total_reward = Math.max(total_reward + trBet,0);
            data.totRew = next_total_reward;       // Correct current trial reward
            trRew = trBet;                         // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'upgoal';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = false;                          // Prevent carryover misses
            data.choiceVar = 1;                    // Integer coded variable checking responses
            jsPsych.endCurrentTimeline();
        };
        if (data.key_press == 90 && data.nSteps != 14 && data.node != data.goalnode) { // Down bet
            trBet = Math.max(trBet,0);       // Do not increase bet by one
            data.trBet = trBet;              // Correct current trial bet
            data.decision = 'down';          // Append participant decision to trial data
            data.choiceVar = 0;                    // Integer coded variable checking responses
        };
        if (data.key_press == 77 && data.nSteps != 14 && data.node != data.goalnode) { // Up bet
            trBet = Math.max(trBet+1,0);     // Increase bet by one
            data.trBet = trBet;              // Correct current trial bet
            data.decision = 'up';            // Append participant decision to trial data 
            data.choiceVar = 1;                    // Integer coded variable checking responses
        };
        if (data.key_press == 90 && data.nSteps == 14 && data.node != data.goalnode) { // Stop bet down
            trBet = Math.max(trBet,0);             // Do not increase bet by one
            data.trBet = trBet;                    // Correct current trial bet
            next_total_reward = Math.max(total_reward - trBet,0);
            data.totRew = next_total_reward;       // Correct current trial reward
            trRew = -trBet;                        // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'downend';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = false;                          // Prevent carryover misses
            data.choiceVar = 0;                    // Integer coded variable checking responses
        };
        if (data.key_press == 77 && data.nSteps == 14 && data.node != data.goalnode) { // Stop bet UP
            trBet = Math.max(trBet+1,0);           // Increase bet by one
            data.trBet = trBet;                    // Correct current trial bet
            next_total_reward = Math.max(total_reward - trBet,0);   // Increment experiment reward down by betted value (lost)
            data.totRew = next_total_reward;       // Correct current trial reward
            trRew = -trBet;                        // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'upend';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = false;                          // Prevent carryover misses
            data.choiceVar = 1;                    // Integer coded variable checking responses
        };
        if (data.key_press == null && data.nSteps != 14 && data.node != data.goalnode) { //No response given
            mbPunish = mbPunish - 1;                      // Tracks punishments over entire miniblock
            data.mbPunish = mbPunish;                     // Correct current trial punishment
            total_reward = Math.max(total_reward - 1,0);  // Punish total reward
            data.totRew = total_reward;                   // Correct current total reward
            data.trRew = trRew;                           // Correct current trial reward
            miss = true;
            data.decision = 'miss';
            data.choiceVar = -1;                          // Integer coded variable checking responses
        };
        if (data.key_press == null && data.node == data.goalnode) {//No response to goal node
            nWin += 1;                             // Increment nWin
            mbPunish = mbPunish - 1;               // Tracks punishment over entire miniblock
            data.mbPunish = mbPunish;              // Correct current trial punishment
            total_reward = Math.max(total_reward - 1,0);            // Punish total reward
            next_total_reward = Math.max(total_reward + trBet,0);   // Increment experiment reward by betted value
            data.totRew = next_total_reward;       // Correct current trial reward
            trRew = trBet;                         // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'missgoal';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = true;                           // Prevent carryover misses in lastmiss
            data.choiceVar = -1;                   // Integer coded variable checking responses
            jsPsych.endCurrentTimeline();
        };
        if (data.key_press == null && data.nSteps == 14 && data.node != data.goalnode) { //No response - ending
            mbPunish = mbPunish - 1;               // Tracks punishment over entire miniblock
            data.mbPunish = mbPunish;              // Correct current trial punishment
            total_reward = Math.max(total_reward - 1,0);            // Punish total reward
            next_total_reward = Math.max(total_reward - trBet,0);   // Decrement experiment reward by betted value
            data.totRew = next_total_reward;       // Correct current trial reward
            trRew = -trBet;                        // Track miniblock-reward
            data.trRew = trRew;                    // Append to trial data
            mbendf = 'missend';
            data.decision = mbendf;                // Append participant decision to trial data
            trRew = 0;                             // Reset miniblock-reward for next miniblock
            miss = true;                           // Prevent carryover misses in lastmiss
            data.choiceVar = -1;                   // Integer coded variable checking responses
        };
    }
}