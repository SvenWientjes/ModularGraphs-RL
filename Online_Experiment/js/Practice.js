// Instructions for first practice block
var prac1instr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Lets first practice a job where you will find the target painting guaranteed. Please respond with &lt;m&gt; in every \
                        display room, so you can see the coins increase. Each display room is presented for a fixed amount of time, \
                        so make sure you respond in time! You should also respond in the final display room with the target painting. \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Remember: You can only affect the price of the target painting. You cannot influence what you see.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'
// Feedback after first practice block
var prac1fb = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    Well done! Hopefully it is clear how to increase the price of your intended sale.\
                </p>\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    Press &lt;m&gt; to continue.\
                </p>\
            </div>'                
// Failed on the first practice block
var prac1fail = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        It seems you did not manage to press &lt;m&gt; in every display room. Lets try again!\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'
// Instructions for second practice block
var prac2instr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Now lets practice a job where you lose. For the sake of time, we will limit this job again to 5 display \
                        rooms. Keep in mind that, during the real experiment, you will get to see up to 15 display rooms! For this short \
                        practice run, please respond with &lt;m&gt; at least once, with &lt;z&gt; at least once, and please forget to respond \
                        at least once, so you get to see what happens when no response is registered.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'
var prac2fail = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        It seems you did not manage to press &lt;m&gt;, press &lt;z&gt;, and omit responding, at least once each. Lets try again!\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'



// Instructions for finishing the practice
var pracfinish = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        It seems like you have the task nailed down! If you with to start the real experiment, press &lt;m&gt. \
                        You can also choose to practice more if you wish. Press the &lt;w&gt key to practice the winning job again. \
                        Press the &lt;l&gt key to practice the losing job again.\
                    </p>\
                    </div>'

// Define variables for practice
var winOrder = [0,1,2,3,4];
var loseOrder = [0,1,2,3,1];
var winPractice = [];
var losePractice = [];
for (var j=0; j<5; j++) { //iterate over trialorder to add stimuli
    winPractice.push({
        stimulus: pracRooms[winOrder[j]],
        trialidx: j,
        mbidx: -1,
        goalidx: 4,
        nodeid: winOrder[j]
    });
    losePractice.push({
        stimulus: pracRooms[loseOrder[j]],
        trialidx: j,
        mbidx: -1,
        goalidx: 4,
        nodeid: loseOrder[j]
    });
}

// Get timeline variable for goal
var pracGoal = {
    timeline: [goalcue],
    timeline_variables: [
        {thegoal: 'img/example-goal.jpg'}
    ]
}
            
/* ---- JSpsych Trial Variables ---- */
// Instructions for first practice block
var pracInstr1 = {
    type: 'html-keyboard-response',
    stimulus: prac1instr,
    choices: ['m']
}
// Notify that first practice block failed
var pracFail1 = {
    type: 'html-keyboard-response',
    stimulus: prac1fail,
    choices: ['m'],
    on_load: function(){
        trBet = 0;
        nWin = 0;
        miss = false;
        condLMidx = 0;
        mbPunish = 0;
    }
}
// Feedback for first practice block
var pracFb1 = {
    type: 'html-keyboard-response',
    stimulus: prac1fb,
    choices: ['m']
}
// Instructions for second practice block
var pracInstr2 = {
    type: 'html-keyboard-response',
    stimulus: prac2instr,
    choices: ['m']
}
// Notify that second practice block failed
var pracFail2 = {
    type: 'html-keyboard-response',
    stimulus: prac2fail,
    choices: ['m'],
    on_load: function(){
        trBet = 0;
        total_reward = 5;
        miss = false;
        condLMidx = 0;
        mbPunish=0;
    }
}
// Finish the practicing blocks!
var finprac = {
    type: 'html-keyboard-response',
    stimulus: pracfinish,
    choices: ['m','l','w'],
    on_finish: function(data){
        if(data.key_press==77){
            total_reward = 0;
            next_total_reward = 0;
            nWin = 0;
        }
    }
}
