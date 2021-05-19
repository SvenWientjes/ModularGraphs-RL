// Instructions for first practice block
var prac1instr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Lets first practice a job where you will find the target painting guaranteed. Please respond with &lt;m&gt; in every \
                        display room, so you can see the coins increase. Each display room is presented for a fixed amount of time, \
                        so make sure you respond in time! You should also respond in the final display room with the target painting. \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Remember: You can only affect the price of the target painting. You cannot influence which display rooms you see.\
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
                        practice run, please respond with &lt;m&gt; exactly once, and with &lt;z&gt; for the other rooms. This way you can \
                        see how a point is deducted when you increase the price, but do not find the target painting.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'
// Feedback after second practice block
var prac2fb = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    Well done! You should have seen the points on top decrease from 5 to 4. You can validate this \
                    in the next and final practice block.\
                </p>\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    Press &lt;m&gt; to continue.\
                </p>\
            </div>'                        
// Failed on the second practice block
var prac2fail = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        It seems you did not manage to press &lt;m&gt; once, and press &lt;z&gt;, for the other rooms. Lets try again!\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'                


// Instructions for third practice block
var prac3instr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        For the final practice block, we would like to show you what happens when you forget to press either &lt;z&gt; or \
                        &lt;m&gt;, or are too late. You will again lose this practice block. Please refrain from responding on one out of \
                        the five display rooms you will see. Respond with &lt;z&gt; on all other display rooms. You should notice the feedback \
                        directly in the hallway afterwards, and the points on top will decrease immediately.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'
// Failed on the third practice block
var prac3fail = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        It seems you did not manage to omit responding once, and press &lt;z&gt; on all other display rooms. Lets try again!\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'
// Feedback after second practice block
var prac3fb = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    Well done! You should have noticed that in the office, you are again reminded that you missed \
                    one or more responses. This message only appears when you have omitted a response during the \
                    preceding job.\
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
                        Press the &lt;l&gt key to practice the losing job again. Once you start with the real experiment, your points \
                        will be reset to zero.\
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
// Padding of variables for shorter fail practices - 4 points
var padFail = {
    type: 'html-keyboard-response',
    stimulus: '<p>',
    trial_duration: 1,
    data: {
        trRew: function(){return -trBet;},
        mbendf: function(){return 'downend';},
        miniblock: -1,
        mbPunish: function(){return jsPsych.data.get().last(1).values()[0].mbPunish}
    },
    on_load: function(data){
        mbendf = 'downend'
        next_total_reward = total_reward - trBet;
    }
}

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
// Feedback for second practice block
var pracFb2 = {
    type: 'html-keyboard-response',
    stimulus: prac2fb,
    choices: ['m']
}

// Instructions for third practice block
var pracInstr3 = {
    type: 'html-keyboard-response',
    stimulus: prac3instr,
    choices: ['m']
}
// Notify that third practice block failed
var pracFail3 = {
    type: 'html-keyboard-response',
    stimulus: prac3fail,
    choices: ['m'],
    on_load: function(){
        trBet = 0;
        total_reward = 4;
        miss = false;
        condLMidx = 0;
        mbPunish=0;
    }
}
// Feedback for third practice block
var pracFb3 = {
    type: 'html-keyboard-response',
    stimulus: prac3fb,
    choices: ['m']
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
