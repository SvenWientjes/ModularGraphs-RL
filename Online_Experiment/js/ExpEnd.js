// String feedback for questions
function quendF(telcor, wingcor){
    return '<div id="EndQfb" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;margin-top:auto;">\
                    You are now done with all the questions. Here you can see how many of the question sets you answered correctly: \
                </p>\
                <div style="display:flex; justify-content:space-around; min-width:100vw;margin-bottom:0;margin-top:auto;">\
                    <p style="color:white; font-family:VideoGame; font-size:35px; line-height:1.5; max-width:40vw;margin-top:auto;">\
                        Teleporters <br> '+ telcor +'/3\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:35px; line-height:1.5; max-width:80vw;margin-top:auto;">\
                        Wing entrances <br> '+ wingcor +'/3\
                    </p>\
                </div>\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                    Press &lt;m&gt; to continue.\
                </p>\
            </div>'
}

// String end of the experiment with total earnings!
function expendF(telcor, wingcor, total_reward, point2cash, q2cash){
    let total_earnings = total_reward * point2cash + telcor * q2cash + wingcor * q2cash;
    return '<div id="Finish" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
            display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                    Congratulations! You are finished with the entire experiment now. In total you earned: \
                </p>\
                <p style="color:white; font-family:VideoGame; font-size:35px; line-height:1.5; max-width:80vw;">\
                    \u20AC' + total_earnings + ' \
                </p>\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                    The next screen will contain a link to send you back to Pavlovia. You must click this to finalize and be eligible for payment. \
                </p>\
                <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                    Press &lt;m&gt; to continue.\
                </p>\
                </div>'
}

// String end of the experiment with total earnings!
var expendStr = '<div id="Finish" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                 display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                        Congratulations! You are finished with the entire experiment now. In total you earned: \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:35px; line-height:1.5; max-width:80vw;">\
                        \u20AC35 \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        The next screen will contain a link to send you back to Pavlovia. You must click this to finalize and be eligible for payment. \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                    </div>'

// Function that can search for array in array of arrays
function searchForArray(haystack, needle){
    var i, j, current;
    for(i = 0; i < haystack.length; ++i){
        if(needle.length === haystack[i].length){
        current = haystack[i];
        for(j = 0; j < needle.length && needle[j] === current[j]; ++j);
        if(j === needle.length)
            return 1;
        }
    }
    return 0;
}

// Define array of arrays of tele-transitions we are looking for
var teleDesire = [[0,14], [14,0], [4,5], [5,4], [9,10], [10,9]];
var wingDesire = [0,4,5,9,10,14];

/* ---- JSpsych Trial Variables ---- */
// Message at the end of the questions
var qendMsg = {
    type: 'html-keyboard-response',
    stimulus: function(){
        // Get teleporter answers
        let tele1fb = jsPsych.data.get().last(13).values()[0].teledecision;
        let tele2fb = jsPsych.data.get().last(11).values()[0].teledecision;
        let tele3fb = jsPsych.data.get().last(9).values()[0].teledecision;
        // Compare to correct hallways
        let idxtel1 = searchForArray(teleDesire, tele1fb);
        let idxtel2 = searchForArray(teleDesire, tele2fb);
        let idxtel3 = searchForArray(teleDesire, tele3fb);

        // Get wing entry answers
        let wing1a = jsPsych.data.get().last(6).values()[0].decision;
        let wing1b = jsPsych.data.get().last(5).values()[0].decision;
        let wing2a = jsPsych.data.get().last(4).values()[0].decision;
        let wing2b = jsPsych.data.get().last(3).values()[0].decision;
        let wing3a = jsPsych.data.get().last(2).values()[0].decision;
        let wing3b = jsPsych.data.get().last(1).values()[0].decision;
        //var wingcor = 0;
        if(wingDesire.includes(wing1a) && wingDesire.includes(wing1b)){wingcor += 1;}
        if(wingDesire.includes(wing2a) && wingDesire.includes(wing2b)){wingcor += 1;}
        if(wingDesire.includes(wing3a) && wingDesire.includes(wing3b)){wingcor += 1;}

        //var telcor = idxtel1+idxtel2+idxtel3;
        telcor = idxtel1+idxtel2+idxtel3;
        return quendF(telcor, wingcor);
    },
    choices: ['m'],
}

// Message at the end of the entire experiment
var expendMsg = {
    type: 'html-keyboard-response',
    stimulus: function(){
        return expendF(telcor, wingcor, total_reward, 0.1, 0.2);
    },
    choices: ['m']
}