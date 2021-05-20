// Initialize canvas for HTML button response - entries
var entryStr = '<div id="ChoiceDiv" style="margin:0 auto;">'+
'<canvas id="ChoiceCv" width="1920" height="1080"</canvas></div>';

// Prompt for entry questions
var entprompStr = '<div id="Entryprompt" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;margin-top:auto;">\
                        For the final set of questions, please consider that the art gallery was subdivided into three "wings", as shown in the figure. \
                        Each circle here, corresponds to one display room. The lines connecting them, are the hallways. You previously had to select the \
                        hallways corresponding to those connecting the wings. Now, we will present you with three sets of five display rooms. These will \
                        correspond to the rooms of one wing. Please select the two rooms you believe are the "entries" to that wing, connecting to another \
                        wing via one hallway. For each wing you will receive a 20 cents bonus if you have both rooms correct.\
                    </p>\
                    <img src="img/Schapiro Graph.png" style="max-width:20vw;max-height:auto; border:0; margin-top:auto;">\
                    <p style="color:white; font-family:VideoGame; font-size:15px; line-height:1.5; max-width:50vw; margin-bottom:auto;">\
                        A layout of the art gallery. Each circle corresponds to one display room. The rooms were grouped into three "wings", indicated by \
                        three different colors.\
                        </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'

// Generate the buttons according to first or second choice - keep idx consistent
function EntryBtnGen(orderedrooms, whichChosen){
    var roombtnStr = [];
    var xpix = 2;
    if(whichChosen < 0){
        for(var i=0; i<5; i++){
            roombtnStr.push(
                '<button class="image-btn-room" style = "position:absolute; left:'+(xpix+i*384)+'px; top:433.125px;"><img src="'+rooms[orderedrooms[i]]+'" width="380" height="380*1080/1920" /></button>'
            );
        }
    }
    if(whichChosen >= 0){
        for(var i=0; i<5; i++){
            if(i == whichChosen){
                // nothing?
            }else if(i != whichChosen){
                roombtnStr.push(
                    '<button class="image-btn-room" style = "position:absolute; left:'+(xpix+i*384)+'px; top:433.125px;"><img src="'+rooms[orderedrooms[i]]+'" width="380" height="380*1080/1920" /></button>'
                );            
            }
        }
    }
    return(roombtnStr)
}

// Draw the canvas as background for the buttons
function EntryC(whichroom, whichi){
    var canvas = document.getElementById('ChoiceCv');
    ctx = canvas.getContext("2d");

    // Display text on top
    ctx.font = '20px VideoGame';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';
    ctx.fillText('Select the two rooms you believe to be entrances to this wing.', canvas.width/2, 150);

    // Draw extra stimulus for second choice
    if(whichi >= 0){
        var roomdraw = new Image();
        roomdraw.src = whichroom;
        ctx.globalAlpha = 0.5;
        ctx.drawImage(roomdraw,(2+whichi*384),433.125,380,380*(1080/1920));
    }
}

// Create timeline variable for indexing entry questions
var entryIdx = [];
for (var j=0; j<3; j++) { //iterate over trialorder to add stimuli
    entryIdx.push({
        eidx: j
    });
}

/* ---- JSpsych Trial Variables ---- */
var entprompMessage = {
    type: 'html-keyboard-response',
    stimulus: entprompStr,
    choices: ['m']
}
// Set up the first choice screen
var entryChoice1 = {
    type: 'html-button-response',
    stimulus: '<canvas id="ChoiceCv" width="1920" height="1080" style="background-color:black"</canvas>',
    choices: ['1','2','3','4','5'],
    button_html: function(){
        return(EntryBtnGen(entryQlist[jsPsych.timelineVariable('eidx',true)], -1));
    },
    on_load: function(){
        EntryC('bub',-1);
    },
    on_finish: function(data){
        data.decision = entryQlist[jsPsych.timelineVariable('eidx',true)][data.button_pressed]
        data.options = entryQlist[jsPsych.timelineVariable('eidx',true)];
    }
}
// Set up the second choice screen
var entryChoice2 = {
    type: 'html-button-response',
    stimulus: '<canvas id="ChoiceCv" width="1920" height="1080" style="background-color:black"</canvas>',
    choices: ['1','2','3','4'],
    button_html: function(){
        waspressed = jsPsych.data.get().last(1).values()[0].button_pressed;
        return(EntryBtnGen(entryQlist[jsPsych.timelineVariable('eidx',true)], waspressed));
    },
    on_load: function(){
        waspressed = jsPsych.data.get().last(1).values()[0].button_pressed;
        orderedrooms = entryQlist[jsPsych.timelineVariable('eidx',true)];
        EntryC(rooms[orderedrooms[waspressed]], waspressed);
    },
    on_finish: function(data){
        if(data.button_pressed >= waspressed){
            data.button_pressed += 1;
        }
        data.decision = entryQlist[jsPsych.timelineVariable('eidx',true)][data.button_pressed]
        data.options = entryQlist[jsPsych.timelineVariable('eidx',true)];
    }
}

