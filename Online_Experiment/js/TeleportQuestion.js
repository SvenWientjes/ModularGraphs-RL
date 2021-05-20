// Initialize canvas for HTML button response - teleportations
var teleStr = '<div id="ChoiceDiv" style="margin:0 auto;">'+
'<canvas id="ChoiceCv" width="1920" height="1080"</canvas></div>';

// Finished with the main experiment
var finishStr = '<div id="Finish" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Congratulations! You are finished with the main part of this experiment. Your points from this section are fixed and \
                        will be paid out to you guaranteed. \
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Now, you will proceed to a shorter section where you answer a few questions. You will receive an additional payment \
                        based on your answers, so pay attention! You will receive feedback after all the questions have been answered.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'

// Prompt for tele questions
var telprompStr = '<div id="Finish" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;margin-top:auto;">\
                        Please now, imagine that you could have some more control over where you would move in the art gallery you have spent \
                        so much time in. Specifically, you have the ability to place three "teleporters" that you could teleport to with the press \
                        of a button, whenever you would like. These teleporters would be placed in hallways between two display rooms. From this teleporter, \
                        you would again move randomly. To maximize your chance of reaching any target painting, where would you place the teleporters? You will \
                        receive 20 cents for each optimal choice.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;margin-bottom:auto;">\
                        We will present you with three sets of five hallways. For each set, you can pick one out of the five hallways to place this teleporter in. \
                        You can select the hallway by clicking on the teleporter icon placed in that hallway, which will look like this:\
                    </p>\
                    <img src="img/teleport.png" style="max-width:10vw;max-height:auto; border:0;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'

var btnStr = ['<button class="image-btn-sven" style = "position:absolute; left:144px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:528px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:912px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:1296px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:1680px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>'];

// Draw all the stimuli for the teleportation question
function TeleC(telIdx) {
    var canvas = document.getElementById('ChoiceCv');
    ctx = canvas.getContext("2d");

    // Make background black
    ctx.beginPath();
    ctx.rect(0, 0, 1920, 1080);
    //ctx.fillStyle = " #17202a ";
    ctx.fillStyle = 'black'
    ctx.fill();

    ctx.strokeStyle = "#06b72e"; // Arrow color
    var imgs1 = []; // Array for loading top images
    var imgs2 = [];// Array for loading bottom images
    // Hallway image (in between pairs of rooms)
    hallImg = new Image();
    hallImg.src = 'img/Hallway.png'
    // Loop over creating room and hallway images
    for(var i=0; i<5; i++){
        imgs1[i] = new Image(); //New image element 
        imgs1[i].src = rooms[telelist[ppn][telIdx][i][0]];
        imgs2[i] = new Image();
        imgs2[i].src = rooms[telelist[ppn][telIdx][i][1]];
        if(i==4){
            imgs1[4].onload = function(){
                for(var j=0;j<5;j++){
                    // Draw the rooms/hallway
                    ctx.drawImage(imgs1[j],(2+j*384),220,380,380*(1080/1920));
                    ctx.drawImage(hallImg, (2+j*384),220+(380*(1080/1920)), 380,380*(1080/1920));
                    ctx.drawImage(imgs2[j],(2+j*384),220+(2*380*(1080/1920)), 380, 380*(1080/1920));
                    // Draw arrows between them
                    ctx.lineWidth = 3;
                    ctx.beginPath();
                    canvas_arrow(ctx, (192+j*384), 220+380*(1080/1920)+30, (192+j*384), 220+380*(1080/1920)-30);
                    canvas_arrow(ctx, (192+j*384), 220+380*(1080/1920)-30, (192+j*384), 220+380*(1080/1920)+30);
                    canvas_arrow(ctx, (192+j*384), 220+2*380*(1080/1920)+30, (192+j*384), 220+2*380*(1080/1920)-30);
                    canvas_arrow(ctx, (192+j*384), 220+2*380*(1080/1920)-30, (192+j*384), 220+2*380*(1080/1920)+30);
                    ctx.stroke();
                }
                // Draw lines in between pair-choices //
                    // Top line   
                //ctx.lineWidth = 5;
                ctx.strokeStyle = " #641e16 "; // Separation line color
                //ctx.beginPath();
                //ctx.moveTo(0, 217.5);
                //ctx.lineTo(1920, 215);
                //ctx.stroke(); 
                    // Bottom line
                //ctx.beginPath();
                //ctx.moveTo(0, 861.25);
                //ctx.lineTo(1920, 861.25);
                //ctx.stroke(); 
                    // Left line
                //ctx.lineWidth = 2;
                //ctx.beginPath();
                //ctx.moveTo(1, 220);
                //ctx.lineTo(1, 861.25);
                //ctx.stroke(); 
                    // Right line
                //ctx.beginPath();
                //ctx.moveTo(1919, 216);
                //ctx.lineTo(1919, 861.25);
                //ctx.stroke(); 
                // Lines in between pairs
                ctx.lineWidth = 4;
                for(var j=1;j<5;j++){
                    ctx.beginPath();
                    ctx.moveTo(384*j, 220);
                    ctx.lineTo(384*j, 861.25);
                    ctx.stroke(); 
                }
            }
        }
    }
    // Display text on top
    ctx.font = '20px VideoGame';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';
    ctx.fillText('Use the mouse to indicate in which hallway you would like to place the teleporter.', canvas.width/2, 150);
}

// Simple function for drawing arrows
function canvas_arrow(context, fromx, fromy, tox, toy) {
    var headlen = 10; // length of head in pixels
    var dx = tox - fromx;
    var dy = toy - fromy;
    var angle = Math.atan2(dy, dx);
    context.moveTo(fromx, fromy);
    context.lineTo(tox, toy);
    context.moveTo(tox, toy)
    context.lineTo(tox - headlen * Math.cos(angle - Math.PI / 6), toy - headlen * Math.sin(angle - Math.PI / 6));
    context.moveTo(tox, toy);
    context.lineTo(tox - headlen * Math.cos(angle + Math.PI / 6), toy - headlen * Math.sin(angle + Math.PI / 6));
  }

// Create timeline variable for indexing tele questions
var teleIdx = [];
for (var j=0; j<3; j++) { //iterate over trialorder to add stimuli
    teleIdx.push({
        tidx: j
    });
}

/* ---- JSpsych Trial Variables ---- */
var finishMessage = {
    type: 'html-keyboard-response',
    stimulus: finishStr,
    choices: ['m']
}

// Prompt for tele questions
var telprompMessage = {
    type: 'html-keyboard-response',
    stimulus: telprompStr,
    choices: ['m']
}

// Screen for tele questions
var teleChoice = {
    type: 'html-button-response',
    stimulus: teleStr,
    choices: ['1','2','3','4','5'],
    button_html: btnStr,
    on_load: function(){
        TeleC(jsPsych.timelineVariable('tidx',true));
    },
    on_finish: function(data){
        data.teledecision = telelist[ppn][jsPsych.timelineVariable('tidx',true)][data.button_pressed];
        data.options = telelist[ppn][jsPsych.timelineVariable('tidx',true)];
    }
}

// Interval between tele questions 
var teleWait = {
    type: 'html-keyboard-response',
    stimulus: '<canvas id="WaitCv" width="1920" height="1080" style="background-color:black"</canvas>',
    choices: jsPsych.NO_KEYS,
    trial_duration: 1000
}