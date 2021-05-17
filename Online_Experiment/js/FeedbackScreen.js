var FeedbStr = '<div id="FeedbackDiv" style="margin:0 auto;">'+
    '<canvas id="FeedbackCv" width="1920" height="1080"</canvas></div>';
    
    function FeedbackS(nGoals, trRewf, trLeft, mbPunishf){
        var canvas = document.getElementById('FeedbackCv');
        ctx = canvas.getContext("2d");
    
        // Make background black
        ctx.beginPath();
        ctx.rect(0, 0, 1920, 1080);
        ctx.fillStyle = " black ";
        ctx.fill();

        // Points on top
        ctx.strokeStyle = 'white';
        //ctx.strokeRect(860,0,200,70)
        ctx.beginPath();
        ctx.moveTo(860,0);
        ctx.lineTo(860,70);
        ctx.lineTo(1060,70);
        ctx.lineTo(1060,0);
        ctx.stroke();
        ctx.font = '40px VideoGame';
        ctx.textAlign = 'center';
        ctx.fillStyle = 'white';
        ctx.fillText(total_reward, canvas.width/2, 50);
    
        // Makeup for text
        ctx.textAlign = 'center';
        ctx.fillStyle = 'white';

        // Title of feedback screen
        ctx.font = '50px VideoGame';
        ctx.fillText('The Office',canvas.width/2,270);

        // Titles of different feedbacks
        ctx.font = '30px VideoGame';
        ctx.fillText('Sales', 480,500);
        ctx.fillText('Cashflow',960,500);
        ctx.fillText('Jobs Left',1440,500);
        if(mbPunishf < 0){
            ctx.fillText('Omissions',720,850);
            ctx.font = '30px VideoGame';
            ctx.textAlign = 'left';
            ctx.fillText('Please remember to respond with',920,875);
            ctx.fillText('<z> or <m> in every room!',920,915)
        }

        // Point values of the feedbacks
        ctx.font = '50px VideoGame';
        ctx.textAlign = 'center'
        ctx.fillText(nGoals, 480,600);
        ctx.fillText(trRewf,960,600);
        ctx.fillText(99-trLeft,1440,600);
        if(mbPunishf < 0){
            ctx.fillText(-mbPunishf, 720, 950)
        }
    }

/* ---- JSpsych Trial Variables ---- */
// The Office - full feedback after every job
var pointfb = {
    type: 'html-keyboard-response',
    stimulus: FeedbStr,
    on_load: function(){
        total_reward = next_total_reward;
        next_total_reward = 0;
        trCheckIdx = 2+condLMidx;
        //var nGoals = jsPsych.data.get().filter([{decision: 'upgoal'}, {decision: 'downgoal'}, {decision: 'missgoal'}]).count();
        var trRewf = jsPsych.data.get().last(trCheckIdx).values()[0].trRew;
        var trLeft = jsPsych.data.get().last(trCheckIdx).values()[0].miniblock;
        var mbPunishf = jsPsych.data.get().last(trCheckIdx).values()[0].mbPunish;
        FeedbackS(nWin, trRewf, trLeft, mbPunishf);
        mbPunish = 0;  //Reset mbPunish for next trial
        condLMidx = 0; //Reset checker if last trial was missed
    }
}