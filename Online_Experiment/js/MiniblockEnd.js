var mbendStr = '<div id="mbendDiv" style="margin:0 auto;">'+
'<canvas id="mbendCv" width="1920" height="1080"</canvas></div>';

function mbendS(mbend, total_reward, trBetf){
    var canvas = document.getElementById('mbendCv');
    ctx = canvas.getContext("2d");

    // Make background black
    ctx.beginPath();
    ctx.rect(0, 0, 1920, 1080);
    ctx.fillStyle = " black ";
    ctx.fill();

    // Draw circles
    ctx.fillStyle = 'rgba(221, 204, 8, 1)'
    ctx.strokeStyle = 'black'
    circStart = 580;
    for(i=0; i<trBetf; i++){
        ctx.beginPath();
        ctx.arc(circStart,1060,20,0,2*Math.PI);
        ctx.fill();
        ctx.stroke();
        circStart += 40;
    }

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

    // Decide text (goal v nogoal)
    if(mbend=='downgoal'||mbend=='upgoal'||mbend=='missgoal'){
        var txt = 'Congratulations! You have found the painting.\n' +
                  'You will be awarded the corresponding \npoints in the office.'
    }else if(mbend=='downend'||mbend=='upend'||mbend=='missend'){
        var txt = 'You did not find the painting in time. \nYou will be fined in the office.\n' +
                  'Better luck next time!'
    }

    // Makeup for text
    ctx.font = '30px VideoGame';
    var lineheight = 40;
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';

    var x = canvas.width/2;
    //var y = canvas.height/2;
    var y = 500;
    var lines = txt.split('\n');

    for(var i=0; i<lines.length; i++){
        ctx.fillText(lines[i], x, y+i*lineheight);
    }
}

/* ---- JSpsych Trial Variables ---- */
// Message at the end of a miniblock
var blockend = {
    type: 'html-keyboard-response',
    stimulus: mbendStr,
    on_load: function(){
        trBetf = trBet;
        mbendS(mbendf, total_reward, trBetf);
        trBet=0;
    }
}