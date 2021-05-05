var inStr = '<div id="InstructDiv" style="margin:0 auto;" style="font-family: VideoGame;">'+
'<canvas id="InstructCv" width="1920" height="1080"</canvas></div>';

function instrS(){
    var canvas = document.getElementById('InstructCv');
    ctx = canvas.getContext("2d");

    // Make background black
    ctx.beginPath();
    ctx.rect(0, 0, 1920, 1080);
    ctx.fillStyle = " #black ";
    ctx.fill();

    // Makeup for text
    ctx.font = '30px VideoGame';
    var lineheight = 40;
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';

    // Actual text
    var txt = 'This screen will contain the instructions for the task. \n'+ 
              'We might display multiple instruction screens to properly \n'+
              ' display the multiple types of screens\n'+
              '(goal, start, feedback, etc.) the participant can encounter.\n'+
              'Press any key to continue.';

    var x = canvas.width/2;
    //var y = canvas.height/2;
    var y = 440;
    var lines = txt.split('\n');

    for(var i=0; i<lines.length; i++){
        ctx.fillText(lines[i], x, y+i*lineheight);
    }
}


var allInstr = ['<canvas id="InstructCv" width="1920" height="1080" style="background-color:blue"</canvas>'];