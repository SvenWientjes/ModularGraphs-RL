function FixationC(miss, trBet){
    var canvas = document.getElementById('FixationCv');
    ctx = canvas.getContext("2d");

    // Points on top
    ctx.fillStyle = 'black'
    ctx.strokeStyle = 'black';
    //ctx.strokeRect(860,0,200,70)
    ctx.beginPath();
    ctx.moveTo(860,0);
    ctx.lineTo(860,70);
    ctx.lineTo(1060,70);
    ctx.lineTo(1060,0);
    //ctx.stroke();
    ctx.font = '40px VideoGame';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'black';
    ctx.fillText(total_reward, canvas.width/2, 50);

    // Text
    ctx.font = '70px Arial';
    ctx.textAlign = 'center';
    if(miss){
        ctx.fillStyle = 'red';
        ctx.fillText('- \u20AC', canvas.width/2, canvas.height/2);
    }else{
        ctx.fillStyle = 'black';
        ctx.fillText('+', canvas.width/2, canvas.height/2);
    }
    
    

    ctx.fillStyle = 'rgba(221, 204, 8, 1)'
    ctx.strokeStyle = 'black'
    circStart = 580;
    for(i=0; i<trBet; i++){
        ctx.beginPath();
        ctx.arc(circStart,1060,20,0,2*Math.PI);
        ctx.fill();
        ctx.stroke();
        circStart += 40;
    }
}

var FixStr = '<div id="FixationDiv" style="margin:0 auto;">'+
'<canvas id="FixationCv" width="1920" height="1080" style="background:url(img/Hallway.png)"></canvas></div>';