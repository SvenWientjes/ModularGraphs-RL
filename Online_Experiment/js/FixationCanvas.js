function FixationC(miss, trBet){
    var canvas = document.getElementById('FixationCv');
    ctx = canvas.getContext("2d");

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