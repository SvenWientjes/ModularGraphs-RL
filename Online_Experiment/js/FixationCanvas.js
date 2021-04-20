function FixationC(miss){
    var canvas = document.getElementById('FixationCv');
    ctx = canvas.getContext("2d");

    // Text
    ctx.font = '70px Arial';
    if(miss){
        ctx.fillStyle = 'red';
    }else{
        ctx.fillStyle = 'black';
    }
    ctx.textAlign = 'center';
    ctx.fillText('+', canvas.width/2, canvas.height/2);
}

var FixStr = '<div id="FixationDiv" style="margin:0 auto;">'+
'<canvas id="FixationCv" width="1920" height="1080" style="background:url(img/Hallway.png)"></canvas></div>';