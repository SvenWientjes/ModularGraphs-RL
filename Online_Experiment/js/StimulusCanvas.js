function StimC(trBet){
    var canvas = document.getElementById('StimulusCv');
    cst = canvas.getContext("2d");

    // Draw circles
    
    cst.fillStyle = 'rgba(221, 204, 8, 1)'
    cst.strokeStyle = 'black'
    circStart = 580;
    for(i=0; i<trBet; i++){
        cst.beginPath();
        cst.arc(circStart,1060,20,0,2*Math.PI);
        cst.fill();
        cst.stroke();
        circStart += 40;
    }
    window.addEventListener('keydown', UpCheck);
}

function UpCheck(e){
    window.addEventListener('keyup', BetLive);
}

function BetLive(e){
    //react to a buttonpress
    var keyCode = e.key;
    if(keyCode == 'm' || keyCode == 'M'){
        cst.beginPath();
        cst.arc(circStart,1060,20,0,2*Math.PI);
        cst.fill();
        cst.stroke();
        window.removeEventListener('keydown', UpCheck);
        window.removeEventListener('keyup', BetLive);
    }else if(keyCode == 'z' || keyCode == 'Z'){
        //cst.clearRect(circStart-60,1040,40,40);
       window.removeEventListener('keydown', UpCheck);
        window.removeEventListener('keyup', BetLive);
    }
}