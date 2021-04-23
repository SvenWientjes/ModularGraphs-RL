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
    window.addEventListener('keydown', BetLive);
}

function UpCheck(e){
    newcoin = true;
    window.removeEventListener('keyup', Upcheck);
}

function BetLive(e){
    //react to a buttonpress
    var keyCode = e.key;
    if(newcoin == true && (keyCode == 'm' || keyCode == 'M')){
        cst.beginPath();
        cst.arc(circStart,1060,20,0,2*Math.PI);
        cst.fill();
        cst.stroke();
        // Upstroke checker
        newcoin = false;
        window.addEventListener('keyup', UpCheck);
        window.removeEventListener('keydown', BetLive);
    }else if(newcoin == true && (keyCode == 'z' || keyCode == 'Z')){
        //cst.clearRect(circStart-60,1040,40,40);
       window.removeEventListener('keydown', UpCheck);
       window.removeEventListener('keyup', BetLive);
    }
}