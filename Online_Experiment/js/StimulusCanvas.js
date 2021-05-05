function StimC(trBet, total_reward){
    var canvas = document.getElementById('StimulusCv');
    cst = canvas.getContext("2d");

    // Points on top
    cst.strokeStyle = 'black';
    cst.fillStyle = 'black'
    //ctx.strokeRect(860,0,200,70)
    cst.beginPath();
    cst.moveTo(860,0);
    cst.lineTo(860,70);
    cst.lineTo(1060,70);
    cst.lineTo(1060,0);
    //cst.stroke();
    cst.font = '40px VideoGame';
    cst.textAlign = 'center';
    cst.fillStyle = 'black';
    cst.fillText(total_reward, canvas.width/2, 50);

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

    // Listener for adding coins right away
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
       window.removeEventListener('keydown', BetLive);
       window.removeEventListener('keyup', UpCheck);
    }
}