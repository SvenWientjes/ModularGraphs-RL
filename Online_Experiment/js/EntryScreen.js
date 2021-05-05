// Initialize canvas for HTML button response - entries
var entryStr = '<div id="ChoiceDiv" style="margin:0 auto;">'+
'<canvas id="ChoiceCv" width="1920" height="1080"</canvas></div>';

function EntryBtnGen(orderedrooms, whichChosen){
    var roombtnStr = [];
    var xpix = 2;
    if(whichChosen < 0){
        for(var i=0; i<5; i++){
            roombtnStr.push(
                '<button class="image-btn-room" style = "position:absolute; left:'+(xpix+i*384)+'px; top:433.125px;"><img src="'+rooms[orderedrooms[i]]+'" width="380" height="380*1080/1920" /></button>'
            );
        }
    }
    if(whichChosen >= 0){
        for(var i=0; i<5; i++){
            if(i == whichChosen){
                // nothing?
            }else if(i != whichChosen){
                roombtnStr.push(
                    '<button class="image-btn-room" style = "position:absolute; left:'+(xpix+i*384)+'px; top:433.125px;"><img src="'+rooms[orderedrooms[i]]+'" width="380" height="380*1080/1920" /></button>'
                );            
            }
        }
    }
    return(roombtnStr)
}

function EntryC(whichroom, whichi){
    var canvas = document.getElementById('ChoiceCv');
    ctx = canvas.getContext("2d");

    // Display text on top
    ctx.font = '20px VideoGame';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';
    ctx.fillText('Select the two rooms you believe to be entrances to this wing.', canvas.width/2, 150);

    // Draw extra stimulus for second choice
    if(whichi >= 0){
        var roomdraw = new Image();
        roomdraw.src = whichroom;
        ctx.globalAlpha = 0.5;
        ctx.drawImage(roomdraw,(2+whichi*384),433.125,380,380*(1080/1920));
    }
}