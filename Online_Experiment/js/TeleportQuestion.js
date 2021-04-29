// Initialize canvas for HTML button response - teleportations
var teleStr = '<div id="ChoiceDiv" style="margin:0 auto;">'+
'<canvas id="ChoiceCv" width="1920" height="1080"</canvas></div>';

var btnStr = ['<button class="image-btn-sven" style = "position:absolute; left:144px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:528px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:912px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:1296px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>',
              '<button class="image-btn-sven" style = "position:absolute; left:1680px; top: 490px"><img src="img/teleport.svg" width="95" height="95" /></button>'];

// Draw all the stimuli for the teleportation question
function TeleC(telIdx) {
    var canvas = document.getElementById('ChoiceCv');
    ctx = canvas.getContext("2d");

    // Make background black
    ctx.beginPath();
    ctx.rect(0, 0, 1920, 1080);
    ctx.fillStyle = " #17202a ";
    ctx.fill();

    ctx.strokeStyle = "#06b72e"; // Arrow color
    var imgs1 = []; // Array for loading top images
    var imgs2 = [];// Array for loading bottom images
    // Hallway image (in between pairs of rooms)
    hallImg = new Image();
    hallImg.src = 'img/Hallway.png'
    // Loop over creating room and hallway images
    for(var i=0; i<5; i++){
        imgs1[i] = new Image(); //New image element 
        imgs1[i].src = rooms[telelist[ppn][telIdx][i][0]];
        imgs2[i] = new Image();
        imgs2[i].src = rooms[telelist[ppn][telIdx][i][1]];
        if(i==4){
            imgs1[4].onload = function(){
                for(var j=0;j<5;j++){
                    // Draw the rooms/hallway
                    ctx.drawImage(imgs1[j],(2+j*384),220,380,380*(1080/1920));
                    ctx.drawImage(hallImg, (2+j*384),220+(380*(1080/1920)), 380,380*(1080/1920));
                    ctx.drawImage(imgs2[j],(2+j*384),220+(2*380*(1080/1920)), 380, 380*(1080/1920));
                    // Draw arrows between them
                    ctx.lineWidth = 3;
                    ctx.beginPath();
                    canvas_arrow(ctx, (192+j*384), 220+380*(1080/1920)+30, (192+j*384), 220+380*(1080/1920)-30);
                    canvas_arrow(ctx, (192+j*384), 220+380*(1080/1920)-30, (192+j*384), 220+380*(1080/1920)+30);
                    canvas_arrow(ctx, (192+j*384), 220+2*380*(1080/1920)+30, (192+j*384), 220+2*380*(1080/1920)-30);
                    canvas_arrow(ctx, (192+j*384), 220+2*380*(1080/1920)-30, (192+j*384), 220+2*380*(1080/1920)+30);
                    ctx.stroke();
                }
                // Draw lines in between pair-choices //
                    // Top line   
                //ctx.lineWidth = 5;
                ctx.strokeStyle = " #641e16 "; // Separation line color
                //ctx.beginPath();
                //ctx.moveTo(0, 217.5);
                //ctx.lineTo(1920, 215);
                //ctx.stroke(); 
                    // Bottom line
                //ctx.beginPath();
                //ctx.moveTo(0, 861.25);
                //ctx.lineTo(1920, 861.25);
                //ctx.stroke(); 
                    // Left line
                //ctx.lineWidth = 2;
                //ctx.beginPath();
                //ctx.moveTo(1, 220);
                //ctx.lineTo(1, 861.25);
                //ctx.stroke(); 
                    // Right line
                //ctx.beginPath();
                //ctx.moveTo(1919, 216);
                //ctx.lineTo(1919, 861.25);
                //ctx.stroke(); 
                // Lines in between pairs
                ctx.lineWidth = 4;
                for(var j=1;j<5;j++){
                    ctx.beginPath();
                    ctx.moveTo(384*j, 220);
                    ctx.lineTo(384*j, 861.25);
                    ctx.stroke(); 
                }
            }
        }
    }
    // Display text on top
    ctx.font = '30px Arial';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';
    ctx.fillText('Use the mouse to indicate in which hallway you would like to place the teleporter.', canvas.width/2, 150);
}

// Simple function for drawing arrows
function canvas_arrow(context, fromx, fromy, tox, toy) {
    var headlen = 10; // length of head in pixels
    var dx = tox - fromx;
    var dy = toy - fromy;
    var angle = Math.atan2(dy, dx);
    context.moveTo(fromx, fromy);
    context.lineTo(tox, toy);
    context.moveTo(tox, toy)
    context.lineTo(tox - headlen * Math.cos(angle - Math.PI / 6), toy - headlen * Math.sin(angle - Math.PI / 6));
    context.moveTo(tox, toy);
    context.lineTo(tox - headlen * Math.cos(angle + Math.PI / 6), toy - headlen * Math.sin(angle + Math.PI / 6));
  }
