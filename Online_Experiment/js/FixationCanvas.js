function FixationC(miss){
    var canvas = document.getElementById('FixationCv');
    //canvas.width = window.innerWidth;
    //canvas.height = window.innerHeight;
    canvas.width = document.body.clientWidth;
    canvas.height = document.body.clientHeight;
    canvasW = canvas.width;
    canvasH = canvas.height;
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

    var img = new Image();
    img.src="img/Hallway.png"
    img.onload = function(e){
        ctx.drawImage(img,0,0,canvas.width, canvas.height)
        ctx.fillText('+', canvas.width/2, canvas.height/2);
    }
}

/*function testFix(c){
    c.width = document.body.clientWidth;
    c.height = document.body.clientHeight;
    var ctx=c.getContext('2d');
    ctx.font = '70px Arial';
    ctx.fillStyle = 'black';
    ctx.textAlign = 'center';
    ctx.fillText('+', c.width/2, c.height/2);

    var img = new Image();
    img.src="img/Hallway.png"
    img.onload = function(e){
        ctx.drawImage(img,0,0,c.width, c.height)
        ctx.fillText('+', c.width/2, c.height/2);
    }
}

function testDraw(){

}*/