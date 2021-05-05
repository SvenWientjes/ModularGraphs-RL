//var welcStr = '<div id="WelcomeDiv" style="margin:0 auto;" style="font-family: VideoGame;">'+
//'<canvas id="WelcomeCv" width="1920" height="1080"</canvas></div>';

var welcStr = '<p>Welcome to the experiment. Press any key to continue.</p><p style="font-family:VideoGame;font-size:10%;">.</p>'
var qintStr = '<p>Before the start of the experiment, we would like to ask you some questions.</p>'+
              '<p>We process all personal data in a secure way, but if you do not feel comfortable</p>'+
              '<p>answering some of these questions, feel free to leave them blank</p>'
var fsintStr = '<p>For the experiment it is important that we set your browser to fullscreen mode.</p>'+
               '<p>The next page will contain a button you can press to enter fullscreen.</p>' +
               '<p>If you leave fullscreen during the experiment at any time, this will be recorded and</p>'+
               '<p>unfortunately we will not be able to complete your payment if this has occurred.</p>'+
               '<p>Additionally, be aware that the experiment only works for 16:9 aspect ratio monitors.</p>'+
               '<p>If you do not have a monitor with this aspect ratio, please quit the experiment, as we will</p>'+
               '<p>ultimately not be able to complete your payment.</p>'+
               '<p>Press any key to continue.</p>'

function welcS(){
    var canvas = document.getElementById('WelcomeCv');
    ctx = canvas.getContext("2d");

    // Make background black
    ctx.beginPath();
    ctx.rect(0, 0, 1920, 1080);
    ctx.fillStyle = " black ";
    ctx.fill();

    // Display text in center
    ctx.font = '30px VideoGame';
    ctx.textAlign = 'center';
    ctx.fillStyle = 'white';
    ctx.fillText('Welcome to the experiment. Press any key to begin.', canvas.width/2, canvas.height/2);
}