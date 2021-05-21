var welcStr = '<p >Welcome to the experiment. Press any key to continue.</p>\
                <p style="font-family:VideoGame;font-size:10%;">.</p>'
var qintStr = '<p style="width:50%; margin-left:auto;margin-right:auto">Before the start of the experiment, we would like to ask you some questions. '+
              'We process all personal data in a secure way, but if you do not feel comfortable '+
              'answering some of these questions, feel free to leave them blank.</p>'
var fsintStr = '<p style="width:50%; margin-left:auto;margin-right:auto">For the experiment it is important that we set your browser to fullscreen mode.'+
               ' The next page will contain a button you can press to enter fullscreen.' +
               ' If you leave fullscreen during the experiment at any time, this will be recorded and'+
               ' unfortunately we will not be able to complete your payment if this has occurred.'+
               ' Additionally, be aware that the experiment only works for 16:9 aspect ratio monitors.'+
               ' If you do not have a monitor with this aspect ratio, please quit the experiment, as we will'+
               ' ultimately not be able to complete your payment.</p>'+
               '<p>Press any key to continue.</p>'

/* ---- JSpsych Trial Variables ---- */
// Define welcome screen
var welcome = {
    type: "html-keyboard-response",
    stimulus: welcStr
};

var qintroPreload = {
    type: "html-keyboard-response",
    stimulus: '<div style="position:absolute; top:99%; left:99%;">\
                <img src="img/example-goal.jpg" style="z-index:1;position: absolute; width:auto; height:auto;">\
                <img src="img/Iron-prac.png" style="z-index:2; position: absolute; width:auto; height:auto;">\
                <img src="img/coinIllustrate.png" style="z-index:3; position: absolute; width:auto; height:auto;">\
                <img src="img/Calculator-prac.png" style="z-index:4; position: absolute; width:auto; height:auto;">\
                <img src="img/Lantern-prac.png" style="z-index:5; position: absolute; width:auto; height:auto;">\
                <img src="img/Pot-prac.png" style="z-index:6; position: absolute; width:auto; height:auto;">\
                <img src="img/3coin-illustrate.png" style="z-index:7; position: absolute; width:auto; height:auto;">\
                <img src="img/4coin-illustrate.png" style="z-index:8; position: absolute; width:auto; height:auto;">\
                <img src="img/fixHallway.png" style="z-index:9; position: absolute; width:auto; height:auto;">\
                <img src="img/Hallway.png" style="z-index:10; position: absolute; width:auto; height:auto;">\
                <img src="img/omitHallway.png" style="z-index:11; position: absolute; width:auto; height:auto;">\
                <img src="img/TheOfficeIllustrated.png" style="z-index:12; position: absolute; width:auto; height:auto;">\
                <img src="img/Balloon.png" style="z-index:13; position:absolute; width:auto; height:auto;">\
                <img src="img/Balloon_goal.png" style="z-index:14; position:absolute; width:auto; height:auto;">\
                <img src="img/Basket.png" style="z-index:15; position:absolute; width:auto; height:auto;">\
                <img src="img/Basket_goal.png" style="z-index:16; position:absolute; width:auto; height:auto;">\
                <img src="img/Book.png" style="z-index:17; position:absolute; width:auto; height:auto;">\
                <img src="img/Book_goal.png" style="z-index:18; position:absolute; width:auto; height:auto;">\
                <img src="img/Boot.png" style="z-index:19; position:absolute; width:auto; height:auto;">\
                <img src="img/Boot_goal.png" style="z-index:20; position:absolute; width:auto; height:auto;">\
                <img src="img/Cap.png" style="z-index:21; position:absolute; width:auto; height:auto;">\
                <img src="img/Cap_goal.png" style="z-index:22; position:absolute; width:auto; height:auto;">\
                <img src="img/Clipboard.png" style="z-index:23; position:absolute; width:auto; height:auto;">\
                <img src="img/Clipboard_goal.png" style="z-index:24; position:absolute; width:auto; height:auto;">\
                <img src="img/Floppy.png" style="z-index:25; position:absolute; width:auto; height:auto;">\
                <img src="img/Floppy_goal.png" style="z-index:26; position:absolute; width:auto; height:auto;">\
                <img src="img/Fryingpan.png" style="z-index:27; position:absolute; width:auto; height:auto;">\
                <img src="img/Fryingpan_goal.png" style="z-index:28; position:absolute; width:auto; height:auto;">\
                <img src="img/Jar.png" style="z-index:29; position:absolute; width:auto; height:auto;">\
                <img src="img/Jar_goal.png" style="z-index:30; position:absolute; width:auto; height:auto;">\
                <img src="img/Lightbulb.png" style="z-index:31; position:absolute; width:auto; height:auto;">\
                <img src="img/Lightbulb_goal.png" style="z-index:32; position:absolute; width:auto; height:auto;">\
                <img src="img/Mug.png" style="z-index:33; position:absolute; width:auto; height:auto;">\
                <img src="img/Mug_goal.png" style="z-index:34; position:absolute; width:auto; height:auto;">\
                <img src="img/Ovenmitt.png" style="z-index:35; position:absolute; width:auto; height:auto;">\
                <img src="img/Ovenmitt_goal.png" style="z-index:36; position:absolute; width:auto; height:auto;">\
                <img src="img/Soapdispenser.png" style="z-index:37; position:absolute; width:auto; height:auto;">\
                <img src="img/Soapdispenser_goal.png" style="z-index:38; position:absolute; width:auto; height:auto;">\
                <img src="img/Toaster.png" style="z-index:39; position:absolute; width:auto; height:auto;">\
                <img src="img/Toaster_goal.png" style="z-index:40; position:absolute; width:auto; height:auto;">\
                <img src="img/Toiletpaper.png" style="z-index:41; position:absolute; width:auto; height:auto;">\
                <img src="img/Toiletpaper_goal.png" style="z-index:42; position:absolute; width:auto; height:auto;">\
                <img src="img/Schapiro Graph.png" style="z-index:43; position:absolute; width:5; height:5px;">\
                <img src="img/Teleport.png" style="z-index:44; position:absolute; width:5px; height:5px;">\
                <img src="img/white-cover.jpg" style="z-index:999; position:absolute;  width:auto; height:auto;">\
               </div>\
               <p style="width:40%; margin-left:auto;margin-right:auto"> Before the start of the experiment, \
                we would like to ask you some questions. We process all personal data in a secure way, but if \
                you do not feel comfortable answering some of these questions, feel free to leave them blank.\
               </p>'
}

// Define intro to questions screen
var qIntro = {
    type: "html-keyboard-response",
    stimulus: qintStr,
};

// Define survey to check demographics
var survey_trial = {
    type: 'survey-text',
    questions: [
        {prompt: "How old are you?", name: 'Age'}, 
        {prompt: "Where were you born?", name: 'BirthLocation'},
        {prompt: "What is your participant number?", name: 'Ppn'}
    ],
    on_finish: function(data) {
        pResponses = JSON.parse(data.responses);
        var PN = parseInt(pResponses.Ppn);
    }
};

// Define instructions before going fullscreen
var fsIntro = {
    type: 'html-keyboard-response',
    stimulus: fsintStr
}