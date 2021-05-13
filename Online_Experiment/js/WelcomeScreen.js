var welcStr = '<p >Welcome to the experiment. Press any key to continue.</p><p style="font-family:VideoGame;font-size:10%;">.</p>'
var qintStr = '<p style="width:50%; margin-left:auto;margin-right:auto">Before the start of the experiment, we would like to ask you some questions. '+
              'We process all personal data in a secure way, but if you do not feel comfortable '+
              'answering some of these questions, feel free to leave them blank.</p>'
var fsintStr = '<p style="width:50%; margin-left:auto;margin-right:auto">For the experiment it is important that we set your browser to fullscreen mode.'+
               ' The next page will contain a button you can press to enter fullscreen.' +
               ' If you leave fullscreen during the experiment at any time, this will be recorded and'+
               ' unfortunately we will not be able to complete your payment if this has occurred.'+
               ' Additionally, be aware that the experiment only works for 16:9 aspect ratio monitors.'+
               ' If you do not have a monitor with this aspect ratio, please quit the experiment, as we will'+
               ' ultimately not be able to complete your payment. Please drag the browser window to the '+
               ' monitor you would like to use during the experiment, before entering fullscreen.</p>'+
               '<p>Press any key to continue.</p>'

/* ---- JSpsych Trial Variables ---- */
// Define welcome screen
var welcome = {
    type: "html-keyboard-response",
    stimulus: welcStr,
};

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