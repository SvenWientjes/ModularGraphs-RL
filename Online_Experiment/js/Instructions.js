var instr1s = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;'+
              'display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                    'Welcome! Before we start the experiment we will provide you with instructions on how to perform the task adequately. You will be able to navigate'+
                    ' these instructions using the &lt;z&gt; key to go backward, and the &lt;m&gt; key to go forward. Press &lt;m&gt; to continue!'+
                '</p>'+
                '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '|-&lt;z&gt;'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '1/10'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '&lt;m&gt;-&gt;'+
                    '</p>'+
                '</div>'+
              '</div>'

var instr2s = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;'+
              'display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                    'During the experiment, you will assume the role of an art dealer. You will perform in total 100 jobs. Every job sends you into an'+
                    ' art gallery, where you will have to look for a specific painting. The desired painting will be shown at the start of that job. You'+
                    ' can never be sure if you will find the painting in time. Every job you will have time to inspect a total of 15 paintings. As soon as'+
                    ' you find the target painting, you will receive a payment. If you do not find the target painting during the job, you will receive a fine.'+
                '</p>'+
                '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '&lt;-&lt;z&gt;'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '2/10'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '&lt;m&gt;-&gt;'+
                    '</p>'+
                '</div>'+
              '</div>'

var showgoalcue = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                   'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                        'At the start of each job, you will be shown which painting you are looking for. All paintings are common objects that '+
                        'you might encounter in your daily life. The goal will be displayed in a gold border similar to this:'+

                    '</p>'+
                    '<img src="img/example-goal.jpg" style="max-width:50vw;max-height:auto; margin-bottom:auto;">'+
                    '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;-&lt;z&gt;'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '3/10'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;m&gt;-&gt;'+
                        '</p>'+
                    '</div>'+
                '</div>'

var showstimfix = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                   'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                        'While you are on the job, you will alternate between display rooms containing paintings, and hallways that '+
                        'connect display rooms. Movement will be automatic, you cannot choose directly where to go.' +
                    '</p>'+
                    '<div style="display:flex; justify-content:space-around; min-width:100vw;margin-bottom:0;">'+
                        '<img src="img/coinIllustrate.png" style="max-width:40vw;max-height:auto;">'+
                        '<img src="img/fixHallway.png" style="max-width:40vw;max-height:auto;"></div>'+
                    '<div style="display:flex; justify-content:space-around; min-width:100vw;margin-bottom:auto;margin-top:0;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:15px;width:40vw">An example of a display room.</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:15px;max-width:40vw">An example of a hallway. It contains a fixation cross. Please keep your eyes\
                            focussed on this cross during the course of the job.</p></div>'+
                    '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;-&lt;z&gt;'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '4/10'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;m&gt;-&gt;'+
                        '</p>'+
                    '</div>'+
                '</div>'

var showactions1 = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                   'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                        'In each display room you have to make a choice. If you think you will be able to find the painting before the end of this job, ' +
                        'you can press the &lt;m&gt; key to text your boss. You will ultimately get paid 1 point for each time you have reassured ' +
                        'your boss. However, if you do not find the painting that job, your boss will fine you this amount. How many points are on the line'+
                        ', is displayed on the bottom of your screen, indicated by yellow coins.' +
                    '</p>'+
                    '<img src="img/3coin-illustrate.png" style="max-width:40vw;max-height:auto; margin-bottom:auto;">'+
                    '<p style="color:white; font-family:VideoGame; font-size:15px; line-height:1.5; max-width:40vw; margin-top:auto; margin-bottom:auto;">'+
                        'For example, here there are three points on the line, indicated by the three yellow coins on the bottom.' +
                    '</p>'+
                    '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;-&lt;z&gt;'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '5/10'+
                        '</p>'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                            '&lt;m&gt;-&gt;'+
                        '</p>'+
                    '</div>'+
                '</div>'

var showactions2 = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                    'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                            'As soon as you press the &lt;m&gt; key, the amount of yellow coins will increase. If you are not sure you will reach the goal ' +
                            'and do not wish to take this risk, you can press the &lt;z&gt; key, and nothing will happen. If you wish to make as many points '+
                            'as possible, you must find the right moments to press the &lt;m&gt; key, and take the risk. Once you have increased the number ' +
                            'of coins, you cannot decrease it again for that job. You can only press &lt;m&gt; or &lt;z&gt; once per display room.'+
                        '</p>'+
                        '<img src="img/4coin-illustrate.png" style="max-width:40vw;max-height:auto; margin-bottom:auto;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:15px; line-height:1.5; max-width:40vw; margin-top:auto; margin-bottom:auto;">'+
                            'For example, here the number of coins has been increased to four. This cannot be undone during this job.' +
                        '</p>'+
                        '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;-&lt;z&gt;'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '6/10'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;m&gt;-&gt;'+
                            '</p>'+
                        '</div>'+
                    '</div>'

var showactions3 = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                    'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                            'Each job, you start out with 0 coins on the line. This means that, by continuously pressing &lt;z&gt;, you do not have to '+
                            'lose any of your current points, if you wish to not take any risks. However, you will also not be able to earn points that way.'+
                        '</p>'+
                        '<img src="img/coinIllustrate.png" style="max-width:40vw;max-height:auto; margin-bottom:auto;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:15px; line-height:1.5; max-width:40vw; margin-top:auto; margin-bottom:auto;">'+
                            'For example, here there are no coins on the line.' +
                        '</p>'+
                        '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;-&lt;z&gt;'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '7/10'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;m&gt;-&gt;'+
                            '</p>'+
                        '</div>'+
                    '</div>'

var omitpunish = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                    'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                            'In each display room, you have to press either &lt;z&gt; or &lt;m&gt;. If you do nothing, your boss will get angry, and take '+
                            'away one point from your grand total immediately. You do not have to press any key when in a hallway. If you have forgotten to '+
                            'press a key in the previous display room, you will be reminded in the following hallway that a point has been deducted.'+
                        '</p>'+
                        '<img src="img/omitHallway.png" style="max-width:40vw;max-height:auto; margin-bottom:auto;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:15px; line-height:1.5; max-width:40vw; margin-top:auto; margin-bottom:auto;">'+
                            'An example of a hallway after no key was pressed in the previous display room.' +
                        '</p>'+
                        '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;-&lt;z&gt;'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '8/10'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;m&gt;-&gt;'+
                            '</p>'+
                        '</div>'+
                    '</div>'

var officeinstr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto;'+
                    'position:absolute; top:0;left:0; display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                        '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto; margin-bottom:auto;">'+
                            'After each job, you will return to your office. In the office, you will be reminded how many points you won or lost on the previous'+
                            ' job. You will also be reminded how many jobs you have left, and how many succesful sales you have achieved so far. A successful '+
                            'sale means that the target painting was acquired on a job.'+
                        '</p>'+
                        '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;-&lt;z&gt;'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '9/10'+
                            '</p>'+
                            '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                                '&lt;m&gt;-&gt;'+
                            '</p>'+
                        '</div>'+
                    '</div>'


var instr4s = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;'+
              'display:flex; align-items:center; justify-content:space-between; flex-direction:column;">'+
                '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-top:auto;">'+
                    'The grand total of all the payouts and fines you receive over the course of 100 jobs will be displayed continuously on top of the screen.'+
                    ' By the end of the experiment, you will be paid according to how many points you have accumulated here.'+
                '</p>'+
                '<p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw; margin-bottom:auto;">'+
                    'This is the end of the instructions. Lets practice! When you press the &lt;m&gt; key, these instructions will end, and you will start a practice job. '+
                    'You will not be able to return to these instructions, but you will be able to repeat the practice job if you wish. Any points made during these practice '+
                    'jobs will be immediatly reset, and will not contribute to your final payout. The paintings during practice are different from the paintings used in the '+
                    'experiment itself. These are used merely to illustrate the structure of the task.' +
                '</p>'+
                '<div style="display:flex; min-width:100vw; justify-content:space-between; align-items: flex-end">'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '|-&lt;z&gt;'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '10/10'+
                    '</p>'+
                    '<p style="color:white; font-family:VideoGame; font-size:25px;">'+
                        '&lt;m&gt;-&gt;'+
                    '</p>'+
                '</div>'+
              '</div>'

var prac1instr = '<div id="InStruct" style="background-color:black; height:100vh; width:100vw; margin:0 auto; position:absolute; top:0;left:0;\
                    display:flex; align-items:center; justify-content:center; flex-direction:column;">\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Lets first practice a job where you will reach the goal guaranteed. Please respond with &lt;m&gt; in every \
                        display room, so you can see the coins increase.\
                    </p>\
                    <p style="color:white; font-family:VideoGame; font-size:25px; line-height:1.5; max-width:80vw;">\
                        Press &lt;m&gt; to continue.\
                    </p>\
                </div>'

/* ---- JSpsych Trial Variables ---- */
// The instructions!
var instr1= {
    type: 'instructions',
    pages: [instr1s, instr2s, showgoalcue, showstimfix, showactions1, showactions2, showactions3, omitpunish, officeinstr, instr4s],
    key_backward: 'z',
    key_forward: 'm'
}