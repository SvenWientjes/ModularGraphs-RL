var trialorder = [
    [1,0,2,3,4,5],
    [5,4,3,2,1,0],
    [2,1,0,5,4,3],
    [5,4,3,0,1,2],
    [3,4,5,2,1,0]
];

var goalorder = [0,0,0,0,0];

// List of room images (idx = node nr)
var rooms = ['img/Balloon.png', 'img/Book.png', 'img/Calculator.png', 'img/Cap.png', 'img/Floppy.png', 'img/Fryingpan.png'];

// List of goal images (idx = node nr -> correspond to rooms!)
var goals = ['img/goal_cue.png'];

// Defines the strings to pass through blockend.
function miniblockFb(mbend){
    if(mbend=='early'){
        return '<p>You ended the miniblock early.</p>'+
        '<p>You might have saved some losses!</p>'
    };
    if(mbend=='getgoal'){
        return '<p>Good job! You have reached the goal.</p>'+
        '<p>You have been awarded the corresponding points</p>'
    };
    if(mbend=='missgoal'){
        return '<p>You missed the goal!</p>'+
        '<p>You could have had a bonus, pay close attention!</p>'
    }
}