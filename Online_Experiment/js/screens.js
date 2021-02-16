// List of room images (idx = node nr)
var rooms = ['img/Balloon.png', 'img/Basket.png', 'img/Book.png', 'img/Boot.png', 'img/Calculator.png', 'img/Cap.png', 
'img/Clipboard.png', 'img/Floppy.png', 'img/Fryingpan.png', 'img/Iron.png', 'img/Jar.png', 'img/Lantern.png',
'img/Lightbulb.png', 'img/Mug.png', 'img/Ovenmitt.png', 'img/Pot.png', 'img/Soapdispenser.png', 'img/Tapedispenser.png',
'img/Toaster.png', 'img/Toiletpaper.png'];

// List of goal images (idx = node nr -> correspond to rooms!)
var goals = ['img/Balloon_goal.png', 'img/Basket_goal.png', 'img/Book_goal.png', 'img/Boot_goal.png', 'img/Calculator_goal.png', 'img/Cap_goal.png', 
'img/Clipboard_goal.png', 'img/Floppy_goal.png', 'img/Fryingpan_goal.png', 'img/Iron_goal.png', 'img/Jar_goal.png', 'img/Lantern_goal.png',
'img/Lightbulb_goal.png', 'img/Mug_goal.png', 'img/Ovenmitt_goal.png', 'img/Pot_goal.png', 'img/Soapdispenser_goal.png', 'img/Tapedispenser_goal.png',
'img/Toaster_goal.png', 'img/Toiletpaper_goal.png'];

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