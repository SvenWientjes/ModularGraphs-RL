// List of room images (idx = node nr)
var roomsdef = ['img/Balloon.png', 'img/Basket.png', 'img/Book.png', 'img/Boot.png', 'img/Calculator.png', 'img/Cap.png', 
'img/Clipboard.png', 'img/Floppy.png', 'img/Fryingpan.png', 'img/Iron.png', 'img/Jar.png', 'img/Lantern.png',
'img/Lightbulb.png', 'img/Mug.png', 'img/Ovenmitt.png', 'img/Pot.png', 'img/Soapdispenser.png', 'img/Tapedispenser.png',
'img/Toaster.png', 'img/Toiletpaper.png'];

// List of goal images (idx = node nr -> correspond to rooms!)
var goalsdef = ['img/Balloon_goal.png', 'img/Basket_goal.png', 'img/Book_goal.png', 'img/Boot_goal.png', 'img/Calculator_goal.png', 'img/Cap_goal.png', 
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
    };
    if(mbend=='nogoal'){
        return '<p>There was no goal in this miniblock.</p>'+
        '<p>Better luck next time! Maybe you could have prevented some losses.</p>'
    }
}

function fbScreen(nSteps, nGoals, trRew, trLeft, total_reward){
    if(trRew >= 0 & total_reward >= 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span>. Congratulations!</p>' +
        '<p>This block, you took a total of <span style="color: #ae7bdd">' + nSteps + ' steps</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew >= 0 & total_reward < 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span>. Congratulations!</p>' +
        '<p>This block, you took a total of <span style="color: #ae7bdd">' + nSteps + ' steps</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward >= 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span>. Stay strong!</p>' +
        '<p>This block, you took a total of <span style="color: #ae7bdd">' + nSteps + ' steps</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' blocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward < 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span>. Stay strong!</p>' +
        '<p>This block, you took a total of <span style="color: #ae7bdd">' + nSteps + ' steps</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    }
}