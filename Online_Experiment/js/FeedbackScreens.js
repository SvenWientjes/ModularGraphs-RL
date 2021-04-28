// Defines the strings to pass through blockend.
function miniblockFb(mbend){
    if(mbend=='downgoal'||mbend=='upgoal'||mbend=='missgoal'){
        return '<p>Congratulations! You have reached the goal.</p>'+
        '<p>You have been awarded the corresponding points</p>'
    }else if(mbend=='downend'||mbend=='upend'||mbend=='missend'){
        return '<p>There was no goal in this miniblock.</p>'+
        '<p>Better luck next time! Maybe you could have prevented some losses.</p>'
    }
}
// Full feedback screen (after blockend message, before new miniblock)
function fbScreen(nSteps, nGoals, trRew, trLeft, total_reward, mbPunish){
    if(trRew >= 0 & total_reward >= 0 & mbPunish == 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span> from betting. Congratulations!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew >= 0 & total_reward < 0 & mbPunish == 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span> from betting. Congratulations!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward >= 0 & mbPunish == 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span> from betting. Stay strong!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' blocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward < 0 & mbPunish == 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span> from betting. Stay strong!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    } else if(trRew >= 0 & total_reward >= 0 & mbPunish < 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span> from betting. Congratulations!</p>' +
        '<p>This block, you lost a total of <span style="color: #ff0000">' +
        mbPunish + ' points</span> from not responding. Please stay vigilant!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew >= 0 & total_reward < 0 & mbPunish < 0){
        return '<p>In this block you made <span style="color: #93c54b">' +
        trRew + ' points</span> from betting. Congratulations!</p>' +
        '<p>This block, you lost a total of <span style="color: #ff0000">' +
        mbPunish + ' points</span> from not responding. Please stay vigilant!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward >= 0 & mbPunish < 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span> from betting. Stay strong!</p>' +
        '<p>This block, you lost a total of <span style="color: #ff0000">' +
        mbPunish + ' points</span> from not responding. Please stay vigilant!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' blocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #93c54b">' + total_reward + ' points</span>.</p>'
    } else if(trRew < 0 & total_reward < 0 & mbPunish < 0){
        return '<p>In this block you made <span style="color: #ff0000">' +
        trRew + ' points</span> from betting. Stay strong!</p>' +
        '<p>This block, you lost a total of <span style="color: #ff0000">' +
        mbPunish + ' points</span> from not responding. Please stay vigilant!</p>' +
        '<p>This block, you saw a total of <span style="color: #ae7bdd">' + (nSteps+1) + ' rooms</span>.</p>' +
        '<p>In total, you have collected the goal <span style="color: #ae7bdd">' + nGoals + ' times</span>.</p>'+
        '<p>You have a total of <span style="color: #ae7bdd">' + parseInt(99-trLeft) + ' miniblocks</span> left.</p>'+
        '<p>In total you now have <span style="color: #ff0000">' + total_reward + ' points</span>.</p>'
    }
}