const fs = require('fs');
const text = fs.readFileSync('./2.txt', 'utf8');

console.log(text.split("\n").reduce((score, round) => {
    const [a, x] = round.split(' ');
    if (!a || !x) return score;
    const hisShapeScore = a.charCodeAt(0) - ('A'.charCodeAt(0));
    const outcome = x.charCodeAt(0) - ('X'.charCodeAt(0));
    const outcomeScore = outcome * 3;
    const gameScore = (((hisShapeScore+3) + (outcome-1)) % 3)+1;
    return score + gameScore + outcomeScore;
} , 0)); 
