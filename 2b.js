import fetch from 'file-fetch';

const request = await fetch("./2.txt");
const text = await request.text();

console.log(text.split("\n").reduce((score, round) => {
    const [a, x] = round.split(' ');
    if (!a || !x) return score;
    const hisShapeScore = a.charCodeAt(0) - ('A'.charCodeAt(0));
    const outcome = x.charCodeAt(0) - ('X'.charCodeAt(0));
    const outcomeScore = outcome * 3;
    const gameScore = (((hisShapeScore+3) + (outcome-1)) % 3)+1;
    return score + gameScore + outcomeScore;
} , 0)); 
