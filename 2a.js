const fs = require('fs');
const text = fs.readFileSync('./2.txt', 'utf8');

console.log(text.split("\n").reduce((score, round) => {
    const [a, x] = round.split(' ');
    if (!a || !x) return score;
    const hisShapeScore = a.charCodeAt(0) - ('A'.charCodeAt(0)) + 1;
    const myShapeScore = x.charCodeAt(0) - ('X'.charCodeAt(0)) + 1;
    const tieScore = Number(hisShapeScore === myShapeScore) * 3;
    const winScore = Number((myShapeScore - hisShapeScore % 3) === 1) * 6;
    return score + myShapeScore + tieScore + winScore;
} , 0));
