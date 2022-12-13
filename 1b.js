const sum = a => a.reduce((a, b) => a + b, 0)

const fs = require('fs');
const text = fs.readFileSync('./1.txt', 'utf8');

console.log(sum(text.split("\n\n").reduce((bestElves, elf) => {
    const currentElfSum = sum(elf.split("\n").map(Number));
    return [
        ...bestElves.filter(x => x > currentElfSum),
        currentElfSum,
        ...bestElves.filter(x => x <= currentElfSum)
    ].slice(0, 3);
} , [])));
