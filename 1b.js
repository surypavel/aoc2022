import fetch from 'file-fetch';

const sum = a => a.reduce((a, b) => a + b, 0)

const request = await fetch("./1.txt");
const text = await request.text();
console.log(sum(text.split("\n\n").reduce((bestElves, elf) => {
    const currentElfSum = sum(elf.split("\n").map(Number));
    return [
        ...bestElves.filter(x => x > currentElfSum),
        currentElfSum,
        ...bestElves.filter(x => x <= currentElfSum)
    ].slice(0, 3);
} , [])));
