const fs = require('fs');
const text = fs.readFileSync('./3.txt', 'utf8');

console.log(
    text.split("\n").reduce((sum, backpack) => {
        if (!backpack) return sum;

        const middle = backpack.length / 2
        const letters = backpack.split('')
        const misarranged = letters.find(
            letter => letters.slice(middle).includes(letter)
        )
        const priority = misarranged.charCodeAt(0) - 'a'.charCodeAt(0) + 1
        const updatedPriority = priority > 0 ? priority : priority + 58
        return sum + updatedPriority
    }, 0));