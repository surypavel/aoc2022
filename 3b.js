import fetch from 'file-fetch';

const request = await fetch("./3.txt");
const text = await request.text();

console.log(

    text.split("\n").flatMap((_, index, array) => 
            index % 3 === 0 && array[index+2] ? [array.slice(index, index+3)] : []
        ).map(group => {
            const badge = group.reduce((acc, backpack) => {
                const letters = backpack.split('');
                return acc === null ? letters : acc.filter(
                    letter => letters.includes(letter)
                );
            }, null)[0];
            const priority = badge.charCodeAt(0) - 'a'.charCodeAt(0) + 1
            return priority > 0 ? priority : priority + 58
        }).reduce((a,b) => a+b, 0)

);

console.log(
    text.split("\n").reduce(({ sum, prev }, backpack, index) => {
        if (!backpack) return { sum, prev };

        const letters = backpack.split('')

        if (index % 3 === 0) {
            return { sum, prev: letters }
        }

        const filteredLetters = prev.filter(
            letter => letters.includes(letter)
        );

        if (index % 3 === 2) {
            const badge = filteredLetters[0];
            const priority = badge.charCodeAt(0) - 'a'.charCodeAt(0) + 1
            const updatedPriority = priority > 0 ? priority : priority + 58

            return { sum: sum + updatedPriority, prev: [] }
        }

        return { sum, prev: filteredLetters }
    }, { sum: 0, prev: [] }).sum);