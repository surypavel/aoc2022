const fs = require('fs');
const data = fs.readFileSync('./13.txt', 'utf8');

const toArray = a => Array.isArray(a) ? a : [a];
const compareArrays = (a, b) => {
    if (!a.length || !b.length) return a.length - b.length;
    const cmp = compare(a[0], b[0]);
    if (cmp !== 0) return cmp;
    return compareArrays(a.slice(1), b.slice(1));
}
const compare = (a, b) => (Number.isInteger(a) && Number.isInteger(b)) ? a - b : compareArrays(toArray(a), toArray(b));

const result = data.split('\n\n')
    .map((group, i) => compare(...group.split('\n').map(JSON.parse)) < 0 ? i+1 : 0)
    .reduce((a,b) => a+b, 0)

const dividers = [[[2]], [[6]]];
const allRows = [...data.split('\n').filter(Boolean).map(JSON.parse), ...dividers];
const result2 = dividers.reduce((acc, curr) => acc * allRows.filter(item => compare(item, curr) <= 0).length, 1)

console.log(result);
console.log(result2);