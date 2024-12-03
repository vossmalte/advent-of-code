// deno-lint-ignore no-unused-vars
function mul(a: number, b: number) {
  return a * b;
}

const f = await Deno.readTextFile("input.txt");

const regex = /mul\(\d+,\d+\)/g;

const matches = f.matchAll(regex);

const bill = matches.toArray().map((x) => x.toString()).join("+");

console.log(eval(bill));

const regex2 = /mul\(\d+,\d+\)|do\(\)|don't\(\)/g;

const matches2 = f.matchAll(regex2).toArray().map((x) => x.toString());

// split by do() and takeUntil don't()
const bill2 = matches2.join("").split("do()").flatMap((x) =>
  x.split("don't()")[0]
).join("").replaceAll("mul", "+mul").slice(1);

console.log(eval(bill2));
