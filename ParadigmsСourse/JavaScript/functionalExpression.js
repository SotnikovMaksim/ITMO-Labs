"use strict";
// А можно hard?

const binary = f => (first, second) => (x, y, z) => f(first(x, y, z), second(x, y, z));
const ternary = f => (first, second, third) => (x, y, z) => f(first(x, y, z), second(x, y, z), third(x, y, z));
const signFunc = (mode, f) => (x, y, z) => {
    if (mode === "negate") return -(f(x, y, z));
    if (mode === "abs") return Math.abs(f(x, y, z));
}

const add = binary((a, b) => a + b);
const c = binary(a => a)
const subtract = binary((a, b) => a - b);
const multiply = binary((a, b) => a * b);
const divide = binary((a, b) => a / b);
const iff =  ternary((a, b, c) => a >= 0 ? b : c);
const avg3 = ternary((a, b, c) => (a + b + c) / 3);
const med5 = (f, g, w, v, m) => (x, y, z) => [f(x, y, z), g(x, y, z), w(x, y, z), v(x, y, z), m(x, y, z)].sort((a, b) => a - b)[2];
const negate = f => signFunc("negate", f);
const abs = f => signFunc("abs", f);
const variable = name => (x, y, z) => {
    if (name === "x") return x;
    if (name === "y") return y;
    if (name === "z") return z;
};
const cnst = mode => () => {
    if (!isNaN(+mode)) return +mode;
    if (mode === "pi") return Math.PI;
    if (mode === "e") return Math.E;
};
const pi = cnst("pi");
const e = cnst("e");

const polishParse = exp => {
    let stack = [];
    let ops = exp.trim().split(" ");
    for (const op of ops) {
        if (!op) continue;
        if (!isNaN(+op) || op === "pi" || op === "e") {
            stack.push(cnst(op));
        } else if (op === "x" || op === "y" || op === "z") {
            stack.push(variable(op));
        } else if (op === "negate" || op === "abs") {
            stack.push(signFunc(op, stack.pop()));
        } else {
            const second = stack.pop();
            const first = stack.pop();
            if (op === "+") {
                stack.push(add(first, second));
            } else if (op === "-") {
                stack.push(subtract(first, second));
            } else if (op === "*") {
                stack.push(multiply(first, second));
            } else if (op === "/") {
                stack.push(divide(first, second));
            } else if (op === "avg3") {
                stack.push(avg3(first, second, stack.pop()));
            } else if (op === "iff") {
                stack.push(iff(stack.pop(), first, second));
            } else if (op === "med5") {
                stack.push(med5(first, second, stack.pop(), stack.pop(), stack.pop()));
            } else {
                return undefined;
            }
        }
    }
    return stack.pop();
}
const parse = exp => (x, y, z) => polishParse(exp)(x, y, z);