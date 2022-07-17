let vars = ["x", "y", "z"];
let pos = 0;

FivePrototype = {
    evaluate: function (x, y, z) {
        return this.f(this.a.evaluate(x, y, z), this.b.evaluate(x, y, z), this.c.evaluate(x, y, z), this.d.evaluate(x, y, z), this.e.evaluate(x, y, z));
    },
    toString: function () {
        return this.a.toString() + " " + this.b.toString() + " " + this.c.toString() + " " + this.d.toString() + " " + this.e.toString() + " " + this.strFormat;
    }
}

TernaryPrototype = {
    evaluate: function (x, y, z) {
        return this.f(this.a.evaluate(x, y, z), this.b.evaluate(x, y, z), this.c.evaluate(x, y, z));
    },
    toString: function () {
        return this.a.toString() + " " + this.b.toString() + " " + this.c.toString() + " " + this.strFormat;
    }
};

BinaryPrototype = {
    evaluate: function (x, y, z) {
        return this.f(this.a.evaluate(x, y, z), this.b.evaluate(x, y, z));
    },
    toString: function () {
        return this.a.toString() + " " + this.b.toString() + " " + this.strFormat;
    },
    prefix: function () {
        return "(" + this.strFormat + " " + this.a.prefix() + " " + this.b.prefix() + ")";
    }
};

UnaryPrototype = {
    evaluate: function () {
        return this.f(+this.value);
    },
    toString: function () {
        return this.strFormat;
    },
    prefix: function () {
        return this.strFormat;
    }
};


function Add(a, b) {
    let add = createBinaryPrototype(a, b, "+", (a, b) => a + b);
    add.diff = function (variable) {
        return new Add(this.a.diff(variable), this.b.diff(variable));
    };
    return add;
}

function Subtract(a, b) {
    let sub = createBinaryPrototype(a, b, "-", (a, b) => a - b);
    sub.diff = function (variable) {
        return new Subtract(this.a.diff(variable), this.b.diff(variable));
    };
    return sub;
}

function Multiply(a, b) {
    let mult = createBinaryPrototype(a, b, "*", (a, b) => a * b);
    mult.diff = function (variable) {
        return new Add(
            new Multiply(
                this.a.diff(variable),
                this.b),
            new Multiply(this.a,
                this.b.diff(variable)
            )
        );
    };
    return mult;
}

function Divide(a, b) {
    let div = createBinaryPrototype(a, b, "/", (a, b) => a / b);
    div.diff = function (variable) {
        return new Divide(
            new Subtract(
                new Multiply(
                    this.a.diff(variable),
                    this.b),
                new Multiply(this.a,
                    this.b.diff(variable))),
            new Multiply(this.b,
                this.b
            )
        )
    };
    return div;
}

function Negate(value) {
    let negate = createUnaryPrototype(value, "negate", (value) => -value);
    negate.evaluate = function (x, y, z) {
        return this.f(this.value.evaluate(x, y, z));
    };
    negate.diff = function (variable) {
        return new Negate(this.value.diff(variable));
    };
    negate.toString = function () {
        return this.value.toString() + " " + this.strFormat;
    };
    negate.prefix = function () {
        return "(" + this.strFormat + " " + this.value.prefix() + ")";
    }
    return negate;
}

function Const(value) {
    let cnst = createUnaryPrototype(value, value.toString(), (value) => value);
    cnst.diff = function () {
        return new Const(0);
    };
    return cnst;
}

function Variable(name) {
    let variable = Object.create(UnaryPrototype);
    variable.strFormat = name;
    variable.evaluate = function (x, y, z) {
        return {"x": x, "y": y, "z": z}[this.strFormat];
    };
    variable.diff = function (variable) {
        if (variable === this.strFormat) return new Const(1);
        return new Const(0);
    };
    return variable;
}

function Min3(a, b, c) {
    let min = Object.create(TernaryPrototype);
    min.f = (a, b, c) => Math.min(a, b, c);
    min.strFormat = "min3";
    min.a = a;
    min.b = b;
    min.c = c;
    min.diff = function (variable) {
        return new Min3(this.a.diff(variable), this.b.diff(variable), this.c.diff(variable));
    }
    return min;
}

function Max5(a, b, c, d, e) {
    let max = Object.create(FivePrototype);
    max.f = (a, b, c, d, e) => Math.max(a, b, c, d, e);
    max.strFormat = "max5";
    max.a = a;
    max.b = b;
    max.c = c;
    max.d = d;
    max.e = e;
    max.diff = function (variable) {
        return new Max5(this.a.diff(variable), this.b.diff(variable), this.c.diff(variable), this.d.diff(variable), this.e.diff(variable));
    }
    return max;
}

function Sinh(value) {
    let sinh = createUnaryPrototype(value, "sinh", (value) => Math.sinh(value));
    sinh.evaluate = function (x, y, z) {
        return this.f(this.value.evaluate(x, y, z));
    };
    sinh.diff = function (variable) {
        return new Multiply(this.value.diff(variable), Cosh(this.value));
    };
    sinh.toString = function () {
        return this.value.toString() + " " + this.strFormat;
    };
    sinh.prefix = function () {
        return "(" + this.strFormat + " " + this.value.prefix() + ")";
    }
    return sinh;
}

function Cosh(value) {
    let cosh = createUnaryPrototype(value, "cosh", (value) => Math.cosh(value));
    cosh.evaluate = function (x, y, z) {
        return this.f(this.value.evaluate(x, y, z));
    };
    cosh.diff = function (variable) {
        return new Multiply(this.value.diff(variable), Sinh(this.value));
    };
    cosh.toString = function () {
        return this.value.toString() + " " + this.strFormat;
    };
    cosh.prefix = function () {
        return "(" + this.strFormat + " " + this.value.prefix() + ")";
    }
    return cosh;
}

function createUnaryPrototype(value, strFormat, f) {
    let proto = Object.create(UnaryPrototype);
    proto.value = value;
    proto.strFormat = strFormat;
    proto.f = f;
    return proto;
}

function createBinaryPrototype(a, b, strFormat, f) {
    let proto = Object.create(BinaryPrototype);
    proto.strFormat = strFormat;
    proto.a = a;
    proto.b = b;
    proto.f = f;
    return proto;
}

function parse(exp) {
    let stack = [];
    let ops = exp.trim().split(/\s+/);
    for (const op of ops) {
        if (!isNaN(+op)) {
            stack.push(new Const(op));
        } else if (vars.includes(op)) {
            stack.push(new Variable(op));
        } else if (op === "negate") {
            stack.push(new Negate(stack.pop()));
        } else {
            const second = stack.pop();
            const first = stack.pop();
            if (op === "+") {
                stack.push(new Add(first, second));
            } else if (op === "-") {
                stack.push(new Subtract(first, second));
            } else if (op === "*") {
                stack.push(new Multiply(first, second));
            } else if (op === "/") {
                stack.push(new Divide(first, second));
            } else if (op === "min3") {
                stack.push(new Min3(stack.pop(), first, second));
            } else if ( op === "max5") {
                let a3 = stack.pop();
                let a2 = stack.pop();
                stack.push(new Max5(stack.pop(), a2, a3, first, second));
            } else {
                return undefined;
            }
        }
    }
    return stack.pop();
}

function parsePrefix(e) {
    bracketBalance(e);
    let expr = e.trim().replaceAll(/\s+/g, " ").split("");
    pos = 0;
    let ans = nextExpression(expr);
    if (pos !== expr.length) {
        throw new ArgumentsException("Found excess symbol in " + e);
    }
    pos = 0;
    return ans;
}

const mp = {
    '+': Add,
    '-': Subtract,
    '*': Multiply,
    '/': Divide
}


function nextExpression(expr) {
    skip(expr);
    let op = expr[pos];
    if (!isNaN(+op) || op === "-") {
        let temp = nextConst(expr);
        return new Const(temp);
    } else if (vars.includes(op)) {
        pos++;
        if (!(expr[pos - 2] === ")" || expr[pos - 2] === " " || expr.length === 1)) {
            throw new UnexpectedSymbolException("Unexpected variable statement:" + expr);
        }
        if (expr[pos] === " " || expr[pos] === "(" || expr[pos] === ")" || !isNaN(+expr[pos]) || pos === expr.length) {
            return new Variable(op);
        }
        let temp = expr[pos];
        throw new UnknownSymbolException(temp);
    } else if (op === "(") {
        pos++;
        skip(expr);
        if (expr.slice(pos, pos + 4).join("") === "sinh") {
            return new Sinh(nextSingle(expr, "sinh"));
        } else if (expr.slice(pos, pos + 4).join("") === "cosh") {
            return new Cosh(nextSingle(expr, "cosh"));
        } else if (expr.slice(pos, pos + 6).join("") === "negate") {
            return new Negate(nextSingle(expr, "negate"));
        } else if (expr[pos] in mp) {
            op = expr[pos];
            expectedNext("(", expr, ++pos);
            let first = nextExpression(expr);
            let second = nextExpression(expr);
            skip(expr);
            expectedNext(")", expr, pos++);
            return new mp[op](first, second);
        } else {
            throw new UnexpectedSymbolException("Expected binary operator, found " + expr[pos]);
        }
    } else {
        throw new UnknownSymbolException(expr[pos]);
    }
}

function nextSingle(expr, name) {
    pos += name.length;
    expectedNext("(", expr, pos);
    let value = nextExpression(expr);
    skip(expr);
    expectedNext(")", expr, pos++);
    return value;
}

function nextConst(expr) {
    let num = "";
    while (/^-?\d+$/.test(expr[pos]) || expr[pos] === "-") {
        num += expr[pos++];
    }
    if (num === "-") {
        throw new UnexpectedSymbolException("Expected number, found " + expr[pos]);
    }
    return +num;
}

function bracketBalance(expr) {
    let balance = 0;
    for (let i = 0; i < expr.length; i++) {
        if (expr[i] === '(') {
            balance++;
        }
        if (expr[i] === ')') {
            balance--;
        }
        if (balance < 0) {
            throw new Error("Wrong bracket count: " + expr);
        }
    }
    if (balance !== 0) {
        throw new Error("Wrong bracket count: " + expr);
    }
}


function skip(expr) {
    while (expr[pos] === " ") {
        pos++;
    }
}

function expectedNext(target, expr, p) {
    if (expr[p] !== target && expr[p] !== " ") {
        throw new UnexpectedSymbolException("Expected " + target + ", found " + expr[p]);
    }
}

function UnknownSymbolException(msg) {
    this.name = "UnknownSymbolException";
    this.message = "Unknown symbol: " + msg;
}

UnknownSymbolException.prototype = Error.prototype;

function BracketBalanceException(msg) {
    this.name = "BracketBalanceException";
    this.message = "Wrong bracket balance in expression: " + msg;
}

BracketBalanceException.prototype = Error.prototype;

function UnexpectedSymbolException(msg) {
    this.name = "UnexpectedSymbolException";
    this.message = msg;
}

UnexpectedSymbolException.prototype = Error.prototype;

function ArgumentsException(msg) {
    this.name = "ArgumentsException";
    this.message = msg;
}

ArgumentsException.prototype = Error.prototype;


