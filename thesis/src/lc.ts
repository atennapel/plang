abstract class Subst {
  abstract toString(): string;
}

class Shift extends Subst {
  readonly shift: number;

  constructor(shift: number) {
    super();
    this.shift = shift;
  }

  toString() {
    return `^${this.shift}`;
  }
}

class Dot extends Subst {
  readonly expr: Expr;
  readonly subst: Subst;

  constructor(expr: Expr, subst: Subst) {
    super();
    this.expr = expr;
    this.subst = subst;
  }

  toString() {
    return `${this.expr}.${this.subst}`;
  }
}

abstract class Expr {
  
}
