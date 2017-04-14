start = Expr

Expr =
  "(" e:Expr ")" { return e }
  / left:ExprBottom right:(whitespace* Expr)+
    { console.log(left, right);return {tag: 'App', left, right} }
  / ExprBottom

ExprBottom =
  n:number { return {tag: 'Number', val: n} }
  / n:name { return {tag: 'Name', val: n} }

name = name:([a-z]i[a-z0-9]i*)
  {
    return name[0] + (name[1]? name[1].join(''): '');
  }

number = digits:([0-9]+ ("." [0-9]*)?)
  {
    var first = digits[0].join('');
    var second = digits[1]? '.' + digits[1][1].join(''): '';
    return +(first + second);
  }

whitespace = [' '\t\r\n]
