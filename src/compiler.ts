import { Term } from './terms';
import { showName } from './names';

export const compile = (term: Term): string => {
  switch (term.tag) {
    case 'Var': return showName(term.name);
    case 'Abs': return `(${showName(term.name)} => ${compile(term.body)})`;
    case 'App': return `${compile(term.left)}(${compile(term.right)})`;
    case 'Ann': return compile(term.term);
  }
};
