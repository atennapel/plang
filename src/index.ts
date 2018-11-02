import {
  Kind,
  kvar,
  kfuns,
} from './kinds';


const type = kvar('type');
console.log(
  '' + kfuns(type, type, kfuns(type, type, type), type)
);
