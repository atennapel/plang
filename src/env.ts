import { err } from "./utils";
import { Type, showType } from "./types";
import { Name } from "./names";

export type Env = { [name: string]: Type };

export const findVar = (env: Env, name: Name): Type =>
  env[name] || err(`undefined var ${name}`);

export const withExtend = <T>(env: Env, name: Name, type: Type, fn: (env: Env) => T): T => {
  const prev: Type | null = env[name] || null;
  env[name] = type;
  const res = fn(env);
  if (prev) env[name] = prev;
  else delete env[name];
  return res;
};

export const showEnv = (env: Env): string => {
  const r = [];
  for (let k in env) {
    r.push(`${k} : ${showType(env[k])}`);
  }
  return `{${r.join(', ')}}`;
};
