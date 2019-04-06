export interface Config {
  debug: boolean;
  showKinds: boolean;
}
export const config: Config = {
  debug: false,
  showKinds: false,
};
export const setConfig = (c: Partial<Config>) => {
  for (let k in c) (config as any)[k] = (c as any)[k];
};

export const log = (msg: () => string) => {
  if (config.debug) console.log(msg());
};

