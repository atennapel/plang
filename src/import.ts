export const isBrowser = typeof window !== 'undefined';
export const load = (lib: string, cb: (err: Error | null, file: string) => void): void => {
  if (isBrowser) {
    fetch(`lib/${lib || 'prelude'}.p`)
      .then(x => x.text()).then(x => cb(null, x))
      .catch(err => cb(err, ''));
  } else {
    require('fs').readFile(`./lib/${lib || 'prelude'}.p`, 'utf8', cb);
  }
};
export const loadPromise = (lib: string): Promise<string> =>
  new Promise((resolve, reject) =>
    load(lib, (err, file) => err ? reject(err) : resolve(file)));
