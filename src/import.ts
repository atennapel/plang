import { ReplState } from './repl';

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

export const store = (state: ReplState): void => {
  const doc = JSON.stringify(state);
  if (typeof localStorage === 'undefined') {
    require('fs').writeFileSync('_repl', doc, 'utf8');
  } else {
    localStorage.setItem('replstate', doc);
  }
};
export const restore = (): ReplState | null => {
  try {
    let doc: string | null = null;
    if (typeof localStorage === 'undefined') {
      doc = require('fs').readFileSync('.repl', 'utf8');
    } else {
      doc = localStorage.getItem('replstate');
    }
    if (!doc) return null;
    return JSON.parse(doc);
  } catch (err) {
    return null;
  }
};
