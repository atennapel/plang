import { showContext, context } from "./context";

let logging = false;
let logContext = false;
export const setLogging = (doLog: boolean = true, doContext: boolean = false) => { 
  logging = doLog;
  logContext = doContext;
};
export const log = (msg: string, doContext: boolean | undefined = undefined) => {
  if (logging) console.log(`${msg}${(typeof doContext === 'boolean' ? doContext : logContext) ? ` in ${showContext(context)}` : ''}`);
};
