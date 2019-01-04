import { showContext, context, prettyContext } from "./context";

export type LoggingOptions = {
  showContext?: boolean;
  prettyContext?: boolean;
};

let logging = false;
let options: LoggingOptions = {};
export const setLogging = (doLog: boolean = true, opt: LoggingOptions = {}) => { 
  logging = doLog;
  options = opt;
};
export const log = (msg: string, doContext: boolean | undefined = undefined) => {
  const ctx = (typeof doContext === 'boolean' ? doContext : options.showContext) ?
    ` in ${options.prettyContext ? prettyContext(context) : showContext(context)}` : '';
  if (logging) console.log(`${msg}${ctx}`);
};
