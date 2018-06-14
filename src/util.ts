export function err<T>(msg: string): T { throw new Error(msg) }
export const impossible = <T>() => err<T>('impossible');
