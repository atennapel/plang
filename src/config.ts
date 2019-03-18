export const config = {
  showKinds: false,
  logging: false,
};

export const log = (msg: string) => {
  if (config.logging) console.log(msg);
};
