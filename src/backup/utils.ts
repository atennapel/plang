export const impossible = () => { throw new Error('impossible') };

export const assocGet = <A extends { equals: (other: A) => boolean }, B>(arr: [A, B][], val: A): B | null => {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i][0].equals(val)) return arr[i][1];
  }
  return null;
};

export const containsDuplicate = <A extends { equals: (other: A) => boolean }>(arr: A[]): boolean => {
  const acc: A[] = [];
  for (let i = 0; i < arr.length; i++) {
    const c = arr[i];
    for (let j = 0; j < acc.length; j++) {
      if (acc[j].equals(c)) return true;
    }
    acc.push(c);
  }
  return false;
};
