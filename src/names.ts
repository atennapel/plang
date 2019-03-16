export interface Name {
  readonly name: string;
  readonly id: number;
}
export const Name = (name: string, id: number): Name =>
  ({ name, id });

