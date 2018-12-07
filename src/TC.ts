import { Either, Right } from "./either";

export type Err = string;
export type TC<T> = Either<Err, T>;

export const ok: TC<true> = Right(true as true);
