import { Either } from "./either";

export type Err = string;
export type TC<T> = Either<Err, T>;
