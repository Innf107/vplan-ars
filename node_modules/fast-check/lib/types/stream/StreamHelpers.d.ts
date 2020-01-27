/** @hidden */
export declare function nilHelper<T>(): IterableIterator<T>;
/** @hidden */
export declare function mapHelper<T, U>(g: IterableIterator<T>, f: (v: T) => U): IterableIterator<U>;
/** @hidden */
export declare function flatMapHelper<T, U>(g: IterableIterator<T>, f: (v: T) => IterableIterator<U>): IterableIterator<U>;
/** @hidden */
export declare function filterHelper<T>(g: IterableIterator<T>, f: (v: T) => boolean): IterableIterator<T>;
/** @hidden */
export declare function takeWhileHelper<T>(g: IterableIterator<T>, f: (v: T) => boolean): IterableIterator<T>;
/** @hidden */
export declare function joinHelper<T>(g: IterableIterator<T>, others: IterableIterator<T>[]): IterableIterator<T>;
