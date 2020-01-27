import { Stream } from '../../../stream/Stream';
/**
 * A Shrinkable<T> holds an internal value of type `T`
 * and can shrink it to smaller `T` values
 */
export declare class Shrinkable<T> {
    readonly value_: T;
    readonly shrink: () => Stream<Shrinkable<T>>;
    /**
     * State storing the result of hasCloneMethod
     * If <true> the value will be cloned each time it gets accessed
     */
    readonly hasToBeCloned: boolean;
    /**
     * Flag indicating whether or not the this.value has already been called once
     * If so, the underlying will be cloned
     * Only set when hasToBeCloned = true
     */
    private readOnce;
    /**
     * Safe value of the shrinkable
     * Depending on {@link hasToBeCloned} it will either be {@link value_} or a clone of it
     */
    readonly value: T;
    /**
     * @param value Internal value of the shrinkable
     * @param shrink Function producing Stream of shrinks associated to value
     */
    constructor(value_: T, shrink?: () => Stream<Shrinkable<T>>);
    /** @hidden */
    private getValue;
    /** @hidden */
    private applyMapper;
    /**
     * Create another shrinkable by mapping all values using the provided `mapper`
     * Both the original value and the shrunk ones are impacted
     *
     * @param mapper Map function, to produce a new element based on an old one
     * @returns New shrinkable with mapped elements
     */
    map<U>(mapper: (t: T) => U): Shrinkable<U>;
    /**
     * Create another shrinkable
     * by filtering its shrunk values against `predicate`
     *
     * All the shrunk values produced by the resulting `Shrinkable<T>`
     * satisfy `predicate(value) == true`
     *
     * @param predicate Predicate, to test each produced element. Return true to keep the element, false otherwise
     * @returns New shrinkable filtered using predicate
     */
    filter(predicate: (t: T) => boolean): Shrinkable<T>;
}
