import express = require('express')
import path = require('path')

export function split<T>(a: T[], f: (x:T) => boolean) : [T[], T[]] {
    if(a.length === 0)
        return [[], []]
    const [x, ...xs] = a
    const [passed, failed] = split(xs, f)
    if(f(x))
        return [[x, ...passed], failed]
    else
        return [passed, [x, ...failed]]
}

export function mergeObjWith<T>(a: {[k:string]: T}, b: {[k:string]: T}, f: (x:T, y:T) => T): {[k:string]:T}{
    const entriesA = Object.entries(a)
    const entriesB = Object.entries(b)

    const mergedEntries = mergeByWith(entriesA, entriesB, 
        (x, y) => x[0] === y[0],
        ([xk, xv], [yk, yv]) => [xk, f(xv, yv)] as [string, T]
    )

    return objFromEntries(mergedEntries)
} 

export function mergeByWith<T>(a: T[], b: T[], match: (x: T, y:T) => boolean, resolve: (x: T, y:T) => T) : T[]{
    if(a[0] === undefined)
        return b
    if(b[0] === undefined)
        return a

    const [x, ...xs] = a
    const [matched, failed] = split(b, y => match(x, y))


    if(matched.length > 0){
        return [[x, ...matched].reduce((x, y) => resolve(x, y)), ...mergeByWith(xs, failed, match, resolve)]
    }
    else
        return [x, ...mergeByWith(xs, b, match, resolve)]
}

export const matchAll = (rgx: RegExp, str: string): RegExpMatchArray[] => {
    const match = str.match(rgx)
    if(match === null)
        return []
    else{
        const remaining = str.replace(match[0], '')
        return [match, ...matchAll(rgx, remaining)]
    }
}

export const wait = (time: number) => new Promise(resolve => setTimeout(() => resolve(time), time))

export function log<T>(msg: string, x: T): T{
    console.log(`${msg} ${x}`)
    return x
}

export function logOnly<T>(msg: string, x: T): T{
    console.log(msg)
    return x
}

export const staticFile = (_path: string) => (req, res) => res.sendFile(path.resolve(_path))

export function mapToObj<A, C>(list: A[], f: (x: A) => [string, C]) : {[key:string]:C}{
    if(list.length === 0)
        return {}
    const [x, ...xs] = list
    const [k, v] = f(x)
    return {[k]:v, ...mapToObj(xs, f)}
}

export function objFromEntries<V>(entries: [string, V][]){
    if(entries.length === 0)
        return {}
    const [[k, v], ...xs] = entries
    
    return {...{[k]:v}, ...objFromEntries(xs)}
}