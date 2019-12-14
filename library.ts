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