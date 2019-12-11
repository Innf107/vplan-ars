import express = require('express')
import xcss = require('prophet-xcss')
import path = require('path')
import util = require('util')
import btoa = require('btoa')
import axios = require('axios')
import iso88592 = require('iso-8859-2')
import fs = require('fs-jetpack')
import Axios from 'axios'
const lite = require('./lite')
const app = express()
const PORT = 5000
const UPDATERATE = 600_000

let data: UntisData

const parsePlan = async (n: number): Promise<UntisData> => {
    const url = `https://vplan.ars-hochtaunus.de/subst_${n.toString().padStart(3, '0')}.htm`

    const res : axios.AxiosResponse<any> = await Axios.get(url, 
    {
        auth:{
            username:"vplan",password:"ars2013"
        },
        responseType: 'arraybuffer',
        responseEncoding: 'binary'
    } as any).catch(_ => null)

    if(res === null)
        return {vplan:[]}

    const data = iso88592.decode(res.data.toString('binary')) as string

    const refreshRgx = /<meta http-equiv="refresh" content="12; URL=subst_001.htm">/

    const dateRgx = /<div\s+class="mon_title">(.+?)( \(.+?)?<\/div>/
    const dateStr = data.match(dateRgx)[1]

    const tableRgx = /<table\s+class="mon_list"\s*>[^]+?<\/table>/
    const tableStr = data.match(tableRgx)[0]
    
    const klassen = matchKlassen(tableStr).map(match => {return{
        name: match[2],
        hours: matchHours(match[3])
    } as UntisKlasse})

    if(!klassen) 
        return {vplan:[]}
    
    const motdAffectedRowRgx = /<td class="info" align="left">([^B][^]+?)<\/td>/
    const motdAffected = data.match(motdAffectedRowRgx)[1].split(',').map(x => x.trim())
    const motdContentRowRgxG = /<td class='info' colspan="2">[^]+?<\/td>/g
    const motdContentRowRgx  = /<td class='info' colspan="2">([^]+?)<\/td>/
    const motdContentRows = data.match(motdContentRowRgxG)

    const motdContent = motdContentRows === null ? [] : motdContentRows.map(x => x.match(motdContentRowRgx)[1])

    const motd : UntisMOTD= {
        affected: motdAffected, 
        content: motdContent,
    }

    if(refreshRgx.test(data))
        return {
            vplan: [{
            day: dateStr,
            motd,
            klassen,
        } as UntisDay]}


    return {
        vplan: mergeData([{
            day: dateStr,
            motd,
            klassen,
        } as UntisDay], (await parsePlan(n + 1)).vplan)
    }
}  

const mergeData = (a: UntisDay[], b: UntisDay[]) : UntisDay[] => mergeByWith(a, b, 
    (x, y) => x.day === y.day, 
    (x, y) => {return {
        day: x.day,
        motd: y.motd,
        klassen: x.klassen.concat(y.klassen), //TODO: mergeKlassen(x.klassen, y.klassen)
    }}).sort(compareDates)

const compareDates = (x: UntisDay, y: UntisDay) => {
    const xDate = Date.parse(x.day.match(/[0-9.]+/)[0])
    const yDate = Date.parse(y.day.match(/[0-9.]+/)[0])
    return xDate - yDate
}


const mergeKlassen = (a: UntisKlasse[], b: UntisKlasse[]) : UntisKlasse[] => mergeByWith(a, b, 
    (x, y) => x.name === y.name,
    (x, y) => {return {
        name:x.name,
        hours: x.hours.concat(y.hours)
    }})

const matchKlassen = (tableStr: string) : RegExpMatchArray[] => {
    const klasseRgx1 = /(<tr\s+class=["']list .+?["']>\s*<td class=["']list inline_header["'].*?>[^]+?<\/td>\s*<\/tr>[^]+?)(?:(?:<tr\s+class=['"]list .+?['"]><td class=['"]list inline_header['"].*?>)|(?:<\/table>))/
    const klasseRgx  = /(<tr\s+class=["']list .+?["']>\s*<td class=["']list inline_header["'].*?>([^]+?)<\/td>\s*<\/tr>([^]+))/
    
    const match = tableStr.match(klasseRgx1)
    
    if(match == null)
        return []
    else
    {
        const remaining = tableStr.replace(match[1], '')
        return [match[1].match(klasseRgx), ...matchKlassen(remaining)]
    }
}

const matchHours = (childrenStr: string): UntisHour[] => {
    //                                                                         Stunde                                     Vertreter                                                Fach                                           Raum                                      Vertretungs-Text
//                                                                             1 - 2                                         DOB                                                   MAT                                            E10                                          Stattstunde
    const hourRgx  = /<tr class='list[^]*?'>\s*<td class="list" align="center">([^]+?)<\/td><td class="list" align="center">([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<\/tr>/

    const results = matchAll(hourRgx, childrenStr).map(match => {return {
        stunde: match[1],
        vertreter: match[2],
        fach: match[3],
        raum: match[4],
        vtext: match[5]
    }})
    return results
}

//API
app.use(xcss([`./public`]))


app.use('/lite', lite)
app.get('/pro', (req, res) => res.sendFile(__dirname + '/public/index.html'))
app.get('/main.css', (req, res) => res.sendFile(__dirname + '/public/main.css'))
app.get('/select.css', (req, res) => res.sendFile(__dirname + '/select.css'))
app.get('/', (req, res) => res.sendFile(__dirname + '/public/select.html'))

app.get('/json', (req, res) => {
    res.json(data || "Parsing...")
})

app.put('/feedback', (req, res) => {
    const date = new Date()
    const fileName = 'temp.txt'
    const filePath = `../data/Feedback`
    const file = fs.createWriteStream(`${filePath}/${fileName}`)
    file.on('open', _ => {
        req.on('data', x => file.write(x))
        req.on('end', async () => {
            file.end()
            const content = fs.read(`${filePath}/${fileName}`)
            await fs.renameAsync(`${filePath}/${fileName}`, content.replace(/\//g, '\\'))
            res.sendStatus(200)
        })
    })

})

app.get('/*', (req, res) => {
    res.sendFile(__dirname + '/public/404.html')
})

app.listen(PORT, () => console.log(`listening on Port ${PORT}`))


interface UntisHour {
    stunde: string,
    vertreter: string,
    fach: string,
    vtext: string,
}

interface UntisKlasse {
    name: string,
    hours: UntisHour[]
}

interface UntisMOTD{
    affected: string[],
    content: string[]
}

interface UntisDay {
    day: string,
    motd: UntisMOTD,
    klassen: UntisKlasse[]
}

interface UntisData {
    vplan: UntisDay[]
}

const update = async () => {
    data = await parsePlan(1)
    console.log(`updated at ${new Date()}`)
}

setInterval(update, UPDATERATE)
update()


//Helpers
const matchAll = (rgx: RegExp, str: string): RegExpMatchArray[] => {
    const match = str.match(rgx)
    if(match === null)
        return []
    else{
        const remaining = str.replace(match[0], '')
        return [match, ...matchAll(rgx, remaining)]
    }
}

const wait = (time: number) => new Promise(resolve => setTimeout(() => resolve(time), time))

function mergeByWith<T>(a: T[], b: T[], match: (x: T, y:T) => boolean, resolve: (x: T, y:T) => T) : T[]{
    if(a[0] === undefined)
        return b
    if(b[0] === undefined)
        return a

    const [x, ...xs] = a
    const [y, ...ys] = b

    if(match(x, y))
        return [resolve(x, y), ...mergeByWith(xs, ys, match, resolve)]
    else
        return [y, x, ...mergeByWith(xs, ys, match, resolve)]
}