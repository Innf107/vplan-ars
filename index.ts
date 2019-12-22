import express = require('express')
import xcss = require('prophet-xcss')
import path = require('path')
import util = require('util')
import btoa = require('btoa')
import axios = require('axios')
import iso88592 = require('iso-8859-2')
import fs = require('fs-jetpack')
import Axios from 'axios'
import http =require('http')
import https = require('https')
import sanitize = require('sanitize-filename')
import {split, mergeByWith, matchAll, wait, log, logOnly, staticFile} from './library'
import userLog = require('./userLog')
const app = express()
const HTTPPORT = 5000
const HTTPSPORT = 5001
const UPDATERATE = 590_000

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
        return log("resIsNull: ", {vplan:[]})

    const data = iso88592.decode(res.data.toString('binary')) as string

    const dateRgx = /<div\s+class="mon_title">(.+?)( \(.+?)?<\/div>/
    const dateStr = data.match(dateRgx)[1]

    const tableRgx = /<table\s+class="mon_list"\s*>[^]+?<\/table>/
    const tableStr = data.match(tableRgx)[0]
    
    const klassen = matchKlassen(tableStr).map(match => {return{
        name: match[2],
        hours: matchHours(match[3])
    } as UntisKlasse})

    if(!klassen)
        return log("!klassen", {vplan:[]})

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

    const refreshRgx = /<meta http-equiv="refresh" content="12; URL=subst_001.htm">/
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
//                                                                             Stunde                                     Vertreter                                                Fach                                           Raum                                      Vertretungs-Text
//                                                                             1 - 2                                         DOB                                                   MAT                                            E10                                          Stattstunde
    const hourRgx  = /<tr class='list[^]*?'>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list"(?: align="center")>([^]+?)<\/td>\s*<\/tr>/

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

let totalUsers = 0

app.use(userLog((reqPath, lastVisit) => {
    if(lastVisit === null)
        totalUsers++;
}))

app.get('/pro', staticFile('public/index.html'))
app.get('/beta', staticFile('public/beta.html'))
app.get('/main.css', staticFile('public/main.css'))
app.get('/select.css', staticFile('select.css'))
app.get('/teacher.css', staticFile('teacher.css'))
app.get('/', staticFile('public/select.html'))
app.get('/teacher', staticFile('public/teacher.html'))
app.get('/robots.txt', staticFile('robots.txt'))
app.get('/index.js', staticFile('public/index.js'))
app.get('/teacher.js', staticFile('public/teacher.js'))
app.get('/personal', staticFile('public/personal.html'))
app.get('/personal.js', staticFile('public/personal.js'))
app.get('/sw.js', staticFile('public/sw.js'))
app.get('/manifest.webmanifest', staticFile('manifest.webmanifest'))
app.get('/logoMain.png', staticFile('logoMain.png'))
app.get('/usersTotal', (req, res) => res.send(totalUsers.toString()))
app.get('/json/kuerzel', (req, res) => {
    var json = JSON.parse(fs.read('kuerzel.json'))
    res.send(Object.keys(json).map(k => {return {key:k, value:json[k]}}))
})

app.get('/json', (req, res) => {
    res.json(data || "Parsing...")
})

app.put('/feedback', (req, res) => {
    const fileName = 'temp.txt'
    const filePath = `../data/Feedback`
    const file = fs.createWriteStream(`${filePath}/${fileName}`)
    file.on('open', _ => {
        req.on('data', x => file.write(x))
        req.on('end', () => {
            file.end()
            const content = fs.read(`${filePath}/${fileName}`)
            fs.renameAsync(`${filePath}/${fileName}`, sanitize(content.replace(/\//g, '\\')))
            res.sendStatus(200)
        })
    })

})

app.get('/*', (req, res) => {
    res.status(404).sendFile(__dirname + '/public/404.html')
})
try {
    const privateKey = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/privkey.pem', 'utf8') || logOnly("private key not found for vplan-ars.spdns.de. trying localhost...", fs.read('localhost-key.pem', 'utf8')) || logOnly('private key for localhost does not exist either!', undefined)
    const certificate = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/cert.pem', 'utf8') || logOnly("certificate not found for vplan-ars.spdns.de. trying localhost...", fs.read('localhost.pem', 'utf8')) || logOnly('certificate for localhost does not exist either!', undefined)
    const ca = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/chain.pem', 'utf8')

    const credentials = {
        key: privateKey,
        cert: certificate,
        //ca
    }

    if(!privateKey || !certificate)
        throw new Error("credentials do not exist!")

    const httpsServer = https.createServer(credentials, app)
    httpsServer.listen(HTTPSPORT, () => console.log(`HTTPS Server listening on port ${HTTPSPORT}`))
    
    const httpserver = express()
    httpserver.get('*', (req, res) => res.redirect(`https://${req.headers.host}${req.url}`))
    httpserver.listen(HTTPPORT, () => console.log(`HTTP Server listening on Port ${HTTPPORT}`))
}
catch{
    const httpserver = http.createServer(app)
    httpserver.listen(HTTPPORT, () => console.log(`HTTP Server listening on Port ${HTTPPORT}`))
    console.log('cannot create HTTPS Server')
}



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
    console.log(`updated vplan at ${new Date()}`)

    fs.write('../data/totalUsers', totalUsers.toString())
}

fs.readAsync('../data/totalUsers').then(s => totalUsers = Number.parseInt(s))
setInterval(update, UPDATERATE)
update()
