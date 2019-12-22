const CACHE_NAME = 'vplan-cache'
const urlsToCache = [
    '/pro',
    '/personal',
    'teacher',
    '/index.js',
    '/teacher.js',
    '/personal.js',
    '/main.css',
]

self.addEventListener('install', ev => {
    console.log('Installed Service Worker!')
    ev.waitUntil(
        caches.open(CACHE_NAME)
            .then(cache => {
                console.log(`opened cache ${cache}`)
                return cache.addAll(urlsToCache)
            })
    )
})

self.addEventListener('fetch', async ev => {
    console.log('req: ', ev.request)
    ev.respondWith(
        fetch(ev.request).then(res => {
            console.log('res', res)
            if(res){
                caches.open(CACHE_NAME)
                    .then(cache => cache.add(ev.request))
                return res;
            }
            else
                return caches.match(ev.request)
        }).catch(e => {
            console.log('failed to fetch', ev.request.url)
            return caches.match(ev.request)
        })
    )
})

self.addEventListener('activate', ev => {
    console.log('Sevice Worker activated!')
})