const CACHE_NAME = 'vplan-cache'

self.addEventListener('install', ev => {
    console.log('Installed Service Worker!')
})

self.addEventListener('fetch', async ev => {
    console.log('req: ', ev.request)
    ev.respondWith(
        fetch(ev.request).then(res => {
            console.log('res', res)
            caches.open(CACHE_NAME)
                .then(cache => cache.add(ev.request))
            return res;
        }).catch(e => {
            if(/ping$/.test(ev.request.url)){
                console.log('failed to fetch ping!')
                return new Response('"false"')
            }
            console.log('failed to fetch', ev.request.url)
            return caches.match(ev.request)
        })
    )
})

self.addEventListener('activate', ev => {
    console.log('Sevice Worker activated!')
})

const purePromise = x => Promise.resolve(x)

const bindPromise = async (p, f) => await f(await p)

const mapPromise = async (p, f) => f(await p)


function str2ab(str) {
    var buf = new ArrayBuffer(str.length*2); // 2 bytes for each char
    var bufView = new Uint16Array(buf);
    for (var i=0, strLen=str.length; i < strLen; i++) {
      bufView[i] = str.charCodeAt(i);
    }
    return buf;
  }
  