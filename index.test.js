const server = require('./index')

const randomData = () => {
    // 0 - 20 days
    let len = Math.round(Math.random() * 20)

    return {
        vplan:{}
    }
}

describe('mergeData', () => {
    let rnd
    beforeEach(() => {
        rnd = randomData()
    })
    test('mergeData(x, {}) === x', () => {
        expect(server.mergeData(rnd, {})).toEqual(rnd)
        expect(server.mergeData({},rnd)).toEqual(rnd)
    })

    /*
    test('mergeData(null, x) fails', () => {
        const rnd = randomData()
        expect(server.mergeData(null, rnd)).toThrow()
        expect(server.mergeData(rnd, null)).toThrow()
    })*/
})
