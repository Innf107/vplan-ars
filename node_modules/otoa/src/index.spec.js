/* global describe it */

const expect = require('chai').expect
const otoa = require('./')

describe('otoa', () => {
    it('otoa({i-0: 0, i-1: 1}, \'i-\'); should return an array.', () => {
        const obj = {
            'i-0': 0,
            'i-1': 1
        }
        const keyPrefix = 'i-'
        const actual = otoa(obj, keyPrefix)
        const expected = [
            0,
            1
        ]
        expect(typeof actual).to.equal(typeof [])
        expect(Array.isArray(actual)).to.equal(true)
        expect(actual[0] === obj[keyPrefix + 0] && actual[0] === 0).to.equal(true)
        expect(actual[1] === obj[keyPrefix + 1] && actual[1] === 1).to.equal(true)
        expect(actual[2] === obj[keyPrefix + 2] && actual[2] === undefined).to.equal(true)
        expect(actual).to.deep.equal(expected)
    })
})
