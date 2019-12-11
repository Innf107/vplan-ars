'use strict'

const objectToArray = (obj, keyPrefix = '') => objectToArray.constraints(obj, keyPrefix) ? Object.keys(obj).map(x => x.replace(keyPrefix, '')).map(h => obj[keyPrefix + h]) : []

objectToArray.constraints = (obj, keyPrefix) => typeof obj === 'object' && Array.isArray(Object.keys(obj)) && typeof keyPrefix === 'string'

module.exports = objectToArray
