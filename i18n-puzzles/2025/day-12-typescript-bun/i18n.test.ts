import { expect, test } from 'bun:test'
import { stripInfixes, sort } from './i18n'

const sample = [
    ['Ñíguez Peña', 'María de los Ángeles', '0151605'],
    ['Åberg', 'Rosa-Maria', '0110966'],
    ['Öztürk', 'Zeynep', '0185292'],
    ['van den Heyden', 'Harm', '0168131'],
    ['Ämtler', 'Lorena', '0112717'],
    ['Olofsson', 'Mikael', '0103652'],
    ['van Leeuwen', 'Joke', '0172199'],
    ['Vandersteen', 'Willy', '0120659'],
    ['Østergård', 'Magnus', '0113959'],
    ['van Leeuw', 'Floor', '0144158'],
    ['Navarrete Ortiz', 'Dolores', '0119411'],
    ['Aalto', 'Alvar', '0192872'],
    ['Zondervan', 'Jan Peter', '0103008'],
    ['Æbelø', 'Aurora', '0113267'],
    ["O'Neill", 'Cara', '0109551'],
]

test('English sorting', () => {
    expect(sort(sample, 'en')).toEqual([
        ['Aalto', 'Alvar', '0192872'],
        ['Åberg', 'Rosa-Maria', '0110966'],
        ['Æbelø', 'Aurora', '0113267'],
        ['Ämtler', 'Lorena', '0112717'],
        ['Navarrete Ortiz', 'Dolores', '0119411'],
        ['Ñíguez Peña', 'María de los Ángeles', '0151605'],
        ['Olofsson', 'Mikael', '0103652'],
        ["O'Neill", 'Cara', '0109551'],
        ['Østergård', 'Magnus', '0113959'],
        ['Öztürk', 'Zeynep', '0185292'],
        ['van den Heyden', 'Harm', '0168131'],
        ['Vandersteen', 'Willy', '0120659'],
        ['van Leeuw', 'Floor', '0144158'],
        ['van Leeuwen', 'Joke', '0172199'],
        ['Zondervan', 'Jan Peter', '0103008'],
    ])
})

test('Swedish sorting', () => {
    expect(sort(sample, 'sv')).toEqual([
        ['Aalto', 'Alvar', '0192872'],
        ['Navarrete Ortiz', 'Dolores', '0119411'],
        ['Ñíguez Peña', 'María de los Ángeles', '0151605'],
        ['Olofsson', 'Mikael', '0103652'],
        ["O'Neill", 'Cara', '0109551'],
        ['van den Heyden', 'Harm', '0168131'],
        ['Vandersteen', 'Willy', '0120659'],
        ['van Leeuw', 'Floor', '0144158'],
        ['van Leeuwen', 'Joke', '0172199'],
        ['Zondervan', 'Jan Peter', '0103008'],
        ['Åberg', 'Rosa-Maria', '0110966'],
        ['Æbelø', 'Aurora', '0113267'],
        ['Ämtler', 'Lorena', '0112717'],
        ['Østergård', 'Magnus', '0113959'],
        ['Öztürk', 'Zeynep', '0185292'],
    ])
})

test('Dutch sorting', () => {
    expect(sort(sample, 'nl')).toEqual([
        ['Aalto', 'Alvar', '0192872'],
        ['Åberg', 'Rosa-Maria', '0110966'],
        ['Æbelø', 'Aurora', '0113267'],
        ['Ämtler', 'Lorena', '0112717'],
        ['Heyden', 'Harm', '0168131'],
        ['Leeuw', 'Floor', '0144158'],
        ['Leeuwen', 'Joke', '0172199'],
        ['Navarrete Ortiz', 'Dolores', '0119411'],
        ['Ñíguez Peña', 'María de los Ángeles', '0151605'],
        ['Olofsson', 'Mikael', '0103652'],
        ["O'Neill", 'Cara', '0109551'],
        ['Østergård', 'Magnus', '0113959'],
        ['Öztürk', 'Zeynep', '0185292'],
        ['Vandersteen', 'Willy', '0120659'],
        ['Zondervan', 'Jan Peter', '0103008'],
    ])
})

test('Normalize last name', () => {
    expect(stripInfixes('van den Heyden')).toBe('Heyden')
    expect(stripInfixes('van Leeuwen')).toBe('Leeuwen')
    expect(stripInfixes('Vandersteen')).toBe('Vandersteen')
    expect(stripInfixes('Ñíguez Peña')).toBe('Ñíguez Peña')
})
