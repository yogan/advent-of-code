import { expect, test } from 'bun:test'
import { isValid } from './i18n'

const authDb = new Map([
    ['etasche', '$2b$07$0EBrxS4iHy/aHAhqbX/ao.n7305WlMoEpHd42aGKsG21wlktUQtNu'],
    ['mpataki', '$2b$07$bVWtf3J7xLm5KfxMLOFLiu8Mq64jVhBfsAwPf8/xx4oc5aGBIIHxO'],
    ['ssatterfield', '$2b$07$MhVCvV3kZFr/Fbr/WCzuFOy./qPTyTVXrba/2XErj4EP3gdihyrum'],
    ['mvanvliet', '$2b$07$gf8oQwMqunzdg3aRhktAAeU721ZWgGJ9ZkQToeVw.GbUlJ4rWNBnS'],
    ['vbakos', '$2b$07$UYLaM1I0Hy/aHAhqbX/ao.c.VkkUaUYiKdBJW5PMuYyn5DJvn5C.W'],
    ['ltowne', '$2b$07$4F7o9sxNeaPe..........l1ZfgXdJdYtpfyyUYXN/HQA1lhpuldO'],
])

const isValidInDb = isValid.bind(null, authDb)

test('isValid', async () => {
    // NOTE: The two passwords show up identical, but are indeed not the same,
    //       due to different usage of combining characters.
    const pw1 = '.pM?XÑ0i7ÈÌ'
    const pw2 = '.pM?XÑ0i7ÈÌ'
    expect(pw1).not.toBe(pw2)

    expect(await isValidInDb('etasche', pw1)).toBe(true)
    expect(await isValidInDb('etasche', pw2)).toBe(true)
})

// vim: tw=90
