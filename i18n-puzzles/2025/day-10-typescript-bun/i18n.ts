import { combinatorics } from 'itertools-ts'

export async function main(filename: string) {
    const [authDb, loginAttempts] = await parseInput(filename)
    const isValidInDb = isValid.bind(null, authDb)

    const chunkSize = 250 // the chunk size is mysterious and important
    const chunks = Array.from(
        { length: Math.ceil(loginAttempts.length / chunkSize) },
        (_, i) => loginAttempts.slice(i * chunkSize, (i + 1) * chunkSize)
    )

    let validCount = 0

    for (const chunk of chunks) {
        const res = await Promise.all(
            chunk.map(async ([user, pw]) => await isValidInDb(user, pw))
        )
        validCount += res.reduce((acc, cur) => acc + Number(cur), 0)
    }

    return validCount
}

export async function isValid(
    authDb: Map<string, string>,
    user: string,
    pw: string
): Promise<boolean> {
    const hash = authDb.get(user)!
    for (const variant of getVariants(pw)) {
        if (await cachedVerify(user, variant, hash)) return true
    }
    return false
}

const vCache = new Map<string, boolean>()

async function cachedVerify(user: string, pw: string, hash: string) {
    const key = `${user}:${pw}`
    if (vCache.has(key)) return vCache.get(key)

    const valid = await Bun.password.verify(pw, hash, 'bcrypt')
    vCache.set(key, valid)
    return valid
}

function getVariants(pw: string): string[] {
    const vars = pw
        .normalize('NFC')
        .split('')
        .map((char) => [...new Set([char.normalize('NFC'), char.normalize('NFD')])])

    return [...combinatorics.cartesianProduct(...vars)].map((chars) => chars.join(''))
}

async function parseInput(
    filename: string
): Promise<[Map<string, string>, [string, string][]]> {
    const toPairList = (section: string) =>
        section.split('\n').map((line) => line.split(' ') as [string, string])

    const [top, bottom] = (await Bun.file(filename).text()).split('\n\n')
    return [new Map(toPairList(top)), toPairList(bottom)]
}
