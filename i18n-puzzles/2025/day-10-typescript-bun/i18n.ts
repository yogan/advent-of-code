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

const cache = new Map<string, string[]>()

export async function isValid(
    authDb: Map<string, string>,
    user: string,
    pw: string
): Promise<boolean> {
    if (cache.has(user)) return cache.get(user)!.includes(pw)

    const hash = authDb.get(user)!
    const pwVariants = getVariants(pw)
    for (const variant of pwVariants) {
        if (await Bun.password.verify(variant, hash, 'bcrypt')) {
            cache.set(user, pwVariants)
            return true
        }
    }
    return false
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
