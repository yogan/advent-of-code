export const main = async (filename: string) => solve(await parseInput(filename))

const parseInput = async (filename: string) =>
    (await Bun.file(filename).text()).split('\n').map((x) => x.split(/[,:] /))

const solve = (lines: string[][]) =>
    ['en', 'sv', 'nl']
        .map((lang) => sort(lines, lang))
        .map(getMiddlePhoneNumber)
        .reduce((acc, x) => acc * x, 1)

const getMiddlePhoneNumber = (lines: string[][]) =>
    parseInt(lines[Math.floor(lines.length / 2)][2], 10)

export const sort = (lines: string[][], lang: string) =>
    lines.toSorted((a, b) => {
        const nameA = lang === 'nl' ? normalizeLastName(a[0]) : a[0]
        const nameB = lang === 'nl' ? normalizeLastName(b[0]) : b[0]
        return nameA.localeCompare(nameB, lang, { ignorePunctuation: true })
    })

export const normalizeLastName = (name: string) => name.replace(/^\P{Lu}+/u, '')
