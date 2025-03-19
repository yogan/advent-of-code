export const main = async (filename: string) => solve(await parseInput(filename))

const parseInput = async (filename: string) =>
    (await Bun.file(filename).text()).split('\n').map((x) => x.split(/[,:] /))

const solve = (lines: string[][]) =>
    ['en', 'sv', 'nl']
        .map((lang) => sort(lines, lang))
        .map((lines: string[][]) => parseInt(lines[Math.floor(lines.length / 2)][2]))
        .reduce((acc, x) => acc * x, 1)

export const sort = (lines: string[][], lang: string) =>
    lines
        .map(([name, ...rest]) => [lang === 'nl' ? stripInfixes(name) : name, ...rest])
        .toSorted(([a], [b]) => a.localeCompare(b, lang, { ignorePunctuation: true }))

export const stripInfixes = (name: string) => name.replace(/^\P{Lu}+/u, '')
