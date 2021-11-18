import { mdProcessor } from './htmlToMDX'
import fs from 'fs'
import path from 'path'

const homedir = 'raw_manuals/elisp'

const dir = fs.readdirSync(homedir)

const converter = (dir: string[]) =>
  dir.map((file) => {
    const text = fs.readFileSync(file, { encoding: 'utf8' })
    mdProcessor(text).then((res) => {
      fs.writeFileSync(`../pages/${path.basename(file, '.html')}.md`, res)
    })
  })

converter(dir)
