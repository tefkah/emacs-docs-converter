//import mdProcessor from '../htmlToMDX.js'
import fs from 'fs'
import path from 'path'

//export const converter = (dir) =>

import { reporter } from 'vfile-reporter'
import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'

//const file = readSync('example.html')

const homedir = '../raw_manuals/elisp'

const dir = fs.readdirSync(homedir)

//const mdProcessor = (file) =>

const process = (file) =>
  unified()
    .use(rehypeParse, { emitParseErrors: true, duplicateAttribute: false })
    .use(rehypeRemark)
    .use(remarkStringify)
    .process(file)
///  .then((file) => {
//    console.error(reporter(file))
//    return String(file)
//  })

dir.forEach((file) => {
  const text = fs.readFileSync(`${homedir}/${file}`, { encoding: 'utf8' })
  console.log(text)
  process(text).then((r) =>
    // console.log(x)
    fs.writeFileSync(`../pages/${path.basename(file, '.html')}.md`, String(r))
  )
})
