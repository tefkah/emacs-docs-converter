//import mdProcessor from '../htmlToMDX.js'
import fs from 'fs'
import path from 'path'
import remarkMdx from 'remark-mdx'
import mdxMetadata from 'remark-mdx-metadata'
import rehypeAttributes from 'rehype-attributes'

//export const converter = (dir) =>

import remarkGFM from 'remark-gfm'
import { reporter } from 'vfile-reporter'
import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'
import { visit } from 'unist-util-visit'

//const file = readSync('example.html')

const homedir = '../raw_manuals/elisp'

const dir = fs.readdirSync(homedir)

//const mdProcessor = (file) =>
const license =
  'This is the GNU Emacs Lisp Reference Manual\ncorresponding to Emacs version 27.2.\n\nCopyright (C) 1990-1996, 1998-2021 Free Software Foundation,\nInc.\n\nPermission is granted to copy, distribute and/or modify this document\nunder the terms of the GNU Free Documentation License, Version 1.3 or\nany later version published by the Free Software Foundation; with the\nInvariant Sections being "GNU General Public License," with the\nFront-Cover Texts being "A GNU Manual," and with the Back-Cover\nTexts as in (a) below.  A copy of the license is included in the\nsection entitled "GNU Free Documentation License."\n\n(a) The FSF\'s Back-Cover Text is: "You have the freedom to copy and\nmodify this GNU manual.  Buying copies from the FSF supports it in\ndeveloping GNU and promoting software freedom." '

const process = (file) =>
  unified()
    .use(rehypeParse, {
      emitParseErrors: true,
      duplicateAttribute: false,
    })

    .use(() => (tree) =>
      fs.writeFileSync('rehype', JSON.stringify(tree, null, 2))
    )
    // .use(rehypeAttributes, {
    //   pre: (node) => {
    //     node.properties.lang = 'lisp'
    //   },
    // })
    .use(rehypeRemark, {
      handlers: {
        comment: (h, node) => h(node, 'text', ''),
        //   code: (h, node) => h(node, 'element', )
      },
    })

    .use(() => (node) => {
      // visit from unist-util-visit
      visit(node, 'code', (element) => {
        element.lang = 'lisp'
        //  element.properties.lang = 'lisp'
      })
    })
    .use(remarkGFM)

    .use(remarkMdx)
    .use(() => (tree) =>
      fs.writeFileSync('test', JSON.stringify(tree, null, 2))
    )
    // .use(mdxMetadata, {
    //    meta: {
    //      license: license,
    //    },
    //  })
    .use(remarkStringify)

    .process(file)
///  .then((file) => {
//    console.error(reporter(file))
//    return String(file)
//  })

dir.forEach((file) => {
  const text = fs.readFileSync(`${homedir}/${file}`, { encoding: 'utf8' })
  // console.log(text)
  process(text).then((r) =>
    // console.log(x)
    fs.writeFileSync(`../pages/${path.basename(file, '.html')}.md`, String(r))
  )
})
