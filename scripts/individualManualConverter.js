//import mdProcessor from '../htmlToMDX.js'
import fs from 'fs'
import path from 'path'
import remarkMdx from 'remark-mdx'
import mdxMetadata from 'remark-mdx-metadata'
import rehypeAttributes from 'rehype-attributes'
import { selectAll } from 'unist-util-select'
import { toString } from 'hast-util-to-string'
//export const converter = (dir) =>

import remarkFrontmatter from 'remark-frontmatter'
import remarkGFM from 'remark-gfm'
import { reporter } from 'vfile-reporter'
import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'
import { visit } from 'unist-util-visit'

import { similarity } from './findSimilarity.js'
import { link } from 'fs/promises'
import { findAndReplace } from 'mdast-util-find-and-replace'

const homedir = '../raw_manuals/'

const dir = fs.readdirSync(homedir)

const license =
  'This is the GNU Emacs Lisp Reference Manual\ncorresponding to Emacs version 27.2.\n\nCopyright (C) 1990-1996, 1998-2021 Free Software Foundation,\nInc.\n\nPermission is granted to copy, distribute and/or modify this document\nunder the terms of the GNU Free Documentation License, Version 1.3 or\nany later version published by the Free Software Foundation; with the\nInvariant Sections being "GNU General Public License," with the\nFront-Cover Texts being "A GNU Manual," and with the Back-Cover\nTexts as in (a) below.  A copy of the license is included in the\nsection entitled "GNU Free Documentation License."\n\n(a) The FSF\'s Back-Cover Text is: "You have the freedom to copy and\nmodify this GNU manual.  Buying copies from the FSF supports it in\ndeveloping GNU and promoting software freedom." '

const rehypeProcessor = unified()
  // .use(remarkGFM)
  .use(rehypeRemark, {
    handlers: {
      comment: (h, node) => h(node, 'text', ''),
      footnoteReference: (h, node) => {
        console.log(node)
        return node
      },
      footnoteDefinition: (h, node) => node,
    },
  })

  .use(() => (node) => {
    visit(node, 'code', (element) => {
      element.lang = 'lisp'
    })
  })

  .use(remarkMdx)
  .use(remarkGFM)
  // .use(() => (node) => {
  //   visit(node, 'text', (text) => {
  //     if (text.value.includes('<') || text.value.includes('>')) {
  //       text.value = text.value.replaceAll(/</g, '\\<')
  //       text.value = text.value.replaceAll(/(?<!\\)>/g, '\\>')
  //     }
  //   })
  // })
  .use(() => (node) => {
    // visit from unist-util-visit
    visit(node, 'root', (root) => {
      const childs = root.children

      const newChilds = childs.flatMap((child) => {
        if (child.type !== 'list') return child

        if (child.ordered) return child
        const items = child.children

        const reworkedList = items.flatMap((listItem) => {
          const listItemContents = listItem?.children

          const heading = listItemContents?.[0]
          const headingWord = heading?.children?.[0]?.value

          if (!headingWord?.includes(':')) return listItem.children
          const [keyword, word] = headingWord
            .replaceAll(/(\w+):(.*?)/g, '$1@$2')
            .split('@')

          heading.children[0].value = word

          const mdxEl = {
            type: 'mdxJsxTextElement',
            name: 'span',
            attributes: [
              {
                type: 'mdxJsXAttribute',
                name: 'className',
                value: `tag ${keyword.toLowerCase().replaceAll(/ /g, '')}`,
              },
            ],
            children: [{ type: 'inlineCode', value: keyword.toLowerCase() }],
          }

          const kids = [mdxEl, ...heading.children]
          // if (heading?.type !== 'paragraph') return listItemContents

          const newHeading = Object.fromEntries([
            ['type', 'heading'],
            ['depth', 3],
            ['children', kids || []],
          ])

          return [newHeading, ...listItemContents.slice(1)]
        })

        return reworkedList
      })
      root.children = newChilds
    })
    // fix the last fucking mistakes
    findAndReplace(node, '”', '"')
    findAndReplace(node, '>', '﹥')
    findAndReplace(node, '<', '﹤')
  })
  .use(remarkStringify)

const singleFileProcessor = unified()
  .use(rehypeParse, {
    emitParseErrors: true,
    duplicateAttribute: false,
  })
  .use(() => (node) => {
    visit(node, 'element', (element) => {
      if (
        element.tagName === 'div' &&
        element.properties?.className?.includes('header')
      ) {
        element.children = []
      }
      //  element.properties.lang = 'lisp'
    })
  })

const dirs = ['org', 'elisp', 'emacs']
const dirContents = dirs.reduce((acc, dir) => {
  acc[dir] = JSON.parse(fs.readFileSync(`${dir}.json`, { encoding: 'utf8' }))
  return acc
}, {})

const bigFileProcessor = (f, dir) =>
  unified()
    .use(rehypeParse, {
      emitParseErrors: true,
      duplicateAttribute: false,
    })
    .use(() => (node) => {
      visit(node, 'element', (link) => {
        if (link.tagName !== 'a') return

        if (link.properties?.href?.includes('FOOT')) return
        if (link.properties?.href?.includes('https://')) return

        const cleanerLink = link.properties.href.replaceAll(/.*?#/g, '')

        // const comparedNames = (list, name) => {
        //   const unsorted = list.map((filename) => ({
        //     filename,
        //     sim: similarity(filename || '', name || ''),
        //   }))
        //   const sorted = unsorted.sort((a, b) => a.sim - b.sim)
        //   return sorted.reverse()
        // }

        // const goodLinks = () => {
        //   if (dirContents[dir].includes(cleanerLink)) return cleanerLink

        //   return comparedNames(dirContents[dir], cleanerLink)?.[0]?.filename
        // }
        //const newLink = goodLinks()
        // link.properties.href = newLink || cleanerLink
        link.properties.href = cleanerLink
      })
    })
    .use(() => (node) =>
      // Make all the h4s into h3s so the heading structure is better
      // and the headers show up on the side

      //also make tags for kbds
      visit(node, 'element', (heading) => {
        if (heading?.tagName !== 'h4') return
        heading.tagName = 'h3'
        if (dir === 'org') heading.tagName = 'h2'
      })
    )
    .use(() => (node) =>
      // Turn all lines which start with an inline code into a header, as these are usually
      // meant as headings
      // only really relevant for org files though, so we hard filter it
      {
        // if (dir !== 'org') return
        visit(node, 'element', (heading) => {
          if (heading?.tagName === 'dl') {
            heading.tagName = 'div'
            return
          }
          if (heading?.tagName === 'dd') {
            heading.tagName = 'div'
          }

          if (heading?.tagName === 'dt') {
            heading.tagName = 'h3'
            return
          }
        })
      }
    )
    // hmm yes very elegant solution
    // basically: loop through all the subnodes of the hast tree
    // chop up the tree in sections between "header" divs
    // parse, stringify and write those divs

    // i thought this would be easier than having to download all the individual files
    // from gnu, but it turns out this has more problems, such as needing to redo the links and footnotes
    // i 'solved" those but it's not ideal, for a second iteration i should just download all the files,
    // who cares if i ddos them a little
    .use(() => (node) => {
      let footNotes = {}
      visit(node, 'element', (heading) => {
        if (heading?.tagName !== 'div') return
        if (!heading?.properties?.className?.includes('footnote')) return

        const footNoteList = heading.children.reduce((acc, curr, index) => {
          if (curr.tagName !== 'h5') return acc

          const a = curr.children[0]
          // const thingy = toString(heading.children[index + 1])
          const thangy = heading.children[index + 2]
          //toString(heading.children[index + 1]) === '\n'
          // ?
          // : thingy
          acc[a.properties.id] = thangy
          return acc
        }, {})

        footNotes = footNoteList
      })

      //console.log(footNotes)
      visit(node, 'element', (bod) => {
        if (bod.properties?.id !== 'content') return
        let isCollecting = false
        let nodes = []
        let firstHeader = null
        let slugId = ''

        let alreadyDone = []
        // fs.writeFileSync('bod', JSON.stringify(bod, null, 2))
        const content = bod.children

        content.forEach((item, index) => {
          const isHeader =
            item.tagName === 'div' &&
            item.properties?.className?.includes('header')

          if (!isHeader) {
            if (!slugId) {
              console.log(content[index - 2])
              slugId = content[index - 2]?.properties?.id

              //   slugId && console.log(content[index - 2])
            }
            if (
              ['h1', 'h2', 'h3', 'h4', 'h5', 'h6'].includes(item.tagName) &&
              !firstHeader
            ) {
              firstHeader = item
              item.tagName = 'h2'
              const headerTitle = toString(item)
              const pref = getPrefix(headerTitle).prefix
              if (pref.length === 4) {
                nodes.push(item)
              }
              return
            }

            nodes.push(item)
            return
          }

          const newTreeBeforeFootNotes = {
            type: 'root',
            children: [
              {
                type: 'element',
                tagName: 'body',
                children: nodes,
              },
            ],
          }

          let footNoteCounter = 0
          let footNoteLinks = []

          visit(newTreeBeforeFootNotes, 'element', (footnote) => {
            const href = footnote?.properties?.href
            if (!href) return
            if (!href.includes('#FOOT')) return
            //console.log(footnote)

            footNoteLinks.push(href.replaceAll(/#/g, ''))
            footNoteCounter++

            // footnote.properties.href = `#f${footNoteCounter}`
            // footnote.properties.id = `l$${footNoteCounter}`
            footnote = {
              type: 'footnoteReference',
              identifier: `${footNoteCounter}`,
              label: `${footNoteCounter}`,
            }
          })

          //console.log(footNoteLinks)
          const feet =
            footNoteLinks.map((link, index) => {
              const textnode = [footNotes[link]]

              return {
                type: 'element',
                tagName: 'footnoteDefinition',
                children: textnode,
                properties: { id: `f${index}` },
              }
            }) || []
          //console.log(feet)
          const footsies = feet?.children?.length
            ? [...newTreeBeforeFootNotes.children, feet]
            : [...newTreeBeforeFootNotes.children]
          const newTree = {
            ...newTreeBeforeFootNotes,
            children: footsies,
          }

          const title = firstHeader ? toString(firstHeader) : 'empty'
          isCollecting = false
          if (!firstHeader) return

          //slugId = content[index - 1]?.properties?.id

          //paste footnotes at the bottom of the files

          nodes = []
          firstHeader = null

          if (title === 'empty') return

          if (alreadyDone.includes(title)) return
          alreadyDone.push(title)

          let cleanTitle = title.replaceAll(/\?/g, '')
          cleanTitle = cleanTitle.replaceAll(/\//g, ' and ')
          cleanTitle = cleanTitle.replaceAll(/\%/g, 'precentage')
          cleanTitle = cleanTitle.replaceAll(/Appendix /g, '')
          const { prefix, title: formattedTitle } = getPrefix(cleanTitle)
          const titles = formattedTitle || cleanTitle

          let newSlug = slugId
          slugId = ''

          rehypeProcessor.run(newTree).then((f) => {
            const rawFile = rehypeProcessor.stringify(f)
            const formattedTitleWithDashes = titles?.replaceAll(/ /g, '-')
            const fileWithMetadata = `---\nslug: ${
              newSlug || formattedTitleWithDashes
            }\n---\n\n${String(rawFile)}`

            if (prefix.length > 3) {
              const directory = fs.readdirSync(dir)
              const ogFile = directory.find((file) => {
                const filePrefix = getPrefix(file).prefix
                return (
                  filePrefix[0] === prefix[0] &&
                  filePrefix[1] === prefix[1] &&
                  filePrefix[2] === prefix[2]
                )
              })
              ogFile && fs.appendFileSync(`${dir}/${ogFile}`, rawFile)
              return
            }

            fs.writeFileSync(`${dir}/${cleanTitle}.md`, fileWithMetadata)
          })
        })
      })
    })

const big = fs.readdirSync(`../raw_manuals`)

big.forEach((filepath) => {
  const file = fs.readFileSync(`../raw_manuals/${filepath}`, {
    encoding: 'utf8',
  })

  const tree = bigFileProcessor().parse(file)

  const dir = `${path.basename(filepath, '.html')}`

  const newProc = bigFileProcessor(tree, dir)
  newProc.data('type', dir)

  fs.existsSync(dir) || fs.mkdirSync(dir)

  newProc.run(tree).then((r) => {})
})

export function getPrefix(str) {
  const [pref, title] = str
    .replaceAll(/(\w+\/)?([A-H\d\.]+) (.*?)/g, '$2@$3')
    .split('@')

  return { prefix: pref.split('.'), title }
}

//module.exports = { getPrefix }
