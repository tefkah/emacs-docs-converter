import { readSync } from 'to-vfile'
import { reporter } from 'vfile-reporter'
import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'

const file = readSync('example.html')

unified()
  .use(rehypeParse, { emitParseErrors: true, duplicateAttribute: false })
  .use(rehypeRemark)
  .use(remarkStringify)
  .process(file)
  .then((file) => {
    console.error(reporter(file))
    console.log(String(file))
  })
