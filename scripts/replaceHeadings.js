export default function replaceHeadings({ heading, headingObject }) {
  if (!headingObject[heading]) return heading
  console.log(headingObject[heading])
  return headingObject[heading]
}
