## Converter functions for Emacs Docs

These are the converter functions used by [Emacs Docs](https://github.com/ThomasFKJorna/emacs-docs) to convert the HTML manuals to Markdown, totally non-optimized atm and extremely jank.

## Usage

1. `git clone`
2. `yarn`
3. Put single-page HTML manuals in raw_manuals
4. `cd scripts`
5. `node converter.js`

Should output the docs in the scripts directory.

## TODO

- [ ] Rewrite to TS. Actually started doing this but couldnt figure out TS-node
- [ ] Make converter.js take commandline arguments, maybe just use Commander
      = [ ] Find a way to include it in the main docs. this probably involves making the main Emacs Docs repo a monorepo with these being one of the packages, or some other way which allows you to include a module easily, maybe package this as a module (what an idea)
- [ ] Find a better way to resolve file links. Probably need to parse the individual pages instead of the big guy for this.
- [ ] Find a better way to include indices
