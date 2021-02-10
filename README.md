# rpminspect-report

## Setup

Install yarnpkg and nodejs: `sudo dnf install -y nodejs yarnpkg`

Then setup the project:

```
git clone https://softwarefactory-project.io/r/software-factory/re-patternfly ../re-patternfly
pnpm install --shamefully-hoist
pnpm start
```

Run test with:

```
pnpm test
```

Setup live hot-reload:

```
pnpm serve
# Open browser on http://localhost:1234
```

Distribute with:

```
pnpm dist
# Open browser in dist/
```
