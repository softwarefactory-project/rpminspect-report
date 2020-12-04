# rpminspect-report

## Setup

Install yarnpkg and nodejs: `sudo dnf install -y nodejs yarnpkg`

Then setup the project:

```
git clone https://softwarefactory-project.io/r/software-factory/re-patternfly ../re-patternfly
yarn install
yarn start
```

Run test with:

```
yarn test
```

Setup live hot-reload:

```
yarn serve
# Open browser on http://localhost:1234
```

Distribute with:

```
yarn dist
# Open browser in dist/
```
