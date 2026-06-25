# Architecture

## Directory Layout
```
.
├── agents              skills for use with AI agents
│   └── skills
├── assets              the Virdant logos and non-code files
├── docs                the Virdant website
├── lib                 the Virdant standard library
├── tests               Virdant tests
│   ├── pass                ... which are expected to pass
│   ├── warnings            ... which are expected to pass with a warning
│   ├── errors              ... which are expected to fail
│   ├── virdoc              ... which sanity-check `vir doc`
│   └── virformat           ... which sanity-check `vir format`
└── virdant             the Virdant compiler
```

## Outside the Repository

[Tree-Sitter-Virdant](https://github.com/virdant-lang/tree-sitter-virdant) - tree-sitter support for Virdant
[https://github.com/virdant-lang/virdant.nvim](https://github.com/virdant-lang/virdant.nvim) - Virdant support for NeoVim
