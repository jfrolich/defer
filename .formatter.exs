# Used by "mix format"
[
  locals_without_parens: [defer: 2, await: 1],
  inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}"],
  export: [
    [
      locals_without_parens: [defer: 2, await: 1]
    ]
  ]
]