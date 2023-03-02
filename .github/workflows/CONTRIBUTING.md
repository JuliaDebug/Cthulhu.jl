# Contributing

Welcome, and thanks for considering contributing to Cthulhu! Please be sure to respect our [community standards](https://julialang.org/community/standards) in all interactions. To contribute, you must also agree to [MIT license](LICENSE.md) your work.

The general [Julia CONTRIBUTING.md document](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) is a good general guide to contributing in the Julia ecosystem. The remainder of this document focuses on Cthulhu-specific details.

## Versioning and the TypedSyntax package

Cthulhu depends on a "sub-directory" package TypedSyntax. For developer convenience, on GitHub Actions we check out the same version of TypedSyntax that is in the tested branch. Consequently, if you're modifying the package in a way that requires coordinated changes to both packages, you can submit all these changes as a single pull request: no need to get the required changes to TypedSyntax merged & registered first.

However, there's an **important practice for both developers and reviewers**: if you are making coordinated changes, be sure to first bump the `TypedSyntax/Project.toml` version *and* the corresponding minimum required version in Cthulhu's `Project.toml` `[compat]` section. This will prevent a failure mode in which you register a new version of Cthulhu while forgetting that you also need to register a new version of TypedSyntax.
