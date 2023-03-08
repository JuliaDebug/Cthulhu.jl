# Contributing

Welcome, and thanks for considering contributing to Cthulhu! Please be sure to respect our [community standards](https://julialang.org/community/standards) in all interactions. To contribute, you must also agree to [MIT license](LICENSE.md) your work.

The general [Julia CONTRIBUTING.md document](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) is a good general guide to contributing in the Julia ecosystem. The remainder of this document focuses on Cthulhu-specific details.

## Filing bug reports

Cthulhu is an interactive package. While it's possible to submit a bug report that reads like

> `@descend` into `foo(8)`, then select the third item from the call menu, then the fourth from that call sub-menu, then the fifth from the bottom in the third menu...

you might imagine that this could be hard for the developers to replicate, especially if they are using different Julia and/or package versions from you. It's much more helpful if you note the (typed) signature of the method that either directly causes the error or immediately preceeds it: then you should be able to replicate the problem with `descend(bar, (types...))`. Once you've verified that a much shorter sequence of steps can trigger the same error, then you're ready to submit the bug report.

Also, please note any settings you're using: are you looking at the Source view? The Typed view? Is `warn` highlighting on? These may also be helpful to replicate the problem.

## Versioning and the TypedSyntax package

Cthulhu depends on a "sub-directory" package TypedSyntax. For developer convenience, on GitHub Actions we check out the same version of TypedSyntax that is in the tested branch. Consequently, if you're modifying the package in a way that requires coordinated changes to both packages, you can submit all these changes as a single pull request: no need to get the required changes to TypedSyntax merged & registered first.

However, there's an **important practice for both developers and reviewers**: if you are making coordinated changes, be sure to first bump the `TypedSyntax/Project.toml` version *and* the corresponding minimum required version in Cthulhu's `Project.toml` `[compat]` section. This will prevent a failure mode in which you register a new version of Cthulhu while forgetting that you also need to register a new version of TypedSyntax.
