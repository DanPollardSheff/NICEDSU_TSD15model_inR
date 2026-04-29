\# Contributing to NICEDSU\_TSD15model\_inR



Thank you for your interest in contributing to this health economic simulation model. To keep the model robust and accurate, please follow these guidelines.



\## 1. Code Style and Conventions

\* \*\*Vectorisation\*\*: All new functions should be vectorised. Avoid `for` loops when applying calculations to patient matrices; use logical masking instead.

\* \*\*Documentation\*\*: Every function must follow the established ROxygen2 documentation header, specifying input types (e.g., matrix, numeric vector) and returned objects.

\* \*\*Naming\*\*: Please use the `\_` suffix for function inputs but not for values calculated within functions to maintain consistency with the existing codebase (e.g., `pat\_chars\_`, `GlobalOptions\_`).



\## 2. Development Workflow

\* \*\*Modular Design\*\*: If adding new event types, create a new `\*Vec` function following the pattern of `HipFractureVec` and `VertFractureVec`.

\* \*\*Parallel Safety\*\*: Remember that PSA runs in parallel using `parSapply`. Any new global variables or dependencies must be compatible with `clusterExport`.

\* \*\*Testing\*\*: Ensure any changes to the core engine are verified against the deterministic baseline before running PSA iterations.



\## 3. Pull Request Process

1\. \*\*Branching\*\*: Create a feature branch for your changes.

2\. \*\*Documentation\*\*: Update the `README.md` or the GitHub wiki if your contribution changes how the simulation flow works.



\## 4. Reporting Issues

When reporting a bug, please include:

\* A minimal reproducible example.

\* The structure of your `pat\_chars\_` and `GlobalOptions\_` objects.

\* Whether the issue persists in deterministic mode or only during PSA.

