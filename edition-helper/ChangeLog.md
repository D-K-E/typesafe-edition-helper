# Changelog for edition-helper

## Unreleased changes

- Io is not yet done
- MVC architecture not completely implemented
- Unit and Container Model do not decompose to native structures
- UnitData and ContainerData do not decompose to native structures

## [0.2.0] - 2019-12-09

- Underlaying structure extended.
- A simple io function is implemented in Main.hs
- Tests are started
- ControlUtils start to have typeclasses for most common functionality

## Known

- Does not compile yet, some implementations in typeclass is preventing
  compilation. I might change the underlaying implementation for models, to
  cope with it.

## [0.1.0] - 2019-12-07

- New module structure conforming to mvc pattern
- Primitives also decompose to native data types like string through the use
  of type classes.
- 
