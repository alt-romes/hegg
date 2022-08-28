# Revision history for hegg

## Unreleased

* Make `CostFunction` polymorphic over the `Cost` type, requiring that type
    to instance `Ord`
* Make e-graph abstract. The internal structure can still be modified through
    the available lenses in `Data.Equality.Graph.Lens`
* Fix a bug related to `NodeMap`'s size.

## 0.1.0.0 -- 2022-08-25

* First version. Released on an unsuspecting world.
