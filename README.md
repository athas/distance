# Distance Functions for Futhark

This package contains a variety of distance functions written in
[Futhark](https://futhark-lang.org).  They are heavily inspired by the
[corresponding functions in
SciPy](https://docs.scipy.org/doc/scipy/reference/spatial.distance.html).

## Installation

```
$ futhark-pkg add github.com/athas/distance
$ futhark-pkg sync
```

## Usage

```
$ futharki
> import "lib/github.com/athas/distance/distance"
> module euclidean32 = mk_euclidean' f32
> euclidean32.distance [0,0] [1,1]
1.4142135f32
```
