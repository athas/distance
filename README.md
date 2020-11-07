# Distance Functions for Futhark [![CI](https://github.com/athas/distance/workflows/CI/badge.svg)](https://github.com/athas/distance/actions) [![Documentation](https://futhark-lang.org/pkgs/github.com/athas/distance/status.svg)](https://futhark-lang.org/pkgs/github.com/athas/distance/latest/)

This package contains a variety of distance functions written in
[Futhark](https://futhark-lang.org).  They are heavily inspired by the
[corresponding functions in
SciPy](https://docs.scipy.org/doc/scipy/reference/spatial.distance.html).

## Installation

```
$ futhark pkg add github.com/athas/distance
$ futhark pkg sync
```

## Usage

```
$ futhark repl
> import "lib/github.com/athas/distance/distance"
> module vector = any_vector { let length = 2 }
> module euclidean32 = mk_euclidean f32 vector
> euclidean32.distance (vector.from_array [0,0]) (vector.from_array [1,1])
1.4142135f32
```
