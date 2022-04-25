import itertools
from typing import Callable, Set, Tuple
from functools import partial

Point = Tuple[int, int]
Points = Set[Point]

def neighbors(point: Point) -> Points:
    x,y = point
    x_range = range(start=x-1, stop=x+1)
    y_range = range(start=y-1, stop=y+1)
    return set([(nx,ny) for nx in x_range for ny in y_range if nx != x or ny != y])

def points_with_neighbors(points: Points) -> Points:
    return points.union(itertools.chain.from_iterable(map(neighbors, points)))

def survive_x_spawn_y(x: int, y: int, points: Points, point: Point) -> bool:
    count = len(points.intersection(neighbors(point)))
    return (count == x and (point in points)) or count == y

def next(cond: Callable[[Points, Point], bool], points: Points) -> Points:
    f = partial(cond, points)
    filter(f, points_with_neighbors(points))