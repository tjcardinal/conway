import itertools
import time
from functools import partial
from typing import Callable, Set, Tuple

import pygame

PIXEL_SIZE = 10

Point = Tuple[int, int]
Points = Set[Point]

def neighbors(point: Point) -> Points:
    x,y = point
    x_range = range(x-1, x+2)
    y_range = range(y-1, y+2)
    return set([(nx,ny) for nx in x_range for ny in y_range if nx != x or ny != y])

def points_with_neighbors(points: Points) -> Points:
    return points.union(itertools.chain.from_iterable(map(neighbors, points)))

def survive_x_spawn_y(x: int, y: int, points: Points, point: Point) -> bool:
    count = len(points.intersection(neighbors(point)))
    return (count == x and (point in points)) or count == y

def next(cond: Callable[[Points, Point], bool], points: Points) -> Points:
    f = partial(cond, points)
    return set(filter(f, points_with_neighbors(points)))

if __name__ == "__main__":
    pygame.init()
    screen = pygame.display.set_mode([200, 200])

    points = set([
        (2,2),
        (2,3),
        (2,4),
        (3,1),
        (3,2),
        (3,3)
        ])
    survive_2_spawn_3 = partial(survive_x_spawn_y, 2, 3)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        screen.fill((255, 255, 255))

        for point in points:
            x,y = point
            rectangle = pygame.Rect(x * PIXEL_SIZE, y * PIXEL_SIZE, PIXEL_SIZE, PIXEL_SIZE)
            pygame.draw.rect(screen, (0, 0, 0), rectangle)

        points = next(survive_2_spawn_3, points)

        pygame.display.flip()
        time.sleep(1)
    
    pygame.quit()
