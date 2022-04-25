use std::collections::HashSet;

pub type Coordinate = u16;
pub type Point = (Coordinate, Coordinate);
pub type PointSet = HashSet<Point>;

pub struct Conway {
    pub current: PointSet,
}

impl From<PointSet> for Conway {
    fn from(set: PointSet) -> Conway {
        Conway { current: set }
    }
}

impl Iterator for Conway {
    type Item = PointSet;

    fn next(&mut self) -> Option<Self::Item> {
        self.current = get_points_to_calc(&self.current)
            .into_iter()
            .filter(|p| is_alive(p, &self.current))
            .collect();
        Some(self.current.clone())
    }
}

fn get_points_to_calc(set: &PointSet) -> PointSet {
    let neighbors = set.iter().map(get_neighbors).flatten().collect();
    set.union(&neighbors).copied().collect()
}

fn is_alive(point: &Point, set: &PointSet) -> bool {
    let neighbor_count = get_neighbors(point)
        .into_iter()
        .filter(|n| set.contains(n))
        .count();
    match neighbor_count {
        2 if set.contains(point) => true,
        3 => true,
        _ => false,
    }
}

fn get_neighbors(point: &Point) -> PointSet {
    let (x, y) = *point;
    vec![
        (x.wrapping_sub(1), y.wrapping_sub(1)),
        (x.wrapping_sub(1), y),
        (x.wrapping_sub(1), y.wrapping_add(1)),
        (x, y.wrapping_sub(1)),
        (x, y.wrapping_add(1)),
        (x.wrapping_add(1), y.wrapping_sub(1)),
        (x.wrapping_add(1), y),
        (x.wrapping_add(1), y.wrapping_add(1)),
    ]
    .into_iter()
    .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from() {
        let input: PointSet = vec![(1, 1), (1, 2), (2, 1), (2, 2)].into_iter().collect();
        let conway = Conway::from(input.clone());
        assert_eq!(input, conway.current);
    }

    #[test]
    fn block() {
        let start: PointSet = vec![(1, 1), (1, 2), (2, 1), (2, 2)].into_iter().collect();
        let mut conway: Conway = start.clone().into();
        assert_eq!(start, conway.next().unwrap());
    }

    #[test]
    fn tub() {
        let start: PointSet = vec![(1, 2), (2, 1), (2, 3), (3, 2)].into_iter().collect();
        let mut conway: Conway = start.clone().into();
        assert_eq!(start, conway.next().unwrap());
    }

    #[test]
    fn blinker() {
        let start: PointSet = vec![(2, 1), (2, 2), (2, 3)].into_iter().collect();
        let mut conway: Conway = start.clone().into();
        let first: PointSet = vec![(1, 2), (2, 2), (3, 2)].into_iter().collect();
        assert_eq!(first, conway.next().unwrap());
        assert_eq!(start, conway.next().unwrap());
    }

    #[test]
    fn toad() {
        let start: PointSet = vec![(2, 2), (2, 3), (2, 4), (3, 1), (3, 2), (3, 3)]
            .into_iter()
            .collect();
        let mut conway: Conway = start.clone().into();
        let first: PointSet = vec![(1, 3), (2, 1), (2, 4), (3, 1), (3, 4), (4, 2)]
            .into_iter()
            .collect();
        assert_eq!(first, conway.next().unwrap());
        assert_eq!(start, conway.next().unwrap());
    }

    #[test]
    fn overflow_wrapping() {
        let x_max = Coordinate::MAX;
        let x_min = Coordinate::MIN;
        let y_max = Coordinate::MAX;
        let y_min = Coordinate::MIN;
        let start: PointSet = vec![(x_max, y_max), (x_max, y_min), (x_max, y_min + 1)]
            .into_iter()
            .collect();
        let mut conway: Conway = start.clone().into();
        let first: PointSet = vec![(x_max - 1, y_min), (x_max, y_min), (x_min, y_min)]
            .into_iter()
            .collect();
        assert_eq!(first, conway.next().unwrap());
        assert_eq!(start, conway.next().unwrap());
    }
}
