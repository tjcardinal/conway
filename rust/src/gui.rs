use std::time::Instant;

use crate::conway;

use eframe::egui::{Color32, Pos2, Rect, Sense, Shape, Stroke, Vec2};
use eframe::{egui, epi};

pub struct App {
    conway: conway::Conway,
    conway_update_time: Instant,
    pixel_size: f32,
    paused: bool,
}

impl Default for App {
    fn default() -> Self {
        Self {
            conway: vec![(2, 2), (2, 3), (2, 4), (3, 1), (3, 2), (3, 3)]
                .into_iter()
                .collect::<conway::PointSet>()
                .into(),
            conway_update_time: Instant::now(),
            pixel_size: 10.0,
            paused: false,
        }
    }
}

impl epi::App for App {
    fn name(&self) -> &str {
        "Conway's Game of Life"
    }

    fn update(&mut self, ctx: &egui::CtxRef, _: &mut epi::Frame<'_>) {
        ctx.request_repaint();
        if (self.conway_update_time.elapsed().as_millis() >= 500) && (!self.paused) {
            self.conway_update_time = Instant::now();
            self.conway.next();
        }

        egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
            let pause_text = match self.paused {
                true => "Unpause",
                false => "Pause",
            };
            ui.horizontal(|ui| {
                if ui.button(pause_text).clicked() {
                    self.paused = !self.paused;
                }
                ui.add(egui::Slider::new(&mut self.pixel_size, 1.0..=50.0).text("Pixel Size"));
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let area = ui.available_size();
            let pixel_size = self.pixel_size;
            let (response, painter) = ui.allocate_painter(area, Sense::click());

            if let Some(Pos2 { x, y }) = response.interact_pointer_pos() {
                let coordinates = (
                    (x / pixel_size) as conway::Coordinate,
                    (y / pixel_size) as conway::Coordinate,
                );
                if response.clicked() {
                    self.conway.current.insert(coordinates);
                } else if response.secondary_clicked() {
                    self.conway.current.remove(&coordinates);
                }
            }

            painter.rect_filled(ui.max_rect_finite(), 0.0, Color32::WHITE);

            let pixels = self
                .conway
                .current
                .iter()
                .map(|(x, y)| Shape::Rect {
                    rect: Rect::from_min_size(
                        Pos2::new(*x as f32 * pixel_size, *y as f32 * pixel_size),
                        Vec2::new(pixel_size, pixel_size),
                    ),
                    corner_radius: 0.0,
                    fill: Color32::BLACK,
                    stroke: Stroke::none(),
                })
                .collect();

            painter.extend(pixels);
        });
    }
}
