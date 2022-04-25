mod conway;
mod gui;

fn main() {
    let app = gui::App::default();
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(Box::new(app), native_options);
}
