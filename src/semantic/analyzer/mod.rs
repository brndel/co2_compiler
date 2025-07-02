mod check_return;
mod check_status_and_types;
mod map_numbers;
mod check_loop_controls;
mod main_fn;
mod struct_namespace;
mod func_namespace;

pub use check_return::check_return;
pub use map_numbers::map_number;
pub use check_status_and_types::check_status_and_types;
pub use check_loop_controls::check_loop_controls;
pub use main_fn::check_main_fn;
pub use struct_namespace::get_struct_namespace;
pub use func_namespace::get_func_namespace;