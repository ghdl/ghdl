pub mod nodes_def;
pub mod nodes_utils;

use nodes_def::Node;

extern "C" {
    #[link_name = "vhdl__prints__disp_vhdl__2"]
    pub fn disp_vhdl(n: Node);
}