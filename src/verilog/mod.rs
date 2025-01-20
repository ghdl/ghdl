mod nodes_def;
mod types;

type Node = u32;

extern "C" {
    #[link_name = "verilog__errors__initialize"]
    pub fn errors_initialize();

    #[link_name = "verilog__scans__init_paths"]
    pub fn scans_init_paths();

    #[link_name = "verilog__sem_scopes__init"]
    pub fn sem_scopes_init();

    #[link_name = "verilog__sem_types__create_basetypes"]
    pub fn sem_types_create_basetypes();

    #[link_name = "verilog__vpi__initialize"]
    pub fn vpi_initialize();
}