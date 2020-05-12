library work;
use work.wishbone_types.all;

entity icache is
    port (
        clk          : in bit;
        rst          : in bit;

--        wishbone_in  : in wb_cpu_in_t
        wishbone_in  : in wishbone_slave_out
        );
end entity icache;

architecture rtl of icache is
begin
end;
