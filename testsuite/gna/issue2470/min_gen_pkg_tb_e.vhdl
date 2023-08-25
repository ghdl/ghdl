package min_gen_pkg is
    generic(
        size_g : positive;
        type my_type_t
    );
    type my_type_array_t is array (0 to size_g - 1) of my_type_t;
end package min_gen_pkg;
--------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
package slv_64_8_array_pkg is new work.min_gen_pkg
    generic map(
        size_g => 64,
        my_type_t => std_logic_vector(8-1 downto 0)
    );
--------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.slv_64_8_array_pkg.all;

entity min_gen_pkg_tb_e is

end entity min_gen_pkg_tb_e;

architecture test of min_gen_pkg_tb_e is
    signal array_s : my_type_array_t;
begin
    process is
    begin
        for i in 0 to 64-1 loop
            array_s(i) <= std_logic_vector(to_unsigned(i,8));
            wait for 1 ns;
            report to_string(array_s(i));
        end loop;
        std.env.stop;
    end process;
end architecture test;
