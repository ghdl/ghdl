library ieee;
use ieee.std_logic_1164.all;

entity reproducer is
end entity reproducer;

architecture rtl of reproducer is
    type t_slv_vector is array(natural range <>) of std_logic_vector;
    type rec is record
        slv_vector : t_slv_vector;
    end record;
    function ret_rec(constant a : std_logic_vector) return rec is
    begin
        return (slv_vector => (0 to 1 => a));
    end function;
begin
    process
        variable slv : std_logic_vector(1 downto 0);
    begin
        slv := ret_rec("10").slv_vector(0);
        wait for 1 ns;
        wait;
    end process;

end architecture;
