library IEEE;
use IEEE.std_logic_1164.all;

entity bug2 is
port(
    clk : in std_ulogic
);
end entity;

architecture rtl of bug2 is

    type table_t is array (natural range<>, natural range<>) of integer;

    function fun1(table : table_t) return integer is
        constant len : natural := table'length(2);
    begin
        return len;
    end function;

    function fun2(table : table_t) return integer is
        variable tmp : natural;
    begin
        tmp := 0;
        for i in table'range(2) loop
            tmp := tmp+1;
        end loop;
        return tmp;
    end function;

    constant table : table_t(0 to 15, 0 to 31) := (others => (others => 0));
    constant l1 : natural := fun1(table);
    constant l2 : natural := fun2(table);
begin

  assert table'right(2) = 15; --  Wrong
end architecture;
