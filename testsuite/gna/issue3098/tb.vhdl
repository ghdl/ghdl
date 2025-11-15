library ieee;
use ieee.std_logic_1164.all;

entity generic_type_entity is
   generic (type T);
   port (port_in : in T; port_out : out T);
end entity;

architecture arch of generic_type_entity is
begin
   port_out <= port_in;
end architecture;

entity tb_ghdl is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture arch of tb_ghdl is
   signal foo, bar : std_logic;
begin

   dut : entity work.generic_type_entity
      generic map(
         T => foo'subtype               --this crashes
      -- T => std_logic                 --this works
      )
      port map(foo, bar);
   process
   begin
      report "hello_world";
      std.env.finish;
   end process;
end architecture;
