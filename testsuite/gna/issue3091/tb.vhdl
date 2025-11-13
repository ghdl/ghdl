library ieee;
use ieee.std_logic_1164.all;

package local_pkg is generic ( g_width : natural );
   type local_type is record
      field: std_logic_vector(g_width-1 downto 0);
   end record;
end package;

package local_pkg_instances is
   package local_pkg_2 is new work.local_pkg generic map(2);
   -- alias local_type_2 is local_pkg_2.local_type;
   subtype other_local_type_2 is local_pkg_2.local_type;
end package;

entity tb_ghdl is
end entity;

architecture arch of tb_ghdl is
   -- signal a : work.local_pkg_instances.local_type_2;
   signal b : work.local_pkg_instances.other_local_type_2;
begin
   process
   begin
      report "hello_world";
      std.env.finish;
   end process;
end architecture;
