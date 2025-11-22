library ieee;
use ieee.std_logic_1164.all;

entity foo is
   generic (type T);
   port (
      port_in       : in  T;
      port_in_other : in  std_logic;
      port_out      : out T
   );
end entity;

architecture arch of foo is
begin
   port_out <= port_in;
end;

entity tb_ghdl is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture arch of tb_ghdl is
   signal a_sig, b_sig, c_sig : std_logic;
begin
   dut : entity work.foo
      generic map(T => std_logic)
      port map(
         port_in       => a_sig,
         port_in_other => b_sig and c_sig
      );
   process
   begin
      report "hello_world";
      std.env.finish;
   end process;
end architecture;
