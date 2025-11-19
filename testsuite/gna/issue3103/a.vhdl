library ieee;
use ieee.std_logic_1164.all;

entity foo is
   generic (type T);
   port (port_in : in T; port_out : out T);
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
   type record_type is record
      a : std_logic;
      b : std_logic;
   end record;
   signal a_sig, b_sig : std_logic;
begin

   dut : entity work.foo
      generic map(T => record_type)
      port map(
      -- A: indexing in the ports (looks shady to me but questasim compiles it)
         port_in.a => a_sig, -- This crashes the GHDL compiler
         port_in.b => b_sig  -- This crashes the GHDL compiler
      -- B: assignment of an inline instance of the record.
        -- port_in => record_type'(a => a_sig, b => b_sig) -- This crashes the GHDL runtime
      );
   process
   begin
      report "hello_world";
      std.env.finish;
   end process;
end architecture;
