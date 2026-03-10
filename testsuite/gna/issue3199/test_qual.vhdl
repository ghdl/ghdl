-- File: test_qual.vhd
library ieee;
use ieee.std_logic_1164.all;

entity child is
  port (
    stb : in std_logic_vector
  );
end entity;

architecture rtl of child is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test_qual is end;
architecture sim of test_qual is
  signal s_stb : std_logic := '0';
begin

  u_child : entity work.child
    port map (
      stb => std_logic_vector'(0 => s_stb)   -- CRASH
    );

  process begin
    wait for 100 ns;
    report "PASS";
    std.env.stop(0);
  end process;
end;

