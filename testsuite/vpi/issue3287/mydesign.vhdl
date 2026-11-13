library ieee;
use ieee.std_logic_1164.all;

entity sub is
  generic (
    G_INT  : integer := 7;
    G_SLV  : std_logic_vector(3 downto 0) := "1010";
    G_STR  : string := "inner";
    G_REAL : real := 2.5;
    G_BOOL : boolean := false
  );
end entity;

architecture arch of sub is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity myentity is
  generic (
    G_INT  : integer := 42;
    G_SLV  : std_logic_vector(7 downto 0) := x"a5";
    G_STR  : string := "hello";
    G_REAL : real := 1.5;
    G_BOOL : boolean := true
  );
end entity;

architecture arch of myentity is
begin
  u_sub : entity work.sub;
end architecture;
