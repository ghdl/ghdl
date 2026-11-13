library ieee;
use ieee.std_logic_1164.all;

entity sub is
  generic (
    G_INT  : integer := 7;
    G_SLV  : std_logic_vector(3 downto 0) := "1010";
    G_STR  : string := "inner";
    G_REAL : real := 2.5;
    G_BOOL : boolean := false;
    G_EMPTY : string := "";
    G_ASC  : std_logic_vector(0 to 7) := "10000001";
    G_OFS  : std_logic_vector(11 downto 4) := x"3c"
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
    G_BOOL : boolean := true;
    G_EMPTY : string := "";
    G_ASC  : std_logic_vector(0 to 7) := "11110000";
    G_OFS  : std_logic_vector(11 downto 4) := x"96"
  );
end entity;

architecture arch of myentity is
begin
  u_sub : entity work.sub;
  u_ovr : entity work.sub
    generic map (G_INT => -3, G_SLV => "0110", G_STR => "ovr",
                 G_REAL => 0.25, G_BOOL => true);
end architecture;
