library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity addern is
 generic(
  width : integer := 8
 );
 port(
  A, B : in  std_logic_vector(width - 1 downto 0);
  Y    : out std_logic_vector(width - 1 downto 0)
 );
end addern;

architecture bhv of addern is
begin
 Y <= A + B;
end bhv;

Library IEEE;
use IEEE.std_logic_1164.all;

entity generics_1 is
 port(
  X, Y, Z : in  std_logic_vector(12 downto 0);
  A, B    : in  std_logic_vector(4 downto 0);
  S       : out std_logic_vector(17 downto 0));
end generics_1;

architecture bhv of generics_1 is
 component addern
  generic(width : integer := 8);
  port(
   A, B : in  std_logic_vector(width - 1 downto 0);
   Y    : out std_logic_vector(width - 1 downto 0));
 end component;
 for all : addern use entity work.addern(bhv);

 signal C1     : std_logic_vector(12 downto 0);
 signal C2, C3 : std_logic_vector(17 downto 0);
begin
 U1 : addern generic map(width => 13) port map(X, Y, C1);
 C2 <= C1 & A;
 C3 <= Z & B;
 U2 : addern generic map(width => 18) port map(C2, C3, S);
end bhv;
