library ieee;
use ieee.std_logic_1164.all;

entity muls is
  port (
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    mul_u4u3u : out std_logic_vector (6 downto 0);
    mul_s4s3s : out std_logic_vector (6 downto 0);
    mul_u4s3s : out std_logic_vector (7 downto 0);
    mul_s4u3s : out std_logic_vector (7 downto 0);

    mul_u4u3v : out std_logic_vector (6 downto 0);
    mul_s4s3v : out std_logic_vector (6 downto 0);
    mul_u4s3v : out std_logic_vector (7 downto 0);
    mul_s4u3v : out std_logic_vector (7 downto 0));
end muls;

library ieee;
use ieee.std_logic_arith.all;

architecture behav of muls is
begin
  mul_u4u3u <= std_logic_vector (unsigned'(unsigned(l4) * unsigned(r3)));
  mul_s4s3s <= std_logic_vector (signed'(signed(l4) * signed(r3)));
  mul_u4s3s <= std_logic_vector (signed'(unsigned(l4) * signed(r3)));
  mul_s4u3s <= std_logic_vector (signed'(signed(l4) * unsigned(r3)));

  mul_u4u3v <= unsigned(l4) * unsigned(r3);
  mul_s4s3v <= signed(l4) * signed(r3);
  mul_u4s3v <= unsigned(l4) * signed(r3);
  mul_s4u3v <= signed(l4) * unsigned(r3);
end behav;
