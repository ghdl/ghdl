library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity oper01 is
  port (clk : std_logic;
        val : std_logic_vector (2 downto 0);
        v2 : std_logic_vector (2 downto 0);
        o : out std_logic);
end oper01;

architecture behav of oper01 is
  signal r1, r2, r3, r4 : std_logic;
  signal vs : signed(2 downto 0);
  signal vu : unsigned(2 downto 0);
  signal c1 : boolean;
begin
  vs <= signed(v2);
  vu <= unsigned(v2);
  r1 <= or val;
  r2 <= and val;
  r3 <= xor val;
  r4 <= '1' when -vs >= "110" else '0';
  c1 <= (vs <= "010") xor (vu <= "110");
  
  assert (r1 or r2) = '1';
  
  process (clk)
  begin
    if falling_edge(clk) and c1 then
      o <= r1 xor r2 xor r3 xor r4;
    end if;
  end process;
end behav;
