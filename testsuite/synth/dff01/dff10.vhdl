library ieee;
use ieee.std_logic_1164.all;

entity dff10 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        rst : std_logic;
        en : std_logic);
end dff10;

architecture behav of dff10 is
begin
  process (clk, rst) is
    constant rval : std_logic_vector(7 downto 0) := x"55";
  begin
    if rst = '1' then
      q <= rval;
    elsif rising_edge (clk) then
      if en = '1' then
        q <= d;
      end if;
    end if;
  end process;
end behav;
