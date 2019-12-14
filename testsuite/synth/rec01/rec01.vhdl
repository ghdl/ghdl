library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

entity rec01 is
  port (inp : myrec;
        rst : std_logic;
        clk : std_logic;
        o : out std_logic);
end rec01;

architecture behav of rec01 is
  signal s : myrec;
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      if rst = '1' then
        s <= (a => "0000", b => '0');
      else
        if inp.b = '1' then
          s <= (a => inp.a, b => '1');
        else
          s <= inp;
        end if;
      end if;
    end if;
  end process;
  
  o <= '1' when s.a (1) = s.b else '0';
end behav;
