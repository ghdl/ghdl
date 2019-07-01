library ieee;
use ieee.std_logic_1164.all;

entity arr04 is
  port (clk : in std_logic;
        rst : std_logic;
        sel_i : std_logic;
        sel_o : std_logic;
        v : std_logic;
        res : out std_logic);
end arr04;

architecture behav of arr04 is
  signal reg : std_logic_vector (0 to 1);
begin
  --  Reader
  process(clk)
  begin
    if rising_edge (clk) then
      if sel_o = '0' then
        res <= reg (0);
      else
        res <= reg (1);
      end if;
    end if;
  end process;

  -- Writer
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        reg <= "00";
      else
        if sel_i = '0' then
          reg (0) <= v;
        else
          reg (1) <= v;
        end if;
      end if;
    end if;
  end process;
end behav;
