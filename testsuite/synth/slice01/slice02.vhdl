library ieee;
use ieee.std_logic_1164.all;

entity slice02 is
  generic (w: natural := 4);
  port (clk : std_logic;
        dat : std_logic_vector (7 downto 0);
        mask : std_logic_vector (1 downto 0);
        res : out std_logic_vector (7 downto 0));
end slice02;

architecture behav of slice02 is
begin
  process(clk)
    variable hi, lo : natural;
  begin
    if rising_edge (clk) then
      res <= (others => '0');
      for i in mask'range loop
        if mask (i) = '1' then
          lo := i * 4;
          hi := lo + 3;
          res (hi downto lo) <= dat (hi downto lo);
        end if;
      end loop;
    end if;
  end process;
end behav;
