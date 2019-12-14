library ieee;
use ieee.std_logic_1164.all;

entity var01a is
  port (clk : std_logic;
        mask : std_logic_vector (1 downto 0);
        val : std_logic_vector (7 downto 0);
        res : out std_logic_vector (7 downto 0));
end var01a;

architecture behav of var01a is
begin
  process (clk)
    variable hi, lo : natural;
  begin
    if rising_edge (clk) then
      for i in 0 to 1 loop
        if mask (i) = '1' then
          lo := i * 4;
          hi := lo + 3;
          res (hi downto lo) <= val (hi downto lo);
        end if;
      end loop;
    end if;
  end process;
end behav;
