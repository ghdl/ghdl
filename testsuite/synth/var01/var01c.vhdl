library ieee;
use ieee.std_logic_1164.all;

entity var01c is
  port (clk : std_logic;
        mask : std_logic_vector (1 downto 0);
        val : std_logic_vector (1 downto 0);
        res : out std_logic_vector (3 downto 0));
end var01c;

architecture behav of var01c is
begin
  process (clk)
    variable hi, lo : natural;
  begin
    if rising_edge (clk) then
      for i in 0 to 1 loop
        if mask (i) = '1' then
          lo := i * 2;
          hi := lo + 1;
          res (hi downto lo) <= val;
        end if;
      end loop;
    end if;
  end process;
end behav;
