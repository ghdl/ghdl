library ieee;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity forloop2 is
  port (vin: in STD_LOGIC_VECTOR (7 downto 0);
        vout: out STD_LOGIC_VECTOR (3 downto 0);
        clk: in STD_LOGIC);
end forloop2;

architecture behav of forloop2 is
begin
  process (clk, vin)
    variable count: unsigned (vout'range);
  begin
    if rising_edge (clk)
    then
      count := (others => '0');
      for I in vin'range loop
        count := count + unsigned'(0 => vin (i));
      end loop;
      vout <= std_logic_vector (count);
    end if;
  end process;
end behav;

