library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ent is
generic (
  W : integer := 1
);
port (
  wen : in std_logic_vector(W-1 downto 0)
);
end entity ent;

architecture rtl of ent is
begin

  process begin
    report "Hello world from ent." severity note;
    wait;
  end process;

end rtl;
