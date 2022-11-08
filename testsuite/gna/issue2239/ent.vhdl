library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture behaviour of ent is
  
  signal test : std_logic_vector(3 downto 0);

  procedure DUMMY (signal slv : out std_logic_vector(7 downto 0)) is
  begin
    slv <= x"00";
  end procedure;
  
begin

  process
  begin
    DUMMY (slv(7 downto 4) => open,
           slv(3 downto 0) => test);
    wait;
  end process;
  
end architecture;
