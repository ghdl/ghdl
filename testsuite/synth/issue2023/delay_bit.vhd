library ieee;
use ieee.std_logic_1164.all;

entity delay_bit is
  generic
  (
    DELAY : integer := 32
  );
  port
  (
    clk   : in std_logic;
    clken : in std_logic := '1';
    d     : in std_logic;
    q     : out std_logic
  );

end delay_bit;

architecture archi of delay_bit is
  signal shreg : std_logic_vector(DELAY - 1 downto 0);
begin

  process (clk) begin
    if rising_edge(clk) then
      if clken = '1' then
        shreg <= shreg(DELAY - 2 downto 0) & d;
      end if;
    end if;
  end process;

  q <= shreg(DELAY - 1);

end archi;