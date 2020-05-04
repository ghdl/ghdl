library ieee;
use ieee.std_logic_1164.all;

entity issue is
    generic (N : natural := 32);
    port (foo   : in  std_logic_vector (N-1 downto 0);
          bar_1 : out std_logic;
          bar_2 : out std_logic);
end issue;

architecture beh of issue is
    subtype slice1_t is natural range 9 downto 0;
    subtype slice2_t is natural range slice1_t'high downto foo'low;
begin
    bar_1 <= or foo (slice1_t); -- works
    bar_2 <= or foo (slice2_t); -- error and crashes
end architecture beh;

