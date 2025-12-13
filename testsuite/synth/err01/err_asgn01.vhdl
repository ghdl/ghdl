library ieee;
use ieee.std_logic_1164.all;

entity err_asgn01 is
  generic (w : natural := 0);
    port (d_in    : std_logic;
          cond        : std_logic_vector(w - 1 downto 0);
          c : std_logic;
          d_out        : out std_logic);
end;

architecture rtl of err_asgn01 is
begin
  d_out <= d_in when cond(w) = '1' else '1' when c = '0' else '0';
end;
