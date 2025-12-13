library ieee;
use ieee.std_logic_1164.all;

entity err_idx04 is
  generic (w : natural := 0);
    port (d_in    : std_logic;
          idx : natural;
          c : std_logic;
          d : std_logic;
          d_out        : out std_logic_vector(w - 1 downto 0));
end;

architecture rtl of err_idx04 is
begin
  d_out(idx) <= d_in when c = '1' else not d_in when d = '1';
end;
