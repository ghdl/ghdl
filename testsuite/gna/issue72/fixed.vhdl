library ieee;
use ieee.std_logic_1164.all;

use work.issue_pkg.t_one_two;           -- does not work
use work.issue_pkg."=";
--use work.issue_pkg.all;                 -- works

entity issue is

  port (
    clk    : in  std_logic;
    input  : in  t_one_two;
    output : out std_logic
    );

end entity issue;

architecture rtl of issue is

begin  -- architecture rtl

  process (clk) is
  begin  -- process
    if clk'event and clk = '1' then  -- rising clock edge
      if input = work.issue_pkg.one then
        output <= '1';
      else
        output <= '0';
      end if;
    end if;
  end process;

end architecture rtl;
