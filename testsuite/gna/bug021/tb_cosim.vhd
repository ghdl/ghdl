--------------------------------------------------------------------------------
-- Company:      Dossmatik GmbH
-- Create Date:   21:08:31 05/17/2011

-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench 
-- test for VHPI
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sim_pkg.all;


entity tb_cosim is
end tb_cosim;

architecture behavior of tb_cosim is

 function crc (crc_value : std_logic_vector(31 downto 0)
                          ) return std_logic_vector is

    variable crc_out : std_logic_vector(31 downto 0);
    
  begin
    crc_out := (crc(3 downto 0)& crc_out(31 downto 4)) xor crc;
    return crc_out;
  end crc;

  signal random : std_logic_vector ( 31 downto 0):=X"00000000";
  -- Clock period definitions

  constant board_clk_period : time := 20 ns;

  signal board_clk: std_logic;
begin


 
  process (board_clk)
  begin 
    if rising_edge(board_clk) then
      street(to_integer(unsigned(random)));
      random<=crc(random);
    end if;
  end process;
  




  -- Clock process definitions
  board_clk_process : process
  begin
    board_clk <= '0';
    wait for board_clk_period/2;
      board_clk <= '1';
    wait for board_clk_period/2;
  end process;


  end;
