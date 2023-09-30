library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use     std.textio.all;
use     std.env.finish; -- use of vhdl-2008


entity i2c_tb is
end entity i2c_tb;

architecture str_tb of i2c_tb is

  constant C_CLK_PERIOD  : time := 20 ns; -- 50 Mhz

  constant C_A  : unsigned := to_unsigned(16#00#, 16); -- R/W
  constant C_B  : unsigned := to_unsigned(16#04#, 16); -- RO

  type regMap_tarr is array (natural range <>) of unsigned;
  constant C_REGISTERMAP : regMap_tarr := (
    0 => C_A,
    1 => C_B
  );

  file fd_log : text;
  signal s_clk          : std_logic := '1';
  signal s_rstn         : std_logic := '0';

BEGIN

clk_gen:
  s_clk <= not s_clk after C_CLK_PERIOD/2;

rst_gen:
    s_rstn <= '1' after 1 us;
stimulus: 
  process
  begin

     wait until s_rstn='1';
     write(output, LF & " ===========================================" & LF
                      & " +--      Starting  Test      --+" & LF
                      & " ===========================================" & LF & LF);

     wait until rising_edge(s_clk);  
      write(output, LF & " +--  Test has finished sucessfully!!" & LF);
      write(output, LF & " ===========================================" & LF
                       & " +--      Starting  Test      --+" & LF
                       & " ===========================================" & LF & LF);
      finish;
    end process;
END architecture str_tb;
