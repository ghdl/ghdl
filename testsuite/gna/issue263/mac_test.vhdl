entity mac_test is
end entity mac_test;

library ieee;
use ieee.std_logic_1164.all, ieee.fixed_pkg.all,
    ieee.math_complex.all;

architecture bench_behavioral of mac_test is

  signal clk, reset, ovf : std_ulogic := '0';
  signal x_real, x_imag,
         y_real, y_imag,
         s_real, s_imag : u_sfixed(0 downto -15);

  signal x, y, s : complex := (0.0, 0.0);

  constant Tpw_clk : time := 50 ns;

begin

  x_real <= x.re; x_imag <= x.im;
  y_real <= y.re; y_imag <= y.im;

  dut : entity work.mac(behavioral)
    port map ( clk, reset,
               x_real, x_imag, y_real, y_imag, s_real, s_imag,
               ovf );

  s <= (s_real, s_imag);

  clock_gen : process is
  begin
    clk <= '1' after Tpw_clk, '0' after 2 * Tpw_clk;
    wait for 2 * Tpw_clk;
  end process clock_gen;

  stimulus : process is
  begin
    -- first sequence
                                            reset <= '1';
    wait until not clk;
    x <= (+0.5, +0.5);  y <= (+0.5, +0.5);  reset <= '1';
    wait until not clk;
    x <= (+0.2, +0.2);  y <= (+0.2, +0.2);  reset <= '1';
    wait until not clk;
    x <= (+0.1, -0.1);  y <= (+0.1, +0.1);  reset <= '1';
    wait until not clk;
    x <= (+0.1, -0.1);  y <= (+0.1, +0.1);  reset <= '0';
    wait until not clk;

    -- should be (0.04, 0.58) when it falls out the other end

                                            reset <= '0';
    wait until not clk;
    x <= (+0.5, +0.5);  y <= (+0.5, +0.5);  reset <= '0';
    wait until not clk;
    x <= (+0.5, +0.5);  y <= (+0.1, +0.1);  reset <= '0';
    wait until not clk;
    x <= (+0.5, +0.5);  y <= (+0.5, +0.5);  reset <= '1';
    wait until not clk;
    x <= (-0.5, +0.5);  y <= (-0.5, +0.5);  reset <= '0';
    wait until not clk;
                                            reset <= '0';
    wait until not clk;
                                            reset <= '0';
    wait until not clk;
                                            reset <= '0';
    wait until not clk;
                                            reset <= '1';
    wait until not clk;

    wait;
  end process stimulus;

end architecture bench_behavioral;
