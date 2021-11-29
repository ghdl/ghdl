-- (c)EMARD
-- License=BSD

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity top_ecp5pll is
  generic
  (
    bits: integer := 26
  );
  port
  (
    clk_25mhz : in  std_logic;  -- main clock input from 25MHz clock source
    led       : out std_logic_vector(7 downto 0)
  );
end;

architecture mix of top_ecp5pll is
    type T_blink is array (0 to 3) of std_logic_vector(bits-1 downto 0);
    signal R_blink: T_blink;
    signal clocks: std_logic_vector(3 downto 0);
begin
    clkgen_inst: entity work.ecp5pll
    generic map
    (
        in_Hz => natural( 25.0e6),
      out0_Hz => natural( 40.0e6),                  out0_tol_hz => 0,
      out1_Hz => natural( 50.0e6), out1_deg =>  90, out1_tol_hz => 0,
      out2_Hz => natural( 60.0e6), out2_deg => 180, out2_tol_hz => 0,
      out3_Hz => natural(  6.0e6), out3_deg => 300, out3_tol_hz => 10
    )
    port map
    (
      clk_i => clk_25mhz,
      clk_o => clocks
    );

    G_blinks: for i in 0 to 3
    generate
      process(clocks(i))
      begin
        if rising_edge(clocks(i)) then
          R_blink(i) <= R_blink(i)+1;
        end if;
        led(2*i+1 downto 2*i) <= R_blink(i)(bits-1 downto bits-2);
      end process;
    end generate;

end mix;
