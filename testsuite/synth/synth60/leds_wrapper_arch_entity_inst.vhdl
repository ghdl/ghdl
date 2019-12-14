architecture rtl_comp_inst of leds_wrapper is

begin

  leds_comp_inst : entity work.leds(spin1)
    port map(
      clk => clk,
      led1 => led1,
      led2 => led2,
      led3 => led3,
      led4 => led4,
      led5 => led5,
      led6 => led6,
      led7 => led7,
      led8 => led8
    );

end architecture;
