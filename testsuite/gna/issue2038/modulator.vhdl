library ieee;
use ieee.std_logic_1164.all;

library modulation_lib;
use modulation_lib.types_pkg.all;

entity modulator is
  port (
    req_modulation : in std_logic;
    busy : out std_logic;
    pwm : out std_logic
  );
end modulator;

architecture behavioral of modulator is
  shared variable symbol_buffer : modulation_lib.symbol_stack_pkg.stack;
begin
  p_modulate : process
    variable v_pwm_symbol : t_symbol;
  begin
    busy <= '0';
    pwm <= '0';
    main_loop : loop
      wait until req_modulation = '1';
      busy <= '1';
      while not symbol_buffer.is_empty loop
        v_pwm_symbol := symbol_buffer.pop;
        pwm <= '1';
        case v_pwm_symbol is
          when NARROW_SYMBOL =>
            wait for 10 us;
          when REGULAR_SYMBOL =>
            wait for 30 us;
          when WIDE_SYMBOL =>
            wait for 50 us;
          when others =>
            assert false report "Unknown modulation symbol" severity error;
        end case;
        pwm <= '0';
        wait for 5 us;
      end loop;
      busy <= '0';
    end loop; -- main_loop
  end process p_modulate;
end architecture behavioral;
