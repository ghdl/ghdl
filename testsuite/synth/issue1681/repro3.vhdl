library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  generic(
    clock_count_c    : natural range 1 to 2 := 2
    );
  port(
    reset_n_i : in  std_ulogic;
    clock_i   : in  std_ulogic_vector(0 to clock_count_c-1)
    );

end ;

architecture beh of repro3 is

  type regs_t is
  record
    foo: std_ulogic;
  end record;

  signal r, rin: regs_t;
  
begin

  regs: process (clock_i, reset_n_i)
  begin
    if clock_i(clock_count_c-1)'event and clock_i(1) = '1' then
      if reset_n_i = '0' then
        r.foo <= '0';
      else
        r <= rin;
      end if;
    end if;
  end process;
  
end architecture;
