library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity A is
end A;

architecture sim of A is

  constant b_t_bits : integer := integer(ceil(log2(real(256))));
  subtype b_t is std_logic_vector(b_t_bits - 1 downto 0);
  -- subtype b_t is std_logic_vector(7 downto 0); -- works

  signal c : b_t := (others => '0');
  type b_arr_t is array (natural range <>) of b_t;

begin
  process
    procedure d(e : b_arr_t) is
    begin
      for i in e'range loop
        c <= e(i);
        wait for 10 ns;
      end loop;
    end procedure;
  begin
    d((
      x"00", 
      x"01"
    ));
    wait;
  end process;
end architecture;
