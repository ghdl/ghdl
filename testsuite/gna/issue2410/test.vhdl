library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;
architecture beh of test is
   type    t_slv_array is array (natural range <>) of std_logic_vector;
   subtype t_word_array is t_slv_array(open)(15 downto 0);

  procedure test_proc(
    variable sig : out t_word_array)
  is
    variable v_sig : t_word_array(0 to sig'length);
  begin
    v_sig := (others => x"AAAA");
    sig := v_sig(1 to sig'length);
  end procedure;
begin
  
  process
    variable v_sig : t_word_array(0 to 0);
  begin
    test_proc(v_sig);
    wait;
  end process;

end architecture beh;
