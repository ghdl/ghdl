library ieee;
context ieee.ieee_std_context;

package uCPUtypes is
  alias logic is std_ulogic;
  alias logic_vec is std_ulogic_vector;
  subtype unsigned_byte is unsigned(7 downto 0);
  subtype code_word is unsigned(11 downto 0);
end package uCPUtypes;
