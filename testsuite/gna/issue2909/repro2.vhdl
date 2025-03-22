library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
end entity;

architecture tb of repro2 is

-- Define tb constants:
  constant clk_period   : time    := 10 ns;
  constant items        : natural := 2;

  type data_t is array (natural range <>) of signed;

  constant test_data_8  : data_t (0 to items-1)( 7 downto 0) := (
    (x"ec"), 
    (x"ea")
  ); 
  constant test_data_16 : data_t (0 to items-1)(15 downto 0) := (
    (x"6a6b"), 
    (x"1448")
  ); 
  constant test_data_32 : data_t (0 to items-1)(31 downto 0) := (
    (x"ff7ffb87"), 
    (x"a5a38967")
  ); 
  
  type checker_t is array (0 to 2) of data_t(open)(7 downto 0);
  constant checker : checker_t := (
  test_data_8, test_data_16, test_data_32);
begin
  array_test: process
  begin
    report "Arrays 1, 2 and 3 are composed of 8-bit, 16-bit and 32-bit data, respectively.";
      for y in 0 to 2 loop
        for x in 0 to items-1 loop
          report "Elment (" & integer'image(x) & ") of array (" & integer'image(y+1) & ") is: (" & to_hstring(std_logic_vector(checker(y)(x))) & ">";
        end loop;
      end loop;
      wait;
  end process;

end architecture;
