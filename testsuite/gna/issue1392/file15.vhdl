-- Code your testbench here
library IEEE;
use IEEE.std_logic_1164.all;
--use std.textio.all;
--use work.proc_pkg.all;


entity file15 is

end entity file15;

architecture rtl of file15 is

  type char_array_t is array(natural range <>) of character;
  type char_arr_file is file of char_array_t;
  
begin
  process
    
    file cr : char_arr_file;
    
    variable mcrr : char_array_t(12 downto 0);
    variable small_mcrr : char_array_t(2 downto 0);
    variable c : character := 'a';
    variable len : natural;
    
  begin
    
    file_open(cr, "./char_arr_file.txt", WRITE_MODE);
    for i in 0 to 12 loop
      mcrr(i) := c;
      c := character'succ(c);
    end loop;
    write(cr, mcrr);
    file_close(cr);
    
    -- small container
    file_open(cr, "./char_arr_file.txt", READ_MODE);
    read(cr, small_mcrr,len);
    
    for i in 0 to 2 loop
      report "" & small_mcrr(i);
    end loop;
    file_close(cr);
    
    
    wait;
  end process;
    
end rtl;


