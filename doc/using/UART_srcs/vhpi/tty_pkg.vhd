--tty_pkg.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package tty_pkg is

  function to_integer( s: std_logic) return integer;  
  function to_std_logic( s : integer ) return std_logic;
  
  function tty_open (portn : integer) return integer;
  attribute foreign of tty_open :
    function is "VHPIDIRECT tty_open";

  function read_data ( dummy: integer) return integer;
  attribute foreign of read_data :
    function is "VHPIDIRECT read_data"; 

  function read_enable ( dummy: integer) return integer;
  attribute foreign of read_enable :
    function is "VHPIDIRECT read_enable"; 

  procedure write_data ( data: in integer);
    attribute foreign of write_data :
    procedure is "VHPIDIRECT write_data"; 

end;


package body tty_pkg is

  function to_integer( s : std_logic ) return integer is
  begin
    if s = '1' then
      return 1;
    else
      return 0;
    end if;
  end function;

  function to_std_logic( s : integer ) return std_logic is
  begin
    if s > 0 then
      return '1';
    else
      return '0';
    end if;
  end function;

  
  function tty_open (portn : integer) return integer is
  begin
    assert false report "VHPI" severity failure;
  end tty_open;
 
  function read_data (dummy: integer) return  integer is
  begin
    assert false report "VHPI" severity failure;
  end read_data;  
 
   function read_enable (dummy: integer) return  integer is
  begin
    assert false report "VHPI" severity failure;
  end read_enable;  
  
  procedure write_data ( data: in integer) is
  begin
    assert false report "VHPI" severity failure;
  end write_data; 
end tty_pkg;
