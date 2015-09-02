
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package sim_pkg is



  procedure house ( reg : in integer );
  attribute foreign of house :
    procedure is "VHPIDIRECT house"; 


  procedure street ( reg : in integer );
  attribute foreign of street :
    procedure is "VHPIDIRECT street";   
end;


package body sim_pkg is


  procedure house (reg : in integer)  is
  begin
    assert false report "VHPI" severity failure;
  end house;

  procedure street (reg : in integer)  is
  begin
    assert false report "VHPI" severity failure;
  end street;
    
end sim_pkg;
