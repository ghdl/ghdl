library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.math_real.all;

package wb_pkg is
  
  constant wb_no_c : std_ulogic := '0';
    
  type array_t is array (integer range <> ) of std_ulogic_vector;
  
  type dmn_t is record
    clk : std_ulogic;
    rst : std_ulogic;
  end record;
  
  --! WISHBONE master to slave signals type
  type wb_M2S_t is record
    tgc : std_ulogic_vector            ;  --! cycle tag
    loc : std_ulogic                   ;  --! lock 
    cyc : std_ulogic                   ;  --! cycle
    stb : std_ulogic                   ;  --! strobe
    we  : std_ulogic                   ;  --! write transaction
    tga : std_ulogic_vector            ;  --! Address tag 
    adr : std_ulogic_vector            ;  --! address   
    bte : std_ulogic_vector(1 downto 0);  --! burst type extension  
    sel : std_ulogic_vector            ;  --! Byte selection
    tgd : std_ulogic_vector            ;  --! data tag master to slave 
    dat : std_ulogic_vector            ;  --! data master to slave     
  end record;
  
  --! WISHBONE slave to master signals type  
  type wb_S2M_t is record
    stl : std_ulogic       ;  --! stall pipeline
    tgd : std_ulogic_vector;  --! data tag slave to master
    dat : std_ulogic_vector;  --! data slave to master
    ack : std_ulogic       ;  --! acknowledgement slave to master 
    err : std_ulogic       ;  --! error slave to master
    rty : std_ulogic       ;  --! retry slave to master
  end record; 
   
  pure function get_uvect( len : positive ; bit_i : in std_ulogic := '0' )
  return std_ulogic_vector;
   
end wb_pkg;
--#############################################################################
--#############################################################################
package body wb_pkg is

  pure function get_uvect( len : positive ; bit_i  : in std_ulogic := '0' )
  return std_ulogic_vector
  is
    variable vect_v : std_ulogic_vector( len - 1  downto 0 );
  begin
    for i in 0 to len - 1 loop
      vect_v( i ) := bit_i ;
    end loop;
    return vect_v;
  end function;

end package body wb_pkg;
--#############################################################################
--#############################################################################
--#############################################################################
--#############################################################################
--#############################################################################
