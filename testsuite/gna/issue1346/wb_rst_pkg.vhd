library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    
library work;
    use work.wb_pkg.all;

package wb_rst_pkg is
  
  pure function get_wb_rst( wb_i : wb_M2S_t)
  return wb_M2S_t;
  
  pure function get_wb_rst( wb_i : wb_S2M_t)
  return wb_S2M_t;
  
  procedure set_wb_rst( wb_o : out wb_M2S_t);
  procedure set_wb_rst( wb_o : out wb_S2M_t);
  
end wb_rst_pkg;
--#############################################################################
--#############################################################################
package body wb_rst_pkg is

  pure function get_wb_rst( wb_i : wb_M2S_t)
  return wb_M2S_t
  is 
    variable wb_v
    : wb_M2S_t
    ( tgc( wb_i.tgc'length - 1 downto 0) 
    , tga( wb_i.tga'length - 1 downto 0) 
    , adr( wb_i.adr'length - 1 downto 0) 
    , sel( wb_i.sel'length - 1 downto 0) 
    , tgd( wb_i.tgd'length - 1 downto 0) 
    , dat( wb_i.dat'length - 1 downto 0) 
    );
  
  begin
  
    wb_v.tgc := get_uvect( wb_i.tgc'length, 'X' );
    wb_v.loc := wb_no_c;
    wb_v.cyc := wb_no_c;
    wb_v.stb := wb_no_c;
    wb_v.we  := 'X';
    wb_v.tga := get_uvect( wb_i.tga'length, 'X' );
    wb_v.adr := get_uvect( wb_i.adr'length, 'X' );
    wb_v.bte := get_uvect( wb_i.bte'length, 'X' );
    wb_v.sel := get_uvect( wb_i.sel'length, 'X' );
    wb_v.tgd := get_uvect( wb_i.tgd'length, 'X' );
    wb_v.dat := get_uvect( wb_i.dat'length, 'X' );
    
    return wb_v;
  end function;

  pure function get_wb_rst( wb_i : wb_S2M_t)
  return wb_S2M_t
  is
    variable wb_v
    : wb_S2M_t
    ( tgd( wb_i.tgd'length - 1 downto 0) 
    , dat( wb_i.dat'length - 1 downto 0) 
    );
   
  begin

    wb_v.tgd := get_uvect( wb_i.tgd'length, 'X' );
    wb_v.dat := get_uvect( wb_i.dat'length, 'X' );
    wb_v.stl := wb_no_c;     
    wb_v.ack := wb_no_c;     
    wb_v.err := wb_no_c;     
    wb_v.rty := wb_no_c;
    
    return wb_v;
  end function;

  procedure set_wb_rst( wb_o : out wb_M2S_t)
  is
    variable wb_v
    : wb_M2S_t
    ( tgc( wb_o.tgc'length - 1 downto 0) 
    , tga( wb_o.tga'length - 1 downto 0) 
    , adr( wb_o.adr'length - 1 downto 0) 
    , sel( wb_o.sel'length - 1 downto 0) 
    , tgd( wb_o.tgd'length - 1 downto 0) 
    , dat( wb_o.dat'length - 1 downto 0) 
    );
  
  begin
    wb_v := get_wb_rst( wb_v ); 
    wb_o := wb_v;
  end procedure;
  
  procedure set_wb_rst( wb_o : out wb_S2M_t)
  is
    variable wb_v
    : wb_S2M_t
    ( tgd( wb_o.tgd'length - 1 downto 0) 
    , dat( wb_o.dat'length - 1 downto 0) 
    );
  begin
    wb_v := get_wb_rst( wb_v ); 
    wb_o := wb_v; 
  end procedure;

end package body wb_rst_pkg;
--#############################################################################
--#############################################################################
--#############################################################################
--#############################################################################
--#############################################################################
