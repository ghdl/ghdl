library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.MATH_REAL.all;
      
library work;
    use work.wb_rst_pkg.all;
    use work.memory_map_pkg.all;
    
entity memory_map is 
    generic
    (  yes_g        : std_logic    :='1'
    );
    port
    ( p_i : in    memory_map_i_t;
      p_o :   out memory_map_o_t      
    );    
    
end memory_map;
architecture rtl of memory_map  is 
  
 alias clk_i : std_ulogic is p_i.dmn.clk;
 alias rst_i : std_ulogic is p_i.dmn.rst;
    
  type reg_t is record
    p_o
    : memory_map_o_t
    ( wb_S2M
      (  tgd( 0 downto 0 )
       , dat( p_i.wb_M2S.dat'length - 1 downto 0 ) )
    , en( p_o.en'length - 1 downto 0 )
    , reg( 0 to p_o.en'length - 1 )( p_o.dat'length - 1 downto 0 )     
    );

  end record;
  
  signal a
  ,      r 
  : reg_t;  
  
begin

  p_o <= r.p_o;
    
  process( p_i, r )
    variable a_v : reg_t;
  begin    
    a_v := r;
    
    if rst_i = yes_g then
      set_wb_rst( a_v.p_o.wb_S2M );
    end if;
    
    a <= a_v;
  end process;
  
  process( clk_i )
  begin
    if rising_edge ( clk_i ) then
      r <= a;
    end if;
  end process;
end rtl;
--#######################################################################################
--#######################################################################################
--#######################################################################################
--#######################################################################################
--#######################################################################################
