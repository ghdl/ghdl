library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.math_real.all;
    
package crc_pkg is

  type crcParam_t is record
    selLen  : integer; 
    poly    : std_ulogic_vector;
    iniVect : std_ulogic_vector; 
    refIn   : boolean;   
    refOut  : boolean;    
    xorOut  : std_ulogic_vector;
  end record;
  
  pure function getCrc32Param( stdCrc : string 
  ;                            datLen : integer   
  ) return crcParam_t;
  
end crc_pkg;

package body crc_pkg is

  pure function getCrc32Param( stdCrc : string
  ;                            datLen : integer                
  ) return crcParam_t is
    variable crcParam_v : crcParam_t( poly    ( 31 downto 0)  
                                    , iniVect ( 31 downto 0)
                                    , xorOut  ( 31 downto 0) );
  begin
    if ( "CRC-32/CCITT-FALSE" = stdCrc ) then
      crcParam_v.selLen  := datLen / 8  ; 
      crcParam_v.poly    := X"04C11DB7" ;
      crcParam_v.iniVect := X"FFFFFFFF" ; 
      crcParam_v.refIn   := true        ;
      crcParam_v.refOut  := true        ;
      crcParam_v.xorOut  := X"FFFFFFFF" ;
    else   
     crcParam_v.selLen  := datLen / 8  ; 
     crcParam_v.poly    := X"000000AF" ;
     crcParam_v.iniVect := X"00000000" ; 
     crcParam_v.refIn   := false       ;
     crcParam_v.refOut  := false       ;
     crcParam_v.xorOut  := X"00000000" ;
      assert false report 
      " Standard crc not implemented Yet."
      severity failure;
    end if;
    return crcParam_v;
  end function;
  
end package body crc_pkg;
