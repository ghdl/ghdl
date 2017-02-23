--
--  File Increment:         TbNames.vhd
--  Design Unit Increment:  TbNames
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis          SynthWorks
--
--
--  Purpose
--      Test Names
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--
--  Revision History:
--    Date      Version    Description
--    
--
--
--  Copyright (c) 2010 - 2016 by SynthWorks Design Inc.  All rights reserved.
--

use std.textio.all ;
use work.TbNamesPkg.all ;

entity TbNames is
end entity TbNames ; 

architecture T1 of TbNames is 
  shared variable IncVar : IncrementPType ; 
begin
  main : process
    variable ErrorCount : integer ; 
  begin
    PrintNames ; 
    CallPrintNames ;
    report "Get: INSTANCE_NAME " & IncVar.Get'Instance_Name;
    wait;
  end process main ; 
end architecture T1 ; 
