package pa is
 type t is range 0 to 3;
end package;

use work.pa.t;
package pb is
 function  f (a,b:t) return t      ;
 function "="(a,b:t) return boolean;
end package;

use work.pa."+";
-- This side note is not part of the bug report: with vhdl pre-2008 this use clause should be required (I think) and it is not
-- use work.pa."=";
package body pb is
 function  f (a,b:t) return t       is begin return a+b; end function;
 function "="(a,b:t) return boolean is begin return a=b; end function;
end package body;

use work.pa.all; -- fails with and without this use clause
use work.pb.f;
use work.pb."="; -- this causes the problem
entity e is begin
 assert f(1,2)=0 severity note;
end entity;
architecture a of e is begin end architecture;
