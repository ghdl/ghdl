
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: tc2484.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p01n02i02484ent IS
END c07s03b03x00p01n02i02484ent;

ARCHITECTURE c07s03b03x00p01n02i02484arch OF c07s03b03x00p01n02i02484ent IS
  signal t1, t2, t3, t4 : INTEGER := -1;
BEGIN
  TESTING: PROCESS
    constant ref    : INTEGER := 123;

    function FuncN1 return INTEGER is
      function FuncN2 return INTEGER is
        function FuncN3 return INTEGER is
          function FuncN4 return INTEGER is
            function FuncN5 return INTEGER is
              function FuncN6 return INTEGER is
                function FuncN7 return INTEGER is
                  function FuncN8 return INTEGER is
                    function FuncN9 return INTEGER is
                      function FuncN10 return INTEGER is
                        function FuncN11 return INTEGER is
                        begin
                          return ref;
                        end FuncN11;
                      begin
                        return FuncN11;
                      end FuncN10;
                    begin
                      return FuncN10;
                    end FuncN9;
                  begin
                    return FuncN9;
                  end FuncN8;
                begin
                  return FuncN8;
                end FuncN7;
              begin
                return FuncN7;
              end FuncN6;
            begin
              return FuncN6;
            end FuncN5;
          begin
            return FuncN5;
          end FuncN4;
        begin
          return FuncN4;
        end FuncN3;
      begin
        return FuncN3;
      end FuncN2;
    begin
      return FuncN2;
    end FuncN1;
    
    
    function Func1 return INTEGER is
    begin
      return 1;
    end Func1;

    function Func2(selector : BOOLEAN) return INTEGER is
    begin
      if selector then
        return 11;
      else
        return 13;
      end if;
    end Func2;

  BEGIN
    t1 <= func1;
    t2 <= func2(TRUE);
    t3 <= func2(FALSE);
    t4 <= funcN1;
    wait for 5 ns;
    assert NOT( t1=1 and t2=11 and t3=13 and t4=123 )
      report "***PASSED TEST: c07s03b03x00p01n02i02484"
      severity NOTE;
    assert ( t1=1 and t2=11 and t3=13 and t4=123 )
      report "***FAILED TEST: c07s03b03x00p01n02i02484 - Function call test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p01n02i02484arch;
