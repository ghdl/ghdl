
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
-- $Id: tc3124.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b01x02p01n01i03124ent_a IS
  generic ( socket_g1 : Boolean;
            socket_g2 : Bit;
            socket_g3 : character;
            socket_g4 : severity_level;
            socket_g5 : integer;
            socket_g6 : real;
            socket_g7 : time;
            socket_g8 : natural;
            socket_g9 : positive
            );
  port    ( socket_p1 : inout Boolean;
            socket_p2 : inout Bit;
            socket_p3 : inout character;
            socket_p4 : inout severity_level;
            socket_p5 : inout integer;
            socket_p6 : inout real;
            socket_p7 : inout time;
            socket_p8 : inout natural;
            socket_p9 : inout positive
            );
END c05s02b01x02p01n01i03124ent_a;

ARCHITECTURE c05s02b01x02p01n01i03124arch_a OF c05s02b01x02p01n01i03124ent_a IS

BEGIN
  socket_p1 <= socket_g1 after 22 ns;
  socket_p2 <= socket_g2 after 22 ns;
  socket_p3 <= socket_g3 after 22 ns;
  socket_p4 <= socket_g4 after 22 ns;
  socket_p5 <= socket_g5 after 22 ns;
  socket_p6 <= socket_g6 after 22 ns;
  socket_p7 <= socket_g7 after 22 ns;
  socket_p8 <= socket_g8 after 22 ns;
  socket_p9 <= socket_g9 after 22 ns;
END c05s02b01x02p01n01i03124arch_a;



ENTITY c05s02b01x02p01n01i03124ent IS
END c05s02b01x02p01n01i03124ent;

ARCHITECTURE c05s02b01x02p01n01i03124arch OF c05s02b01x02p01n01i03124ent IS
  component ic_socket
    generic ( socket_g1 : Boolean;
              socket_g2 : Bit;
              socket_g3 : character;
              socket_g4 : severity_level;
              socket_g5 : integer;
              socket_g6 : real;
              socket_g7 : time;
              socket_g8 : natural;
              socket_g9 : positive
              );
    port    ( socket_p1 : inout Boolean;
              socket_p2 : inout Bit;
              socket_p3 : inout character;
              socket_p4 : inout severity_level;
              socket_p5 : inout integer;
              socket_p6 : inout real;
              socket_p7 : inout time;
              socket_p8 : inout natural;
              socket_p9 : inout positive
              );
  end component;
  signal socket_p1 :  Boolean;
  signal socket_p2 :  Bit;
  signal socket_p3 :  character;
  signal socket_p4 :  severity_level;
  signal socket_p5 :  integer;
  signal socket_p6 :  real;
  signal socket_p7 :  time;
  signal socket_p8 :  natural;
  signal socket_p9 :  positive;
BEGIN
  instance : ic_socket
    generic map ( true,
                  '1',
                  '$',
                  warning,
                  -100002,
                  -9.999,
                  20 ns,
                  23423,
                  4564576
                  )
    port map    ( socket_p1,
                  socket_p2,
                  socket_p3,
                  socket_p4,
                  socket_p5,
                  socket_p6,
                  socket_p7,
                  socket_p8,
                  socket_p9
                  );

  TESTING: PROCESS
  BEGIN
    wait for 30 ns;
    assert NOT(    socket_p1 = true   and
                   socket_p2 = '1'      and
                   socket_p3 = '$'      and
                   socket_p4 = warning   and
                   socket_p5 = -100002   and
                   socket_p6 = -9.999   and
                   socket_p7 = 20 ns   and
                   socket_p8 = 23423   and
                   socket_p9 = 4564576   )
      report "***PASSED TEST: c05s02b01x02p01n01i03124"
      severity NOTE;
    assert (    socket_p1 = true   and
                socket_p2 = '1'      and
                socket_p3 = '$'      and
                socket_p4 = warning   and
                socket_p5 = -100002   and
                socket_p6 = -9.999   and
                socket_p7 = 20 ns   and
                socket_p8 = 23423   and
                socket_p9 = 4564576   )
      report "***FAILED TEST: c05s02b01x02p01n01i03124 - Positional association generic and port list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b01x02p01n01i03124arch;




configuration c05s02b01x02p01n01i03124cfg of c05s02b01x02p01n01i03124ent is
  for c05s02b01x02p01n01i03124arch
    for instance : ic_socket use entity work.c05s02b01x02p01n01i03124ent_a (c05s02b01x02p01n01i03124arch_a)
                               generic map ( true,
                                             '1',
                                             '$',
                                             warning,
                                             -100002,
                                             -9.999,
                                             20 ns,
                                             23423,
                                             4564576
                                             )
                               port map    ( socket_p1,
                                             socket_p2,
                                             socket_p3,
                                             socket_p4,
                                             socket_p5,
                                             socket_p6,
                                             socket_p7,
                                             socket_p8,
                                             socket_p9
                                             );
    end for;
  end for;
end c05s02b01x02p01n01i03124cfg;
