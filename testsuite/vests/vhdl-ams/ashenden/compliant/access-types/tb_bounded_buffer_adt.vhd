
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity tb_bounded_buffer_adt is
end entity tb_bounded_buffer_adt;


architecture test of tb_bounded_buffer_adt is
begin

  process is

    use work.bounded_buffer_adt.all;

    variable buf : bounded_buffer := new_bounded_buffer(4);
    variable empty, full : boolean;
    variable d : byte;

  begin
    test_empty(buf, empty);
    assert empty;
    test_full(buf, full);
    assert not full;

    write(buf, X"01");
    write(buf, X"02");

    test_empty(buf, empty);
    assert not empty;
    test_full(buf, full);
    assert not full;

    write(buf, X"03");
    write(buf, X"04");

    test_empty(buf, empty);
    assert not empty;
    test_full(buf, full);
    assert full;

    write(buf, X"05");

    read(buf, d);
    read(buf, d);

    test_empty(buf, empty);
    assert not empty;
    test_full(buf, full);
    assert not full;

    read(buf, d);
    read(buf, d);

    test_empty(buf, empty);
    assert empty;
    test_full(buf, full);
    assert not full;

    read(buf, d);

    write(buf, X"06");
    write(buf, X"07");
    write(buf, X"08");
    read(buf, d);
    read(buf, d);
    write(buf, X"09");
    read(buf, d);
    write(buf, X"0A");
    read(buf, d);
    write(buf, X"0B");
    read(buf, d);
    write(buf, X"0C");
    read(buf, d);
    write(buf, X"0D");
    read(buf, d);
    write(buf, X"0E");
    read(buf, d);
    write(buf, X"0F");
    read(buf, d);

    wait;
  end process;

end architecture test;
