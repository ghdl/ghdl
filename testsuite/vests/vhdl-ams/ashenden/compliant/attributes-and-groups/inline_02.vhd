
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

library project;

entity inline_02 is
end entity inline_02;


architecture test of inline_02 is
begin

  process is

    use project.mem_pkg;
    use project.mem_pkg.all;
    variable words : word_array(0 to 3);

  begin
    assert
    -- code from book (in text)
    mem_pkg'path_name = ":project:mem_pkg:"
    -- end code from book
    ;
    report mem_pkg'path_name;

    assert
    -- code from book (in text)
    word'path_name = ":project:mem_pkg:word"
    -- end code from book
    ;
    report word'path_name;

    assert
    -- code from book (in text)
    word_array'path_name = ":project:mem_pkg:word_array"
    -- end code from book
    ;

    report word_array'path_name;

    assert
    -- code from book (in text)
    load_array'path_name = ":project:mem_pkg:load_array"
    -- end code from book
    ;
    report load_array'path_name;

    load_array(words, "/dev/null");
    wait;
  end process;

end architecture test;
