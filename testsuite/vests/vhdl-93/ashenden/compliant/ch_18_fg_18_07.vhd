
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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
-- $Id: ch_18_fg_18_07.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_18_07_a is
end entity fg_18_07_a;


architecture writer of fg_18_07_a is
begin

  process is
            type transform_file is file of real;
          file initial_transforms : transform_file open write_mode is "transforms.ini";
  begin
    for i in 1 to 50 loop
      write(initial_transforms, real(i));
    end loop;
    wait;
  end process;

end architecture writer;




entity fg_18_07 is
end entity fg_18_07;


architecture test of fg_18_07 is
begin

  process is

            -- code from book (in text)

            type transform_array is array (1 to 3, 1 to 3) of real;
          variable transform1, transform2 : transform_array;

          type transform_file is file of real;
          file initial_transforms : transform_file
            open read_mode is "transforms.ini";

          -- end code from book

          -- code from book (Figure 18-7)

          procedure read_transform ( file f : transform_file;
                                     variable transform : out transform_array ) is
          begin
            for i in transform'range(1) loop
              for j in transform'range(2) loop
                if endfile(f) then
                  report "unexpected end of file in read_transform - "
                    & "some array elements not read"
                    severity error;
                  return;
                end if;
                read ( f, transform(i, j) );
              end loop;
            end loop;
          end procedure read_transform;

          -- end code from book

  begin

    -- code from book (in text)

    read_transform ( initial_transforms, transform1 );
    read_transform ( initial_transforms, transform2 );

    -- end code from book

    wait;
  end process;

end architecture test;
