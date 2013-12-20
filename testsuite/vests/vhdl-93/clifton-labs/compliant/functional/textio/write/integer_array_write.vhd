
-- Copyright (C) Clifton Labs.  All rights reserved.

-- CLIFTON LABS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE
-- SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT
-- NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR NON-INFRINGEMENT.  CLIFTON LABS SHALL NOT BE
-- LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, RESULT
-- OF USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.

-- By using or copying this Software, Licensee agrees to abide by the
-- intellectual property laws, and all other applicable laws of the U.S.,
-- and the terms of this license.

-- You may modify, distribute, and use the software contained in this
-- package under the terms of the GNU General Public License as published
-- by the Free Software Foundation; version 2 of the License.

-- You should have received a copy of the GNU General Public License along
-- with this software; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

entity integer_array_write is
end integer_array_write;

architecture test0 of integer_array_write is

  type integer_array_type is array (0 to 9) of integer;

  constant integer_array : integer_array_type := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  type integer_array_file is file of integer_array_type;

begin
  doit: process
    file fileout : integer_array_file open write_mode is "integer_array_write.out";
  begin
    write(fileout, integer_array);

    assert false
      report "PASSED TEST: integer_array_write."
      severity note;
    wait;
  end process;

end test0;
