
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

entity record_write is
end record_write;

architecture test0 of record_write is

  type record_structure is record
    a_boolean : boolean;
    a_bit : bit;
    a_character : character;
    a_severity : severity_level;
    a_string : string(0 to 10);
    a_integer : integer;
    a_real : real;
  end record;

  constant test_record : record_structure :=
    ( false,
      '1',
      'T',
      note,
      "Hello World",
      45,
      10.5
      );

  type record_file is file of record_structure;

begin
  doit: process
    file fileout : record_file open write_mode is "record_write.out";
  begin
    write(fileout,test_record);

    assert false
      report "PASSED TEST: record_write."
      severity note;
    wait;
  end process;

end test0;
