
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

entity fopen_test_3 is
end fopen_test_3;

architecture test0 of fopen_test_3 is

  constant StringLength: integer := 16;
  constant NumOfStrings: integer := 5;
  
  subtype str16 is string (1 to StringLength);
  type string_table is array (1 to NumOfStrings) of str16;

  constant string_array: string_table :=
    ( "This is string 1"
      ,"__Hello  World__"
      ,"This is string " & "3"
      ,"_Bird is a word_"
      ,"_Goodbye (ciao)_"
      );

  type ft is file of string;

begin
  doit: process
    file file_desc : ft;
  begin
    file_open(file_desc, "fopen_test_3.out", write_mode);
    for i in NumOfStrings downto 1 loop
      write(file_desc, string_array(i));
    end loop;
    file_close(file_desc);

    file_open(file_desc, "fopen_test_3.out", write_mode);
    for i in 1 to NumOfStrings loop
      write(file_desc, string_array(i));
    end loop;
    file_close(file_desc);

    wait;
  end process; 

end test0;
