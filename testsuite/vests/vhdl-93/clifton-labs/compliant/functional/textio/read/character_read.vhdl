
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

entity character_read is
end character_read;

architecture test0 of character_read is
  type character_file is file of character;
  signal k : integer := 0;
begin
  doit: process
    file filein : character_file open read_mode is "character.file";
    variable  v : character;
  begin
    assert(endfile(filein) = false)
      report "End of file reached before expected."
      severity failure;

    read(filein,v); 

    assert(v = '1') severity failure;

    read(filein,v); 
    assert(v = 'A') severity failure;

    read(filein,v); 
    assert(v = '$') severity failure;

    read(filein,v); 
    assert(v = '+') severity failure;

    assert(endfile(filein));

    report "PASSED TEST: character_read."
      severity NOTE;
    wait;
  end process;

end test0;
