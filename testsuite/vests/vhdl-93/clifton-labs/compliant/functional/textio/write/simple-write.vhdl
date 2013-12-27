
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

entity simple_write is
end simple_write;

use std.textio.all;

architecture only of simple_write is
  type integer_file is file of integer;
begin  -- only
  doit: process
    file infile : integer_file open write_mode is "simple.file";
    variable v : integer;
  begin  -- process

    write( infile, 1 );

    write( infile, 2 );

    write( infile, 3 );

    write( infile, 4 );

    report "PASSED"
      severity NOTE;
    
    wait;
  end process;
end only;
