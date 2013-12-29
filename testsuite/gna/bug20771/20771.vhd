entity Jon is
end Jon;

use std.textio.all;

architecture Taylor of Jon is
begin

process is
  variable buf:line;
  variable s : string(1 to 1);

  variable fstatus : file_open_status;
  file readfile : text;	
  constant temp_string : string := "hello.txt";
begin
   file_open(fstatus, readfile, temp_string, read_mode);
   report "open " & file_open_status'image(fstatus) severity note;
   if fstatus = OPEN_OK then
--      while not endfile(readfile) 
      loop
         readline(readfile, buf);
         read(buf, s);
         report "s= "& s severity note;         
      end loop;
   end if;
   report "done" severity note;
   wait;
end process;

end Taylor;
