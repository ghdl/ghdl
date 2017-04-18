entity fp_write_read_issue is
end entity fp_write_read_issue ;
architecture doit of fp_write_read_issue is
  use std.textio.all ; 
  file MyFile : text ;
begin
  process 
    variable A, B : real ;
    variable buf : line ;   
  begin
    file_open(MyFile, "./fp_write_read_file_A.txt", WRITE_MODE) ;
    A := 5.0 / 7.0 ; 
    write(buf, A) ; 
    writeline(MyFile, buf) ;
    file_close(MyFile) ;
    swrite(buf, "A = ") ;
    write(buf, A) ; 
    writeline(OUTPUT, buf) ;
    file_close(MyFile) ;
    file_open(MyFile, "./fp_write_read_file_A.txt", READ_MODE) ;
    readline(MyFile, buf) ;
    read(buf, B);
    file_close(MyFile) ;
    swrite(buf, "B = ") ;
    write(buf, B) ; 
    writeline(OUTPUT, buf) ;
    Assert A = B report "Read value differs from write value"   severity failure ; 

    std.env.stop ; 
    wait ; 
  end process ;
end architecture doit ; 
