--------------------------------
--  Copyright 1992-2001 Future Parallel
--  VLSI Design Lab
--  Library: VFP
--  Designer: Tim Pagden
--  Opened: 02.06.2001
--  Updated: 12.06.2001
--  DNH: T:/author/dnh/integer_class.dnh
--------------------------------

entity integer_class_tb is
--
-- tests for integer_class package
--
end integer_class_tb;

  use std.textio.all;
library vfp;
  use vfp.std_verification.all;
  use vfp.integer_class.all;

architecture tb0 of integer_class_tb is
  
begin

  process
    --variable num_chars : integer;
    variable j : integer;
    variable k : integer;
    variable tfi : boolean;
    --variable bit_width : integer;
    variable log_line : line;
    file log_file : text open write_mode is "intclass.log";

  begin
    debug("Starting tests...");
    log(log_file, "========  Start of integer_class tests  ========");
    debug(3);
    debug("int is ", 3);
    log(log_file, 3);
    log(log_file, "int is ", 3);
    --tb_log(log_file, "==== strlen tests...");
    --for i in 0 to 9 loop
    --  log(log_file, string_length(10 ** i));
    --  log(log_file, string_length(-(10 ** i)));
    --end loop;
    log(log_file, "==== binary_wordlength tests...");
    for i in 0 to 31 loop
      log(log_file, binary_wordlength(2 ** i));
      log(log_file, binary_wordlength(-(2 ** i)));
      --write(log_line, (2 ** i)); -- it's OK 2**31 -> -(2**31) automatically,
      -- presumably simulator does a shift operation internally!
    end loop;
    log(log_file, "==== integer string length tests ...");
    for i in 0 to 9 loop
      log(log_file, (integer'IMAGE(10 ** i)))'LENGTH;
      log(log_file, (integer'IMAGE(-(10 ** i))))'LENGTH;
    end loop;
    log(log_file, "==== next_greater_binary_power_minus_1 tests ...");
    for i in 0 to 31 loop
      log(log_file, next_greater_binary_power_minus_1(2 ** i));
      log(log_file, next_greater_binary_power_minus_1(-(2 ** i)));
    end loop;
    --tb_log(log_file, "==== is_factor_of_32 tests ...");
    --for i in 0 to 32 loop
    --  tfi := is_factor_of_32(i);
    --  if tfi then
    --    write(log_line, i);
    --    write(log_line, string'(" is a factor"));
    --    -- sprintf("i,s", +integer, string)
    --    -- unary + takes any operand and returns a string
    --    -- sprintf("i,s", integer, string)
    --    writeline(log_file, log_line);
    --  else
    --    write(log_line, i);
    --    write(log_line, string'("..."));
    --    writeline(log_file, log_line);
    --  end if;
    --end loop;
    write(log_line, string'("==== is_power_of_2 tests ..."));
    writeline(log_file, log_line);
    for i in 0 to 31 loop
      k := 2 ** i;
      tfi := is_power_of_2(k);
      if tfi then
        write(log_line, string'("Correct, "));
        write(log_line, k);
        write(log_line, string'(" is a power of 2"));
        writeline(log_file, log_line);
      else
        write(log_line, k);
        write(log_line, string'(" has not been captured as a power of 2 - it should be!"));
        writeline(log_file, log_line);
      end if;
    end loop;
    write(log_line, string'("==== log_2 tests ..."));
    writeline(log_file, log_line);
    for i in 0 to 31 loop
      k := (2 ** i)-1;
      j := log_2(k);
      write(log_line, k);
      write(log_line, string'(" , "));
      write(log_line, j);
      writeline(log_file, log_line);
      k := 2 ** i;
      j := log_2(k);
      write(log_line, k);
      write(log_line, string'(" , "));
      write(log_line, j);
      writeline(log_file, log_line);
      k := (2 ** i)+1;
      j := log_2(k);
      write(log_line, k);
      write(log_line, string'(" , "));
      write(log_line, j);
      writeline(log_file, log_line);
    end loop;
    log(log_file, "========  End of integer_class tests  ========");
    debug("Tests finished.");
    wait;
  end process;
  
end tb0;

configuration integer_class_tb_cfg_0 of integer_class_tb is
  for tb0
  end for;
end integer_class_tb_cfg_0;

