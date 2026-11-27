--  The line tgingold identified on the thread:
--    new_ptr := new string'(1 to length => character'low);
--  in VUnit's string_ptr_pkg.  The aggregate is built before being copied
--  into the allocated object, and it is built on the stack.

entity repro is
  generic (length : natural := 16 * 1024 * 1024);   -- 16 MB, default stack is 8
end entity;

architecture a of repro is
  type string_acc is access string;
begin
  process
    variable p : string_acc;
  begin
    p := new string'(1 to length => character'low);
    report "allocated " & integer'image (p'length);
    deallocate (p);
    wait;
  end process;
end architecture;
