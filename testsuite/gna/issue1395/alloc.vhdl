--  The constraint of an allocator is elaborated by the same code.
entity alloc is
  generic (n : integer := 0);
end;

architecture arch of alloc is
  type str_p is access string;
begin
  process
    variable p : str_p;
  begin
    p := new string (4 downto n);
    report integer'image (p'length);
    wait;
  end process;
end;
