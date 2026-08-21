--  Ranges that must keep working: a null range is compatible with any index
--  subtype whatever its bounds (LRM08 5.2.1), and an in-range constraint is
--  of course fine.  Guards against the check rejecting valid designs.
entity ok_ranges is
  generic (n : integer := 0);
end;

architecture arch of ok_ranges is
  signal null_str  : string (1 to n);          --  null range when n = 0
  signal valid_str : string (4 downto n + 1);  --  4 downto 1
begin
  process
  begin
    report "null=" & integer'image (null_str'length)
      & " valid=" & integer'image (valid_str'length);
    wait;
  end process;
end;
