--  Not specific to STRING: any array type with a constrained index subtype
--  is checked the same way.
entity user_array is
  generic (n : integer := 0);
end;

architecture arch of user_array is
  type pos_vec is array (positive range <>) of integer;
  signal s : pos_vec (4 downto n);
begin
  process
  begin
    report integer'image (s'length);
    wait;
  end process;
end;
