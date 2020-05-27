entity myentity2 is
end myentity2;

architecture arch of myentity2 is
  type myrecord is record
    a : bit_vector;
  end record;
  subtype myboundedrecord is myrecord(a(1 downto 0));
  type myarray is array (natural range <>) of myboundedrecord;

  signal s : myarray (1 downto 0);
begin
end arch;
