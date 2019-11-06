entity record_test is
  port (
    o : out integer
  );
end record_test;

architecture rtl of record_test is
  type t_record is record
    int  : integer;
  end record t_record;
  constant rec_constant : t_record := (int => 27.777 us / 83.333 ns);
  constant rec_constant2 : t_record := rec_constant;
begin
  o <= rec_constant2.int;
end rtl;
