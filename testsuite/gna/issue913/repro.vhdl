entity repro is
end;

use work.apackage.all;

architecture arch of repro is
  signal r : t_record;
  signal v : t_dbyte;
begin
  v <= to_hkbyte(r);
end arch;
