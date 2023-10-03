package repro1 is
  signal s : bit;

  impure function get_f_delay return bit;
end repro1;

package body repro1 is
	impure function get_f_delay return bit is
	begin
		return s'delayed(1 ns);
	end;
end;
