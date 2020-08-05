entity tb is
	generic (output_path : string := "");
end entity;
architecture bench of tb is
  type tb_cfg_t is record
    s1 : string;
    s2 : string;
  end record tb_cfg_t;

  impure function decode return tb_cfg_t is
  begin
	return (
		s1 => output_path&"a",
		s2 => "b"
	);
  end function decode;
begin
	proc : process begin
		std.env.finish;
	end process;
end bench;
