entity tb is
end entity;
architecture bench of tb is
	constant kill_size : positive := 50331648;
	signal s : string(1 to kill_size);

	function fun return string is
		variable v : string(1 to kill_size);
	begin
		return "hello";
	end;
begin
	proc: process
		-- Segmentation fault occurs for variable too, but much faster
--		variable s : string(1 to kill_size);
	begin
--		s := fun;
		s <= fun;
		report "pass" severity failure;
	end process;
end bench;
