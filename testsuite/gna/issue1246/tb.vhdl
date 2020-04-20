--------------------------------------
-- use work.pkg_A.all;
--------------------------------------

entity tb is
end tb;

architecture arch of tb is
        --------------------------------------
	package pkgA is new work.pkg_A
		generic map (
			A 	=> 0
		);
        --------------------------------------
begin
	process
                --------------------------------------
		use pkgA.all;
                --------------------------------------
	begin
		showA;
		wait;
	end process;
end architecture;
