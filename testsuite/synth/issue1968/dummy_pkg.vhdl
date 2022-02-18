library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.all;

package dummy_pkg is
	type wordarray is array (natural range<>) of std_logic_vector(31 downto 0);

    procedure p_csa (
        variable A:	in	std_logic_vector(31 downto 0);
        variable B:	in	std_logic_vector(31 downto 0);
        variable Ci:	in	std_logic_vector(31 downto 0);
        variable S:	out	std_logic_vector(31 downto 0);
        variable Co:	out	std_logic_vector(31 downto 0)
    );

    function f_csa (
        A : std_logic_vector(31 downto 0);
        B:	std_logic_vector(31 downto 0);
        Ci:	std_logic_vector(31 downto 0)
    ) return wordarray;

    procedure this_works (
        signal A_i : in std_logic_vector(31 downto 0);
        signal B_i : in std_logic_vector(31 downto 0);
        signal C_i : in std_logic_vector(31 downto 0);
        signal o : out std_logic_vector(31 downto 0)
    );

    procedure this_doesnt_work (
        signal A_i : in std_logic_vector(31 downto 0);
        signal B_i : in std_logic_vector(31 downto 0);
        signal C_i : in std_logic_vector(31 downto 0);
        signal o : out std_logic_vector(31 downto 0)
    );

end dummy_pkg;

package body dummy_pkg is

    procedure p_csa (
        variable A:	in	std_logic_vector(31 downto 0);
        variable B:	in	std_logic_vector(31 downto 0);
        variable Ci:	in	std_logic_vector(31 downto 0);
        variable S:	out	std_logic_vector(31 downto 0);
        variable Co:	out	std_logic_vector(31 downto 0)
        ) is
        variable Co_tmp : std_logic_vector(32 downto 0);
    begin
        S := A xor B xor Ci;
        Co_tmp := ((A and B) or (B and Ci) or (A and Ci)) & '0';
        Co := Co_tmp(31 downto 0);
    end procedure p_csa;

    function f_csa (
        A : std_logic_vector(31 downto 0);
        B:	std_logic_vector(31 downto 0);
        Ci:	std_logic_vector(31 downto 0)
    ) return wordarray is
        variable r : wordarray(1 downto 0);
        variable Co_tmp : std_logic_vector(32 downto 0);
    begin
        r(0) := A xor B xor Ci;
        Co_tmp := ((A and B) or (B and Ci) or (A and Ci)) & '0';
        r(1) := Co_tmp(31 downto 0);
        return r;
    end function;

    procedure this_works (
        signal A_i : in std_logic_vector(31 downto 0);
        signal B_i : in std_logic_vector(31 downto 0);
        signal C_i : in std_logic_vector(31 downto 0);
        signal o : out std_logic_vector(31 downto 0)
    ) is
        variable a : std_logic_vector(31 downto 0);
        variable b : std_logic_vector(31 downto 0);
        variable c : std_logic_vector(31 downto 0);
        variable r0 : wordarray(1 downto 0);
        variable s0 : std_logic_vector(31 downto 0);
        variable c0 : std_logic_vector(31 downto 0);
    begin
        a := A_i;
        b := B_i;
        c := C_i;
        r0 := f_csa(a, b, c);
        s0 := r0(0);
        c0 := r0(1);
        o <= s0 + c0;
    end procedure this_works;

    procedure this_doesnt_work (
        signal A_i : in std_logic_vector(31 downto 0);
        signal B_i : in std_logic_vector(31 downto 0);
        signal C_i : in std_logic_vector(31 downto 0);
        signal o : out std_logic_vector(31 downto 0)
    ) is
        variable a : std_logic_vector(31 downto 0);
        variable b : std_logic_vector(31 downto 0);
        variable c : std_logic_vector(31 downto 0);
        variable s0 : std_logic_vector(31 downto 0);
        variable c0 : std_logic_vector(31 downto 0);
    begin
        a := A_i;
        b := B_i;
        c := C_i;
        p_csa(a, b, c, s0, c0);
        o <= s0 + c0;
    end procedure this_doesnt_work;

end dummy_pkg;
