-- Types and functions for Haskell Primitives

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package \Prim\ is

  subtype \Int#\ is signed(31 downto 0);
  subtype \GHC.Types.Int\ is signed(31 downto 0);
  subtype \GHC.Types.Bool\ is std_logic;

  -- Primitive arithmetic operations
  function \GHC.Prim.==#\ ( a, b : \Int#\ ) return \GHC.Types.Bool\;
  function \GHC.Prim.<#\  ( a, b : \Int#\ ) return \GHC.Types.Bool\;
  function \GHC.Prim.-#\  ( a, b : \Int#\ ) return \Int#\;

  -- Data Constructor predicates: each takes a object and returns
  -- a boolean (i.e., tested with VHDL's if) that indicates whether the
  -- object was constructed with the given constructor
  function \is_GHC.Types.False\ (a : \GHC.Types.Bool\) return boolean;
  function \is_GHC.Types.True\ (a : \GHC.Types.Bool\) return boolean;
  function \is_GHC.Types.I#\ (a : \GHC.Types.Int\) return boolean;

  -- Data "deconstructor" procedures: split apart an algebraic data type
  -- into fields

  procedure \expand_GHC.Types.I#\ ( input : in  \GHC.Types.Int\;
                                    field1 : out \Int#\);

end \Prim\;

package body \Prim\ is

  function \GHC.Prim.==#\ ( a, b : \Int#\ ) return \GHC.Types.Bool\ is
  begin
    if a = b then
      return '1';
    else
      return '0';
    end if;
  end \GHC.Prim.==#\;

  function \GHC.Prim.<#\ ( a, b : \Int#\ ) return \GHC.Types.Bool\ is
  begin
    if a < b then
      return '1';
    else
      return '0';
    end if;
  end \GHC.Prim.<#\;

  function \GHC.Prim.-#\ ( a, b : \Int#\ ) return \Int#\ is
  begin
    return a - b;
  end \GHC.Prim.-#\;

  function \is_GHC.Types.False\ (a : \GHC.Types.Bool\) return boolean is
  begin
    return a = '0';
  end \is_GHC.Types.False\;

  function \is_GHC.Types.True\ (a : \GHC.Types.Bool\) return boolean is
  begin
    return a = '1';
  end \is_GHC.Types.True\;

  function \is_GHC.Types.I#\ (a : \GHC.Types.Int\) return boolean is
  begin
    return true;          -- Trivial: there's only one constructor
  end \is_GHC.Types.I#\;
  
  procedure \expand_GHC.Types.I#\ (
    input  : in  \GHC.Types.Int\;
    field1 : out \Int#\) is
  begin
    field1 := input;
  end \expand_GHC.Types.I#\;
  
end \Prim\;
