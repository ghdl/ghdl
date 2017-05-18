--
--  File Name:         TextUtilPkg.vhd
--  Design Unit Name:  TextUtilPkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      jim@synthworks.com
--
--
--  Description:
--        Shared Utilities for handling text files
--          
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date       Version    Description
--    01/2015:   2015.05    Initial revision
--    01/2016:   2016.01    Update for L.all(L'left)
--    11/2016:   2016.11    Added IsUpper, IsLower, to_upper, to_lower
--
--
--  Copyright (c) 2015-2016 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--
--  This source file is free software; you can redistribute it
--  and/or modify it under the terms of the ARTISTIC License
--  as published by The Perl Foundation; either version 2.0 of
--  the License, or (at your option) any later version.
--
--  This source is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the Artistic License for details.
--
--  You should have received a copy of the license with this source.
--  If not download it from,
--     http://www.perlfoundation.org/artistic_license_2_0
--

use std.textio.all ;
library ieee ; 
use ieee.std_logic_1164.all ; 

package TextUtilPkg is
  ------------------------------------------------------------
  function IsUpper (constant Char : character ) return boolean ;
  function IsLower (constant Char : character ) return boolean ;
  function to_lower (constant Char : character ) return character ;
  function to_lower (constant Str : string ) return string ;
  function to_upper (constant Char : character ) return character ;
  function to_upper (constant Str : string ) return string ;
  function ishex (constant Char : character ) return boolean ; 
  function isstd_logic (constant Char : character ) return boolean ;

  ------------------------------------------------------------
  procedure SkipWhiteSpace (
  ------------------------------------------------------------
    variable L     : InOut line ;
    variable Empty : out   boolean
  ) ;
  procedure SkipWhiteSpace (variable L : InOut line) ;
  
  ------------------------------------------------------------
  procedure EmptyOrCommentLine (
  ------------------------------------------------------------
    variable L                : InOut  line ;
    variable Empty            : InOut  boolean ;
    variable MultiLineComment : inout  boolean 
  ) ;
  
  ------------------------------------------------------------
  procedure ReadHexToken (
  -- Reads Upto Result'length values, less is ok.
  -- Does not skip white space
  ------------------------------------------------------------
    variable L      : InOut line ;
    variable Result : Out   std_logic_vector ;
    variable StrLen : Out   integer 
  ) ; 
    
  ------------------------------------------------------------
  procedure ReadBinaryToken (
  -- Reads Upto Result'length values, less is ok.
  -- Does not skip white space
  ------------------------------------------------------------
    variable L      : InOut line ;
    variable Result : Out   std_logic_vector ;
    variable StrLen : Out   integer 
  ) ;   

end TextUtilPkg ;
  
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body TextUtilPkg is
  constant LOWER_TO_UPPER_OFFSET : integer := character'POS('a') - character'POS('A') ;
  
  ------------------------------------------------------------
  function "-" (R : character ; L : integer ) return character is
  ------------------------------------------------------------
  begin
    return character'VAL(character'pos(R) - L) ;  
  end function "-" ; 
  
  ------------------------------------------------------------
  function "+" (R : character ; L : integer ) return character is
  ------------------------------------------------------------
  begin
    return character'VAL(character'pos(R) + L) ;  
  end function "+" ; 

  ------------------------------------------------------------
  function IsUpper (constant Char : character ) return boolean is
  ------------------------------------------------------------
  begin
    if Char >= 'A' and Char <= 'Z' then 
      return TRUE ; 
    else
      return FALSE ; 
    end if ; 
  end function IsUpper ; 
  
  ------------------------------------------------------------
  function IsLower (constant Char : character ) return boolean is
  ------------------------------------------------------------
  begin
    if Char >= 'a' and Char <= 'z' then 
      return TRUE ; 
    else
      return FALSE ; 
    end if ; 
  end function IsLower ;
 
  ------------------------------------------------------------
  function to_lower (constant Char : character ) return character is
  ------------------------------------------------------------
  begin
    if IsUpper(Char) then 
      return Char + LOWER_TO_UPPER_OFFSET ; 
    else
      return Char ; 
    end if ; 
  end function to_lower ;

  ------------------------------------------------------------
  function to_lower (constant Str : string ) return string is
  ------------------------------------------------------------
    variable result : string(Str'range) ;
  begin
    for i in Str'range loop 
      result(i) := to_lower(Str(i)) ;
    end loop ;
    return result ; 
  end function to_lower ;

  ------------------------------------------------------------
  function to_upper (constant Char : character ) return character is
  ------------------------------------------------------------
  begin
    if IsLower(Char) then 
      return Char - LOWER_TO_UPPER_OFFSET ; 
    else
      return Char ; 
    end if ; 
  end function to_upper ;

  ------------------------------------------------------------
  function to_upper (constant Str : string ) return string is
  ------------------------------------------------------------
    variable result : string(Str'range) ;
  begin
    for i in Str'range loop 
      result(i) := to_upper(Str(i)) ;
    end loop ;
    return result ; 
  end function to_upper ;

  ------------------------------------------------------------
  function ishex (constant Char : character ) return boolean is
  ------------------------------------------------------------
  begin
    if Char >= '0' and Char <= '9' then 
      return TRUE ; 
    elsif Char >= 'a' and Char <= 'f' then 
      return TRUE ; 
    elsif Char >= 'A' and Char <= 'F' then 
      return TRUE ; 
    else
      return FALSE ; 
    end if ; 
  end function ishex ; 
  
  ------------------------------------------------------------
  function isstd_logic (constant Char : character ) return boolean is
  ------------------------------------------------------------
  begin
    case Char is
      when 'U' | 'X' | '0' | '1' | 'Z' | 'W' | 'L' | 'H' | '-' => 
        return TRUE ; 
      when others =>
        return FALSE ; 
    end case ; 
  end function isstd_logic ;
  
--  ------------------------------------------------------------
--  function iscomment (constant Char : character ) return boolean is
--  ------------------------------------------------------------
--  begin
--    case Char is
--      when '#' | '/' | '-'  => 
--        return TRUE ; 
--      when others =>
--        return FALSE ; 
--    end case ; 
--  end function iscomment ;

  ------------------------------------------------------------
  procedure SkipWhiteSpace (
  ------------------------------------------------------------
    variable L     : InOut line ;
    variable Empty : out   boolean
  ) is
    variable Valid : boolean ;
    variable Char  : character ;
    constant NBSP  : CHARACTER := CHARACTER'val(160);  -- space character
  begin
    Empty := TRUE ; 
    WhiteSpLoop : while L /= null and L.all'length > 0 loop
      if (L.all(L'left) = ' ' or L.all(L'left) = NBSP or L.all(L'left) = HT) then
        read (L, Char, Valid) ;
        exit when not Valid ; 
      else
        Empty := FALSE ; 
        return ;
      end if ; 
    end loop WhiteSpLoop ;
  end procedure SkipWhiteSpace ;

  ------------------------------------------------------------
  procedure SkipWhiteSpace (
  ------------------------------------------------------------
    variable L     : InOut line 
  ) is
    variable Empty : boolean ;
  begin
    SkipWhiteSpace(L, Empty) ; 
  end procedure SkipWhiteSpace ;
  
  ------------------------------------------------------------
  -- Package Local 
  procedure FindCommentEnd (
  ------------------------------------------------------------
    variable L     : InOut line ;
    variable Empty : out   boolean ;
    variable MultiLineComment : inout boolean 
  ) is
    variable Valid : boolean ;
    variable Char  : character ;
  begin
    MultiLineComment := TRUE ; 
    Empty            := TRUE ; 
    FindEndOfCommentLoop : while L /= null and L.all'length > 1 loop
      read(L, Char, Valid) ; 
      if Char = '*' and L.all(L'left) = '/' then
        read(L, Char, Valid) ; 
        Empty            := FALSE ;
        MultiLineComment := FALSE ;
        exit FindEndOfCommentLoop ;
      end if ; 
    end loop ; 
  end procedure FindCommentEnd ;
  
  ------------------------------------------------------------
  procedure EmptyOrCommentLine (
  ------------------------------------------------------------
    variable L                : InOut  line ;
    variable Empty            : InOut  boolean ;
    variable MultiLineComment : inout  boolean 
  ) is
    variable Valid : boolean ;
    variable Next2Char  : string(1 to 2) ;
    constant NBSP  : CHARACTER := CHARACTER'val(160);  -- space character
  begin
    if MultiLineComment then 
      FindCommentEnd(L, Empty, MultiLineComment) ; 
    end if ; 
    
    EmptyCheckLoop : while not MultiLineComment loop 
      SkipWhiteSpace(L, Empty) ; 
      exit when Empty ; -- line null or 0 in length detected by SkipWhite
      
      Empty := TRUE ; 

      exit when L.all(L'left) = '#' ; -- shell style comment
      
      if L.all'length >= 2 then 
        if L'ascending then 
          Next2Char := L.all(L'left to L'left+1) ;
        else 
          Next2Char := L.all(L'left to L'left-1) ;
        end if; 
        exit when Next2Char = "//" ; -- C style comment
        exit when Next2Char = "--" ; -- VHDL style comment
        
        if Next2Char = "/*" then   -- C style multi line comment
          FindCommentEnd(L, Empty, MultiLineComment) ;
          exit when Empty ; 
          next EmptyCheckLoop ; -- Found end of comment, restart processing line
        end if ; 
      end if ; 
      
      Empty := FALSE ; 
      exit ; 
    end loop EmptyCheckLoop ;
  end procedure EmptyOrCommentLine ;
  
  ------------------------------------------------------------
  procedure ReadHexToken (
  -- Reads Upto Result'length values, less is ok.
  -- Does not skip white space
  ------------------------------------------------------------
    variable L      : InOut line ;
    variable Result : Out   std_logic_vector ;
    variable StrLen : Out   integer 
  ) is
    constant NumHexChars   : integer := (Result'length+3)/4 ;
    constant ResultNormLen : integer := NumHexChars * 4 ; 
    variable NextChar      : character ; 
    variable CharCount     : integer ; 
    variable ReturnVal     : std_logic_vector(ResultNormLen-1 downto 0) ;
    variable ReadVal       : std_logic_vector(3 downto 0) ; 
    variable ReadValid     : boolean ; 
  begin
    ReturnVal := (others => '0') ;
    CharCount := 0 ; 
    
    ReadLoop : while L /= null and L.all'length > 0 loop
      NextChar := L.all(L'left) ; 
      if ishex(NextChar) or NextChar = 'X' or NextChar = 'Z' then 
        hread(L, ReadVal, ReadValid) ; 
        ReturnVal := ReturnVal(ResultNormLen-5 downto 0) & ReadVal ; 
        CharCount := CharCount + 1 ; 
        exit ReadLoop when CharCount >= NumHexChars ; 
      elsif NextChar = '_' then 
        read(L, NextChar, ReadValid) ; 
      else
        exit ; 
      end if ; 
    end loop ReadLoop ;
    
    if CharCount >= NumHexChars then 
      StrLen := Result'length ; 
    else
      StrLen := CharCount * 4 ; 
    end if ; 
    
    Result := ReturnVal(Result'length-1 downto 0) ; 
  end procedure ReadHexToken ; 
    
  ------------------------------------------------------------
  procedure ReadBinaryToken (
  -- Reads Upto Result'length values, less is ok.
  -- Does not skip white space
  ------------------------------------------------------------
    variable L      : InOut line ;
    variable Result : Out   std_logic_vector ;
    variable StrLen : Out   integer 
  ) is
    variable NextChar       : character ; 
    variable CharCount      : integer ; 
    variable ReadVal        : std_logic ; 
    variable ReturnVal      : std_logic_vector(Result'length-1 downto 0) ;
    variable ReadValid      : boolean ; 
  begin
    ReturnVal := (others => '0') ;
    CharCount := 0 ; 
    
    ReadLoop : while L /= null and L.all'length > 0 loop
      NextChar := L.all(L'left) ; 
      if isstd_logic(NextChar) then 
        read(L, ReadVal, ReadValid) ; 
        ReturnVal := ReturnVal(Result'length-2 downto 0) & ReadVal ; 
        CharCount := CharCount + 1 ; 
        exit ReadLoop when CharCount >= Result'length ; 
      elsif NextChar = '_' then 
        read(L, NextChar, ReadValid) ; 
      else
        exit ; 
      end if ; 
    end loop ReadLoop ;
    
    StrLen := CharCount ; 
    Result := ReturnVal ;
  end procedure ReadBinaryToken ;   


end package body TextUtilPkg ;