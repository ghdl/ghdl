--
--  File Name:         SortListPkg_int.vhd
--  Design Unit Name:  SortListPkg_int
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      jim@synthworks.com
--
--  Description:
--      Sorting utility for array of scalars
--        Uses protected type so as to shrink and expand the data structure
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date       Version    Description
--    06/2008:   0.1        Initial revision
--                          Numerous revisions for VHDL Testbenches and Verification
--    02/2009:   1.0        First Public Released Version
--    02/25/2009 1.1        Replaced reference to std_2008 with a reference to
--                          ieee_proposed.standard_additions.all ;
--    06/16/2010 1.2        Added EraseList parameter to to_array
--    3/2011     2.0        added inside as non protected type
--    6/2011     2.1        added sort as non protected type
--    4/2013     2013.04    No Changes
--    5/2013     2013.05    No changes of substance. 
--                          Deleted extra variable declaration in procedure remove
--    1/2014     2014.01    Added RevSort.  Added AllowDuplicate paramter to Add procedure
--    1/2015     2015.01    Changed Assert/Report to Alert
--    11/2016    2016.11    Revised Add.  When AllowDuplicate, add a matching value last.
--
--
--
--  Copyright (c) 2008 - 2016 by SynthWorks Design Inc.  All rights reserved.
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

use work.OsvvmGlobalPkg.all ; 
use work.AlertLogPkg.all ; 
use std.textio.all ;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
use ieee.std_logic_textio.all ;

-- comment out following 2 lines with VHDL-2008.  Leave in for VHDL-2002 
-- library ieee_proposed ;						          -- remove with VHDL-2008
-- use ieee_proposed.standard_additions.all ;   -- remove with VHDL-2008


package SortListPkg_int is
  -- with VHDL-2008, convert package to generic package
  -- convert subtypes ElementType and ArrayofElementType to generics
  -- package SortListGenericPkg is
  subtype ElementType is integer ;
  subtype ArrayofElementType is integer_vector ;

  impure function inside (constant E : ElementType; constant A : in ArrayofElementType) return boolean ;
  impure function sort (constant A : in ArrayofElementType) return ArrayofElementType ;
  impure function revsort (constant A : in ArrayofElementType) return ArrayofElementType ;

  type SortListPType is protected
    procedure add ( constant A : in ElementType ; constant AllowDuplicate : Boolean := FALSE ) ;
    procedure add ( constant A : in ArrayofElementType ) ;
    procedure add ( constant A : in ArrayofElementType ; Min, Max : ElementType ) ;
    procedure add ( variable A : inout SortListPType ) ;
    -- Count items in list
    impure function  count return integer ;
    impure function  find_index ( constant A : ElementType) return integer ;
    impure function inside (constant A : ElementType) return boolean ;
    procedure insert ( constant A : in ElementType; constant index : in integer := 1 ) ;
    impure function get ( constant index : in integer := 1 ) return ElementType ;
    procedure erase  ;
    impure function Empty return boolean ;
    procedure print ;

    procedure remove ( constant A : in ElementType ) ;
    procedure remove ( constant A : in ArrayofElementType ) ;
    procedure remove ( variable A : inout SortListPType ) ;

    impure function to_array (constant EraseList : boolean := FALSE) return ArrayofElementType ;
    impure function to_rev_array (constant EraseList : boolean := FALSE) return ArrayofElementType ;
  end protected SortListPType ;

end SortListPkg_int ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body SortListPkg_int is

  impure function inside (constant E : ElementType; constant A : in ArrayofElementType) return boolean is
  begin
    for i in A'range loop
      if E = A(i) then
        return TRUE ;
      end if ;
    end loop ;
    return FALSE ;
  end function inside ;
  
  type SortListPType is protected body
    type ListType ;
    type ListPointerType is access ListType ;
    type ListType is record
      A           : ElementType ;
      -- item_num    : integer ;
      NextPtr     : ListPointerType ;
      -- PrevPtr     : ListPointerType ;
    end record ;
    variable HeadPointer : ListPointerType := NULL ;
    -- variable TailPointer : ListPointerType := NULL ;

    procedure add ( constant A : in ElementType ; constant AllowDuplicate : Boolean := FALSE ) is
      variable CurPtr, tempPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        HeadPointer  := new ListType'(A, NULL) ;
      elsif A = HeadPointer.A then -- ignore duplicates
        if AllowDuplicate then
          tempPtr := HeadPointer ;
          HeadPointer   := new ListType'(A, tempPtr) ;
        end if ; 
      elsif A < HeadPointer.A  then
        tempPtr := HeadPointer ;
        HeadPointer   := new ListType'(A, tempPtr) ;
      else
        CurPtr := HeadPointer ;
        AddLoop : loop
          exit AddLoop when CurPtr.NextPtr = NULL ;
          exit AddLoop when A < CurPtr.NextPtr.A  ;
          if A = CurPtr.NextPtr.A then 
--            if AllowDuplicate then  -- changed s.t. insert at after match rather than before
--              exit AddLoop ;    -- insert 
--            else
            if not AllowDuplicate then 
              return ;  -- return without insert
            end if; 
          end if ; 
          CurPtr := CurPtr.NextPtr ;
        end loop AddLoop ;
        tempPtr := CurPtr.NextPtr ;
        CurPtr.NextPtr := new ListType'(A, tempPtr) ;
      end if ;
    end procedure add ;
    
    procedure add ( constant A : in ArrayofElementType ) is
    begin
      for i in A'range loop
        add(A(i)) ;
      end loop ;
    end procedure add ;

    procedure add ( constant A : in ArrayofElementType ; Min, Max : ElementType ) is
    begin
      for i in A'range loop
        if A(i) >= Min and A(i) <= Max then
          add(A(i)) ;
        end if ;
      end loop ;
    end procedure add ;

    procedure add ( variable A : inout SortListPType ) is
    begin
      for i in 1 to A.Count loop
        add(A.Get(i)) ;
      end loop ;
    end procedure add ;

    -- Count items in list
    impure function count return integer is
      variable result : positive := 1 ;
      variable CurPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        return 0 ;
      else
        CurPtr := HeadPointer ;
        loop
          exit when CurPtr.NextPtr = NULL ;
          result := result + 1 ;
          CurPtr := CurPtr.NextPtr ;
        end loop ;
        return result ;
      end if ;
    end function count ;

    impure function find_index (constant A : ElementType) return integer is
      variable result : positive := 2 ;
      variable CurPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        return 0 ;
      elsif A <= HeadPointer.A then
        return 1 ;
      else
        CurPtr := HeadPointer ;
        loop
          exit when CurPtr.NextPtr = NULL ;
          exit when A <= CurPtr.NextPtr.A ;
          result := result + 1 ;
          CurPtr := CurPtr.NextPtr ;
        end loop ;
        return result ;
      end if ;
    end function find_index ;

    impure function inside (constant A : ElementType) return boolean is
      variable CurPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        return FALSE ;
      end if ;
      if A = HeadPointer.A then
        return TRUE ;
      else
        CurPtr := HeadPointer ;
        loop
          exit when CurPtr.NextPtr = NULL ;
          exit when A < CurPtr.NextPtr.A  ;
          if A = CurPtr.NextPtr.A then
            return TRUE ;  -- exit
          end if;
          CurPtr := CurPtr.NextPtr ;
        end loop ;
      end if ;
      return FALSE ;
    end function inside ;


    procedure insert( constant A : in ElementType; constant index : in integer := 1 ) is
      variable CurPtr, tempPtr : ListPointerType ;
    begin
      if index <= 1 then
        tempPtr := HeadPointer ;
        HeadPointer   := new ListType'(A, tempPtr) ;
      else
        CurPtr := HeadPointer ;
        for i in 3 to index loop
          exit when CurPtr.NextPtr = NULL ; -- end of list
          CurPtr := CurPtr.NextPtr ;
        end loop ;
        tempPtr := CurPtr.NextPtr ;
        CurPtr.NextPtr := new ListType'(A, tempPtr) ;
      end if;
    end procedure insert ;

    impure function get ( constant index : in integer := 1 ) return ElementType is
      variable CurPtr : ListPointerType ;
    begin
      if index > Count then
        Alert(OSVVM_ALERTLOG_ID, "SortLIstPkg_int.get index out of range", FAILURE) ;
        return ElementType'left ;
      elsif HeadPointer = NULL then
        return ElementType'left ;
      elsif index <= 1 then
        return HeadPointer.A ;
      else
        CurPtr := HeadPointer ;
        for i in 2 to index loop
          CurPtr := CurPtr.NextPtr ;
        end loop ;
        return CurPtr.A ;
      end if;
    end function get ;


    procedure erase (variable CurPtr : inout ListPointerType ) is
    begin
      if CurPtr.NextPtr /= NULL then
        erase (CurPtr.NextPtr) ;
      end if ;
      deallocate (CurPtr) ;
    end procedure erase ;

    procedure erase is
    begin
      if HeadPointer /= NULL then
        erase(HeadPointer) ;
        -- deallocate (HeadPointer) ;
        HeadPointer := NULL ;
      end if;
    end procedure erase ;

    impure function Empty return boolean is
    begin
      return HeadPointer = NULL ;
    end Empty ;

    procedure print is
      variable buf : line ;
      variable CurPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        write (buf, string'("( )")) ;
      else
        CurPtr := HeadPointer ;
        write (buf, string'("(")) ;
        loop
          write (buf, CurPtr.A) ;
          exit when CurPtr.NextPtr = NULL ;
          write (buf, string'(", ")) ;
          CurPtr := CurPtr.NextPtr ;
        end loop ;
        write (buf, string'(")")) ;
      end if ;
      writeline(OUTPUT, buf) ;
    end procedure print ;

    procedure remove ( constant A : in ElementType ) is
      variable CurPtr, tempPtr : ListPointerType ;
    begin
      if HeadPointer = NULL then
        return ;
      elsif A = HeadPointer.A then
        tempPtr := HeadPointer ;
        HeadPointer := HeadPointer.NextPtr ;
        deallocate (tempPtr) ;
      else
        CurPtr := HeadPointer ;
        loop
          exit when CurPtr.NextPtr = NULL ;
          if A = CurPtr.NextPtr.A then
            tempPtr := CurPtr.NextPtr ;
            CurPtr.NextPtr := CurPtr.NextPtr.NextPtr ;
            deallocate (tempPtr) ;
            exit ;
          end if ;
          exit when A < CurPtr.NextPtr.A ;
          CurPtr := CurPtr.NextPtr ;
        end loop ;
      end if ;
    end procedure remove ;

    procedure remove ( constant A : in ArrayofElementType ) is
    begin
      for i in A'range loop
        remove(A(i)) ;
      end loop ;
    end procedure remove ;

    procedure remove ( variable A : inout SortListPType ) is
    begin
      for i in 1 to A.Count loop
        remove(A.Get(i)) ;
      end loop ;
    end procedure remove ;

    impure function to_array (constant EraseList : boolean := FALSE) return ArrayofElementType is
      variable result : ArrayofElementType(1 to Count) ;
    begin
      for i in 1 to Count loop
        result(i) := Get(i) ;
      end loop ;
      if EraseList then
        erase ;
      end if ;
      return result ;
    end function to_array ;

    impure function to_rev_array (constant EraseList : boolean := FALSE) return ArrayofElementType is
      variable result : ArrayofElementType(Count downto 1) ;
    begin
      for i in 1 to Count loop
        result(i) := Get(i) ;
      end loop ;
      if EraseList then
        erase ;
      end if ;
      return result ;
    end function to_rev_array ;

    end protected body SortListPType ;
 
 
  impure function sort (constant A : in ArrayofElementType) return ArrayofElementType is
    variable Result : SortListPType ;
  begin
    for i in A'range loop 
      Result.Add(A(i), TRUE) ;
    end loop ;
    return Result.to_array(EraseList => TRUE)  ; 
  end function sort ;

  impure function revsort (constant A : in ArrayofElementType) return ArrayofElementType is
    variable Result : SortListPType ;
  begin
    for i in A'range loop 
      Result.Add(A(i), TRUE) ;
    end loop ;
    return Result.to_rev_array(EraseList => TRUE)  ; 
  end function revsort ;
end SortListPkg_int ;

