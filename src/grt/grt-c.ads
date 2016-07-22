--  GHDL Run Time (GRT) - C interface.
--  Copyright (C) 2005 - 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

--  This package declares C types.
--  It is a really stripped down version of interfaces.C!
with System;

package Grt.C is
   pragma Preelaborate (Grt.C);

   --  Type void * and char *.
   subtype voids is System.Address;
   subtype chars is System.Address;

   --  Type size_t.
   type size_t is mod 2 ** Standard'Address_Size;

   --  Type int.  It is an alias on Integer for simplicity.
   subtype int is Integer;
   subtype long is Long_Integer;
   type unsigned is mod 2 ** Integer'Size;

   --  Low level memory management.
   procedure Free (Addr : System.Address);
   function Malloc (Size : size_t) return System.Address;
   function Realloc (Ptr : System.Address; Size : size_t)
                    return System.Address;

private
   pragma Import (C, Free);
   pragma Import (C, Malloc);
   pragma Import (C, Realloc);
end Grt.C;
