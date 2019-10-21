------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--                 S T A C K _ S A F E _ R E C U R S I O N
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

--  Generic to use for parallel subprograms that do not generate a final result

generic
   type Work_Type is private;
   --  Data type to be processed recursively
package Parallel.Stack_Safe_Recursion is

   type Parallelism_Manager is limited interface;

   procedure Execute_Parallel_Subprogram
     (Manager : in out Parallelism_Manager;

      Item : Work_Type;
      --  Top level item to process recursively

      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process   : not null access
         procedure (Item : Work_Type;
                    Stack_Limit : System.Address);
      Stack_Deferrals : out Natural
      --  Indicates the number of times workers had to defer their work
      --  in order to avoid stack overflow. Specifically, it indicates the
      --  number of times the Max_Depth threshold was crossed. This does not
      --  indicate a failure, only that you may want to increase the stack
      --  size next time, since hitting the stack limit does add extra
      --  processing which can significantly impact performance. We would
      --  like this to be an out parameter but we have to wait for Ada 2012
      --  for functions to have out parameters.
     )
   is abstract;

end Parallel.Stack_Safe_Recursion;
