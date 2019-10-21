--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .
--                F U N C T I O N A L _ R E D U C T I O N .
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
--
--  This package provides the capability to recurse in parallel
--  using a work seeking approach and produce a result for an elementary type.
--  In addition, the recursion can be limited such that when attempting to
--  recurse deeper than the specified limit, the recursion item is "saved"
--  for later processing which will then be processed with a fresh stack,
--  thus ensuring that overflow below the limit does not occur.
--  NOTE: Also, this stack safe feature may be applied to a single worker
--  so it may be beneficial to use this generic even in cases where parallel
--  execution is not needed.
--  This version of the code is considered "safer" than the stack_limited
--  version because this version lets you specify the limit in terms of
--  stack size or as a percentage of the current stack size, intead of just
--  the recursive call depth. It is difficult to know if a call depth limit
--  would be contained on the stack, but for instance specifying the limit as
--  80% of the stack size should ensure that the recursion does not overflow.
------------------------------------------------------------------------------

pragma Warnings (Off, "*Reducer* is not referenced");
pragma Warnings (Off, "*Identity_Value* is not referenced");

generic

   type Result_Type is private;
   --  Final Result type

   with function Reducer (Left, Right : Result_Type) return Result_Type;
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Result_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the reducing result.

   type Work_Type is private;
   --  Data type to be processed recursively

package Parallel.Functional_Reducing_Stack_Safe_Recursion is

   type Parallelism_Manager is limited interface;

   function Execute_Parallel_Subprogram
     (Manager : in out Parallelism_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      --  Top level item to process recursively
      Process : not null access
        function (Item : Work_Type;
                  Stack_Limit : System.Address) return Result_Type;
      Stack_Deferrals : aliased out Natural
     --  Indicates the number of times workers had to defer their work
     --  in order to avoid stack overflow. Specifically, it indicates the
     --  number of times the Max_Depth threshold was crossed. This does not
     --  indicate a failure, only that you may want to increase the stack
     --  size next time, since hitting the stack limit does add extra
     --  processing which can significantly impact performance. We would
     --  like this to be an out parameter but we have to wait for Ada 2012
     --  for functions to have out parameters.
     ) return Result_Type
   is abstract;

   pragma Compile_Time_Warning
     (False,
      "GNAT Bug: " &
        "Garbage result if Stack_Deferrals is non-aliased out parameter");
end Parallel.Functional_Reducing_Stack_Safe_Recursion;

pragma Warnings (On, "*Reducer* is not referenced");
pragma Warnings (On, "*Identity_Value* is not referenced");
