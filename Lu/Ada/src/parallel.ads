------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L                               --
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

--  This package is the parent package of a suite of generic subprograms
--  that facilitate adding parallelism to loops and recursive structures.
--
--  Both iterative and recursive generics provide two forms of parallelism,
--  Work-Sharing and Work-Seeking. Work-Sharing is a simple divide and
--  conquor strategy, whereas Work-Seeking extends Work-Sharing by allowing
--  workers to seek more work once the workers task has been completed.
--
--  The generics are further distinguished by whether a final result needs to
--  be obtained. The gathering of this final result is called reducing,
--  since results from multiple workers need to be reduced to a single final
--  result.
--
--  The reducing generics are then further distinguished by whether the
--  type of the result is an elementary type, or a composite type. Elementary
--  results are returned as function return values whereas composite reductions
--  compute the result as an in-out parameter to a procedure.
--
--  This package defines data types shared common to all the generics.

with System.Multiprocessors;

package Parallel is

   pragma Preelaborate;
--   pragma Remote_Types;

   --  CPUs available on target

   subtype CPU_Count is System.Multiprocessors.CPU;

   function Number_Of_CPUs return CPU_Count
     renames System.Multiprocessors.Number_Of_CPUs;

   subtype CPU is System.Multiprocessors.CPU;

   --  Worker Count available for parallel execution

   Max_Workers : constant := 1_024;  -- Implementation defined
   type Worker_Count_Type is range 0 .. Max_Workers;
   for Worker_Count_Type'Size use 16;

   subtype Positive_Worker_Count is
     Worker_Count_Type range 1 .. Worker_Count_Type'Last;

   subtype Worker_Id is Positive_Worker_Count;

   function Default_Worker_Count return Positive_Worker_Count;

   function Optimal_Worker_Count
     (Iterations : Positive) return Worker_Count_Type;
   --  Returns the optimal number of workers needed to process
   --  a specific number of iterations for a parallel loop
   --  assuming that each iteration involves roughly the same amount
   --  of work.

   Use_Optimal_Worker_Count : constant Worker_Count_Type := 0;
   --  A special value when specified for a Worker_Count parameter
   --  indicates the called subprogram should attempt to use an
   --  optimal value for worker count, based on other specified
   --  parameters of the call.

   Default_Chunk_Size : constant := 0;
   --  A special value when specified for a Chunk_Size parameter
   --  indicates that the generic should select a value that should give
   --  good results.

   function Determine_Chunk_Size
     (Workers : Worker_Count_Type := Use_Optimal_Worker_Count;
      Iterations : Positive;
      Requested_Chunk_Size : Natural := Default_Chunk_Size) return Positive;
   --  If the client doesn't wish to provide a chunk size, this function
   --  may be used to choose a value that should generally work quite well

   subtype Worker_Priority is System.Priority range
     System.Priority'First .. System.Priority'Last - 1;

   Default_Worker_Priority : constant Worker_Priority
     := -- Implementation defined
       System.Default_Priority;

   --  Work_Budget

   --  The number of times a worker may be assigned work
   subtype Work_Budget_Limit is Integer range -1 .. Integer'Last;
   Unlimited_Work_Budget : constant Work_Budget_Limit := -1;

   --  Worker Storage Size

   --  Implementation Defined
   Default_Worker_Storage_Size : constant := 16#200_000#;
   Minimum_Worker_Storage_Size : constant := 16#4_000#;

   --  Work Splitting

   type Split_Progress is (Pre_Split, Post_Split);

   --------------------------------------------
   --            W O R K   S E E K I N G
   --------------------------------------------

   --  Used for work seeking to indicate workers are looking for work.
   --  Note: This type is a record rather than just a boolean because
   --  by-reference semantics are needed when atomic variables as passed
   --  as parameters.
   --  RM C.6(12), C.6(18) do not specify whether Boolean types have
   --  reference semantics, so the options are to wrap the boolean in a
   --  record, or use Boolean, but pass using an access type to force
   --  reference semantics. The decision was made to use an enclosing
   --  record, so that parameter passing modes can be used, and makes it
   --  more clear that access values are not being squirreled away.
   type Work_Seeking_State is
      record
         Seeking_Work : Boolean;
      end record;
--   type Work_Seeking_State is new Boolean;
   pragma Atomic (Work_Seeking_State);

   -----------------------------------------------
   --              W O R K   S T E A L I N G
   -----------------------------------------------

   --  Used for work stealing as loop iterators. It is atomic because
   --  the work stealing manager needs to be able to see the progress
   --  of the clients loop, and to modify the loop terminator if
   --  another worker wishes to steal work. In places where atomicity
   --  is not needed, the Work_Stealing_Loop_Index may be used instead.
   --  It is an unconstrained Integer subtype to faciliate integration
   --  with client code that may be using other integer types for loop
   --  indexes.
   --  Note: This type is a record rather than just an integer because
   --  by-reference semantics are needed when atomic variables as passed
   --  as parameters.
   --  RM C.6(12), C.6(18) do not specify whether Integer types have
   --  reference semantics, so the options are to wrap the Integer in a
   --  record, or use Integer, but pass using an access type to force
   --  reference semantics. The decision was made to use an enclosing
   --  record, so that parameter passing modes can be used, and makes it
   --  more clear that access values are not being squirreled away.

   subtype Work_Stealing_Loop_Index is Integer;

   type Atomic_Loop_Index_Type is
      record
         Value : Work_Stealing_Loop_Index;
      end record;
   pragma Atomic (Atomic_Loop_Index_Type);

   procedure Next (Item : in out Atomic_Loop_Index_Type);
   function "<=" (L, R : Atomic_Loop_Index_Type) return Boolean;

   --  Defines the stack limit for the recursion as a percentage of the
   --  total available stack space

   type Stack_Percentage is range 1 .. 99;

   Default_Maximum_Stack_Depth : constant Stack_Percentage := 90;

   function Effective_Worker_Count
     (Workers : Worker_Count_Type;
      Iterations : Positive) return Positive_Worker_Count;
   --  Given a requested number of workers and iterations
   --  Returns the number of Workers that will be used.

private

   use System;

   function Debug_Logging return Boolean;

end Parallel;
