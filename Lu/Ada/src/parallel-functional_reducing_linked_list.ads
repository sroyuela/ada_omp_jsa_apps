------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .                              --
--      F U N C T I O N A L _ R E D U C I N G _ L I N K E D _ L I S T       --
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

--  A Reducing_Linked_List is a specialized bounded container used for
--  reducing results from multiple workers into a single result.
--  Each worker has its own entry in this list while it is actively working.
--  As each worker computes its result, its result is reduced into the workers
--  corresponding node in the list, then that result is reduced into the
--  node to the left, (if one exists) and the workers node is then unlinked
--  from the list. As a result, the reduction process is also computed in
--  parallel, rather than after all the workers have computed their results.
--
--  When all workers have completed their work, there will be one node
--  remaining in the list, the leftmost node.
--
--  The position of the workers entry in the list maintains the order of
--  operations to match the order that a sequential version of the code would
--  utilize.
--
--  If a worker seeks more work, then the workers node is reinserted back into
--  the list to the right or left of the offering node.
--  Typically, iterative parallelism would always insert to the right, whereas
--  recursive parallelism may insert to the right or left depending on which
--  sub-branch of recursion is being taken.
--
--  The structure utilizes an entry family in order to minimize contention
--  between workers. A single reduction operations mostly involves
--  acquiring the PO for the workers node and the node to the left of the
--  workers node.

with System;

generic
   type Element_Type is private;
   --  The type of the reduction result

   with function Reducer (Left, Right : Element_Type) return Element_Type;
   Identity_Value : Element_Type;

package Parallel.Functional_Reducing_Linked_List is

--   pragma Preelaborate;

   type List (Worker_Count : Positive_Worker_Count;
              Priority : System.Priority) is limited private;

   subtype Donor_Id is Worker_Count_Type;
   subtype Effective_Worker_Id is Donor_Id range 1 .. Donor_Id'Last;

   type Cursor is private;

   function Create
     (Worker_Count : Positive_Worker_Count;
      Priority : System.Priority) return List;
   --
   --  Returns a list will all nodes pre-linked into the list. This is
   --  useful for iterative generics which start with all work initially
   --  assigned to workers. For recursive generics, where work is assigned
   --  to one worker and grows recursively it is better to start with an
   --  empty list, which is what you start with if you dont make this call.

   function To_Cursor
     (Worker : Donor_Id) return Cursor;

   procedure Reduce
     (Container : in out List;
      Item : Element_Type;
      Position : Cursor);
   --  Performs a reduction of the value of 'Item' into the workers node,
   --  as the left operand using the current value of the node as the right
   --  operand. If there is a node to the left of the workers node then the
   --  resulting value is then reduced into the node to the immediate left of
   --  the workers node as the left operand where the right operand is the
   --  current value of that node. The workers node is then unlinked from the
   --  list.

   procedure Insert_Right
     (Container : in out List;
      Item, Position : Cursor);
   --  Inserts a workers node (the 'Item') right of the specified position
   --  in the reduction list. This is a non-blocking call.

   procedure Insert_Left
     (Container : in out List;
      Item, Position : Cursor);
   --  Inserts a workers node (the 'Item') left of the specified position
   --  in the reduction list. This is a non-blocking call.

   function Value
     (Container : List;
      Position : Cursor) return Element_Type;
   --  Returns the value of an element at a cursor

   procedure Result
     (Container : in out List;
      Reduction_Result : out Element_Type);
   --  Returns the final result of the reduction once all workers have
   --  reported their result. The call blocks until all the work is done.

--     procedure Dump
--       (Container : in out List;
--        Header : String := "");
--     --  A debugging routine that dumps the contents of the list to stdout.

   procedure Cancel
     (Container : in out List);

private

   type Node_Type;
   type Node_Access is access all Node_Type;
--   pragma Atomic (Node_Access);

   type Element_State is (Deleted, Available);

   type Node_Type is
      record
         Element : Element_Type;
         Next    : Node_Access;
         Prev    : Node_Access;
         State   : Element_State;
      end record;

   type Element_Array is
     array (Worker_Count_Type range <>) of aliased Node_Type;

   protected type List
     (Worker_Count : Positive_Worker_Count;
      Priority : System.Priority) is

      pragma Priority (Priority);

      procedure Reduce
        (Item : Element_Type;
         Source : Positive_Worker_Count);

      procedure Insert_Right (Item, Position : Positive_Worker_Count);
      procedure Insert_Left (Item, Position : Positive_Worker_Count);

      procedure Setup_All_Reductions;
      --  Initializes the list with all workers having work assigned.
      --  This is useful for initializing the list for the iterative generics,
      --  where all workers are loaded with work up front.

      entry Result (Item : out Element_Type);

--        procedure Dump (Header : String);

      function Value (Index  : Worker_Count_Type) return Element_Type;

      procedure Cancel;

   private

      Outstanding_Reductions : Worker_Count_Type := 0;
      Initialized : Boolean := False;
      Elements : Element_Array (0 .. Worker_Count)
        := (0 =>
              (Element => Identity_Value,
               Next => null,
               Prev => null,
               State => Available),
            others =>
              (Element => Identity_Value,
               Next => null,
               Prev => null,
               State => Deleted));
   end List;

   type Cursor is new Donor_Id;

   pragma Inline (To_Cursor);
end Parallel.Functional_Reducing_Linked_List;
