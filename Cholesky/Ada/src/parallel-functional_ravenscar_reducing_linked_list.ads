------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .                              --
--               F U N C T I O N A L _ R A V E N S C A R _
--                R E D U C I N G _ L I N K E D _ L I S T
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
--  For Ravenscar, it is assumed that all calls to this package are from
--  a protected operation. This package does not add any synchronization
--  protected.

pragma Profile (Ravenscar);

generic
   type Element_Type is private;
   --  The type of the reduction result

   with function Reducer (Left, Right : Element_Type) return Element_Type;
   Identity_Value : Element_Type;

package Parallel.Functional_Ravenscar_Reducing_Linked_List is

--   pragma Preelaborate;

   type List (Worker_Count : Positive_Worker_Count) is limited private;

   subtype Donor_Id is Worker_Count_Type;
   subtype Effective_Worker_Id is Donor_Id range 1 .. Donor_Id'Last;

   type Cursor is private;

   procedure Initialize
     (Container : in out List;
      Worker_Count : Positive_Worker_Count);
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

   function Result_Available
     (Container : List)
      return Boolean;
   --  Returns True if the reductions are all complete

   procedure Result
     (Container : List;
      Reduction_Result : out Element_Type)
   with Pre => Result_Available (Container);
   --  Returns the final result of the reduction once all workers have
   --  reported their result.

private

   type Node_Type;
   type Node_Access is access all Node_Type;
   --  pragma Atomic (Node_Access);

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

   type List (Worker_Count : Positive_Worker_Count) is
      record

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
      end record;

   type Cursor is new Donor_Id;

   function Result_Available
     (Container : List) return Boolean is
     (Container.Outstanding_Reductions = 0 and then Container.Initialized);

   function To_Cursor
     (Worker : Donor_Id) return Cursor is
     (Cursor (Worker));

   pragma Inline (To_Cursor);
end Parallel.Functional_Ravenscar_Reducing_Linked_List;
