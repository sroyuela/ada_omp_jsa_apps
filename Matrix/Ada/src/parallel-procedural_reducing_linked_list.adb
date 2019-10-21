------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .                              --
--      P R O C E D U R A L _ R E D U C I N G _ L I N K E D _ L I S T       --
--                                                                          --
--                                B o d y                                   --
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

package body Parallel.Procedural_Reducing_Linked_List is

   protected body List is

--        procedure Dump (Header : String) is
--           Item : Node_Type := Elements (Elements'First);
--           Index : Node_Access := Node_Access'First;
--        begin
--           if Header'Length > 0 then
--              Put_Line (Header & ' ');
--           end if;
--
--           Dump_Loop : loop
--              Put_Line ("Index=" & Node_Access'Image (Index) &
--                        ", state=" & Element_State'Image (State (Index)) &
--                        ", Prev=" & Node_Access'Image (Item.Prev) &
--                        ", Next=" & Node_Access'Image (Item.Next));
--              exit Dump_Loop when Item.Next = Node_Access'First;
--              Index := Item.Next;
--              Item := Elements (Item.Next);
--           end loop Dump_Loop;
--        end Dump;

      procedure Insert_Left (Item, Position : Positive_Worker_Count)
      is
         Insert_Point : Node_Access := Elements (Position)'Unchecked_Access;
         Item_To_Insert : constant Node_Access
           := Elements (Item)'Unchecked_Access;
      begin

         Item_To_Insert.all := Node_Type'(Next => Insert_Point,
                                          Prev => Insert_Point.Prev,
                                          Element => Identity_Value,
                                          State => Available);

         --  Left of Donor Node's next link needs to point to the
         --  Workless node.
         --  Note: This is safe to do because of the Guard of this entry
         --  and this being a protected action.
         Insert_Point.Prev.Next := Item_To_Insert;

         --  Adjust the Donor node's previous pointer.
         --  Note: It is safe to manipulate the previous pointer for
         --  the Donor node that hasn't been acquired, since that the
         --  Donor worker is blocked on this call.
         Insert_Point.Prev := Item_To_Insert;

         Outstanding_Reductions := Outstanding_Reductions + 1;

         --  Clear the Deletion Block, if it applies, and
         --  Allow the position to perform reductions.
         Insert_Point.State := Available;

      end Insert_Left;

      procedure Insert_Right (Item, Position : Positive_Worker_Count)
      is
         Insert_Point : Node_Access := Elements (Position)'Unchecked_Access;
         Item_To_Insert : constant Node_Access
           := Elements (Item)'Unchecked_Access;
      begin

         --  Initial Seek Right must occur before we're done
         Initialized := True;

         --  If there is a value in the specified position that is not the
         --  identity value, that means workers on the right of this node
         --  have already reduced into the Donor. That is work that needs
         --  to be to the right of the new insertion, so we take that value
         --  from the Donor, and put the identity value back into the Donor.
         Item_To_Insert.all := Node_Type'(Next => Insert_Point.Next,
                                          Prev => Insert_Point,
                                          Element => Insert_Point.Element,
                                          State => Available);

         --  The specified position's next link needs to point to the Workless
         --  node.
         --  Note: This is safe to do because this call is made by the worker
         --  associated with the position. That worker can't be doing anything
         --  else right now, and this is a protected action, so we shouldn't
         --  need to worry about the worker currently to the right.
         Insert_Point.Next := Item_To_Insert;
         Insert_Point.Element := Identity_Value;

         --  Adjust next nodes previous pointer, if there is a next node
         --  Note: It is safe to manipulate the previous pointer for a node
         --  that hasn't been acquired, since this is a protected action and
         --  nodes are inserted or deleted atomically.
         if Item_To_Insert.Next /= null then
            Item_To_Insert.Next.Prev := Item_To_Insert;
         end if;

         Outstanding_Reductions := Outstanding_Reductions + 1;

         --  Clear the Deletion Block, if it applies, and
         --  Allow the position to perform reductions.
         Insert_Point.State := Available;
      end Insert_Right;

      procedure Reduce
        (Item : in out Element_Type;
         Source : Positive_Worker_Count)
      is
         Reduction_Node : constant Node_Access :=
           Elements (Source)'Unchecked_Access;
      begin

         pragma Assert (Reduction_Node.State /= Deleted and then
                        Reduction_Node.Prev.State /= Deleted);

         Reduction_Node.State := Deleted;

         --  Adjust next nodes previous pointer, if there is a next node
         --  Note: It is safe to manipulate the previous pointer for
         --  node that hasn't been acquired, since that node would have
         --  had to first acquire the node that is being "deleted",
         --  which it could not since that node *has* been acquired by
         --  the reducing task.
         if Reduction_Node.Next /= null then
            Reduction_Node.Next.Prev := Reduction_Node.Prev;
         end if;

         --  Perform client requested reduction first with
         --  whatever value is already at that slot
         Reducer (Item, Reduction_Node.Element);

         --  Now perform reduction of deleted nodes value,
         Reducer (Reduction_Node.Prev.Element, Item);

         --  Adjust the previous nodes next pointer
         Reduction_Node.Prev.Next := Reduction_Node.Next;

         Outstanding_Reductions := Outstanding_Reductions - 1;
      end Reduce;

      entry Result
        (Item : out Element_Type)
           when Outstanding_Reductions = 0 and then Initialized is
      begin
         Item := Elements (Elements'First).Element;
      end Result;

      procedure Setup_All_Reductions is
         Previous_Index,
         Next_Index : Node_Access;
      begin
         for I in Elements'Range loop
            if I = Elements'Last then
               Next_Index := null;
            else
               Next_Index := Elements (I + 1)'Unchecked_Access;
            end if;

            if I = Elements'First then
               Previous_Index := null;
            else
               Previous_Index := Elements (I - 1)'Unchecked_Access;
            end if;

            Elements (I) := Node_Type'
              (Element => Identity_Value,
               Prev => Previous_Index,
               Next => Next_Index,
               State => Available);

         end loop;

         Outstanding_Reductions := Worker_Count;

         Initialized := True;

      end Setup_All_Reductions;

      function Value
        (Index : Worker_Count_Type) return Element_Type is
      begin
         return Elements (Index).Element;
      end Value;

   end List;

   function Create
     (Worker_Count : Positive_Worker_Count;
      Priority : System.Priority) return List is
   begin
      return New_List : List (Worker_Count,
                              Priority)
      do
         New_List.Setup_All_Reductions;
      end return;
   end Create;

--     procedure Dump
--       (Container : in out List;
--        Header : String := "") is
--     begin
--        Container.Dump (Header);
--     end Dump;

   procedure Insert_Left
     (Container : in out List;
      Item,
      Position : Cursor) is
   begin
      Container.Insert_Left
        (Positive_Worker_Count (Item), Positive_Worker_Count (Position));
   end Insert_Left;

   procedure Insert_Right
     (Container : in out List;
      Item,
      Position : Cursor) is
   begin
      Container.Insert_Right
        (Positive_Worker_Count (Item), Positive_Worker_Count (Position));
   end Insert_Right;

   procedure Reduce
     (Container : in out List;
      Item : in out Element_Type;
      Position : Cursor) is
   begin
      Container.Reduce (Item, Positive_Worker_Count (Position));
   end Reduce;

   procedure Result
     (Container : in out List;
      Reduction_Result : out Element_Type) is
   begin
      Container.Result (Reduction_Result);
   end Result;

   function To_Cursor
     (Worker : Donor_Id) return Cursor is
   begin
      return Cursor (Worker);
   end To_Cursor;

   procedure Value
     (Container : List;
      Position : Cursor;
      Result : out Element_Type) is
   begin
      Result := Container.Value (Worker_Count_Type (Position));
   end Value;

end Parallel.Procedural_Reducing_Linked_List;
