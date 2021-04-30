with STM_Board;    use STM_Board;
with Inverter_PWM;

package body Error_Handling is

   procedure Make_Safe is
   begin
      if not STM_Board.Is_Initialized then
         STM_Board.Initialize_GPIO;
      end if;

      --  Force the gate driver into a safe state
      Inverter_PWM.Safe_State;
      --  Signal error to the user
      STM_Board.Turn_On (Red_LED);
      STM_Board.Turn_Off (Green_LED);

   end Make_Safe;

end Error_Handling;
