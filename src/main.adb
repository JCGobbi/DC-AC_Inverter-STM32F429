
with Ada.Real_Time; use Ada.Real_Time;

with StartUp;
with STM_Board;    use STM_Board;
with Inverter_PWM; use Inverter_PWM;

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

procedure Main is

begin

   --  Start-up GPIOs, ADCs, Timer and PWM
   StartUp.Initialize;

   --  Test if all peripherals correctly initialized
   while not StartUp.Is_Initialized  loop
         Turn_On (Red_LED);
         delay until Clock + Milliseconds (1000);  --  arbitrary
         Turn_Off (Red_LED);
         delay until Clock + Milliseconds (1000);  --  arbitrary
   end loop;

   --  Enable PWM gate drivers
   Set_PWM_Gate_Power (True);

   --  Start generating the sinusoid
   Start_PWM;

   --  Enter steady state
   loop
      Set_Toggle (Green_LED);
      delay until Clock + Milliseconds (1000);  --  arbitrary
   end loop;

end Main;
