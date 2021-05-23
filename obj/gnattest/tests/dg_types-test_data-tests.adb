--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DG_Types.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body DG_Types.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Boolean_To_YN (Gnattest_T : in out Test);
   procedure Test_Boolean_To_YN_3d5779 (Gnattest_T : in out Test) renames Test_Boolean_To_YN;
--  id:2.2/3d577903f73df662/Boolean_To_YN/1/0/
   procedure Test_Boolean_To_YN (Gnattest_T : in out Test) is
   --  dg_types.ads:108:5:Boolean_To_YN
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert (Boolean_To_YN(false) = 'N', "Oops!");
      AUnit.Assertions.Assert (Boolean_To_YN(true) = 'Y', "Oops!");

--  begin read only
   end Test_Boolean_To_YN;
--  end read only


--  begin read only
   procedure Test_Clear_W_Bit (Gnattest_T : in out Test);
   procedure Test_Clear_W_Bit_3be3ae (Gnattest_T : in out Test) renames Test_Clear_W_Bit;
--  id:2.2/3be3aea25085716b/Clear_W_Bit/1/0/
   procedure Test_Clear_W_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:111:5:Clear_W_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);
      WD : Word_T := 2#11111111_11111111#;
   begin

      Clear_W_Bit(WD, 3);
      AUnit.Assertions.Assert (WD = 2#11101111_11111111#, "Clear_W_Bit.");
      Clear_W_Bit(WD, 3);
      AUnit.Assertions.Assert (WD = 2#11101111_11111111#, "Clear_W_Bit.");

--  begin read only
   end Test_Clear_W_Bit;
--  end read only


--  begin read only
   procedure Test_Flip_W_Bit (Gnattest_T : in out Test);
   procedure Test_Flip_W_Bit_d2d2b6 (Gnattest_T : in out Test) renames Test_Flip_W_Bit;
--  id:2.2/d2d2b6e658b2b202/Flip_W_Bit/1/0/
   procedure Test_Flip_W_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:112:5:Flip_W_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Flip_W_Bit;
--  end read only


--  begin read only
   procedure Test_Set_W_Bit (Gnattest_T : in out Test);
   procedure Test_Set_W_Bit_f63f6c (Gnattest_T : in out Test) renames Test_Set_W_Bit;
--  id:2.2/f63f6c8c25ceb56c/Set_W_Bit/1/0/
   procedure Test_Set_W_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:113:5:Set_W_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_W_Bit;
--  end read only


--  begin read only
   procedure Test_Test_W_Bit (Gnattest_T : in out Test);
   procedure Test_Test_W_Bit_7a11db (Gnattest_T : in out Test) renames Test_Test_W_Bit;
--  id:2.2/7a11dba379068970/Test_W_Bit/1/0/
   procedure Test_Test_W_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:114:5:Test_W_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Test_W_Bit;
--  end read only


--  begin read only
   procedure Test_Get_W_Bits (Gnattest_T : in out Test);
   procedure Test_Get_W_Bits_ea9a74 (Gnattest_T : in out Test) renames Test_Get_W_Bits;
--  id:2.2/ea9a74be9c46ca89/Get_W_Bits/1/0/
   procedure Test_Get_W_Bits (Gnattest_T : in out Test) is
   --  dg_types.ads:116:5:Get_W_Bits
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_W_Bits;
--  end read only


--  begin read only
   procedure Test_Get_DW_Bits (Gnattest_T : in out Test);
   procedure Test_Get_DW_Bits_a2bf67 (Gnattest_T : in out Test) renames Test_Get_DW_Bits;
--  id:2.2/a2bf6705dcb2799f/Get_DW_Bits/1/0/
   procedure Test_Get_DW_Bits (Gnattest_T : in out Test) is
   --  dg_types.ads:117:5:Get_DW_Bits
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_DW_Bits;
--  end read only


--  begin read only
   procedure Test_Test_DW_Bit (Gnattest_T : in out Test);
   procedure Test_Test_DW_Bit_799079 (Gnattest_T : in out Test) renames Test_Test_DW_Bit;
--  id:2.2/79907977bd025bc6/Test_DW_Bit/1/0/
   procedure Test_Test_DW_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:118:5:Test_DW_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Test_DW_Bit;
--  end read only


--  begin read only
   procedure Test_Clear_QW_Bit (Gnattest_T : in out Test);
   procedure Test_Clear_QW_Bit_f212ea (Gnattest_T : in out Test) renames Test_Clear_QW_Bit;
--  id:2.2/f212ea34daf43e93/Clear_QW_Bit/1/0/
   procedure Test_Clear_QW_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:119:5:Clear_QW_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Clear_QW_Bit;
--  end read only


--  begin read only
   procedure Test_Set_QW_Bit (Gnattest_T : in out Test);
   procedure Test_Set_QW_Bit_1b4331 (Gnattest_T : in out Test) renames Test_Set_QW_Bit;
--  id:2.2/1b4331029495556d/Set_QW_Bit/1/0/
   procedure Test_Set_QW_Bit (Gnattest_T : in out Test) is
   --  dg_types.ads:120:5:Set_QW_Bit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_QW_Bit;
--  end read only


--  begin read only
   procedure Test_Get_Lower_Byte (Gnattest_T : in out Test);
   procedure Test_Get_Lower_Byte_2c7e4b (Gnattest_T : in out Test) renames Test_Get_Lower_Byte;
--  id:2.2/2c7e4be0e15951e4/Get_Lower_Byte/1/0/
   procedure Test_Get_Lower_Byte (Gnattest_T : in out Test) is
   --  dg_types.ads:123:5:Get_Lower_Byte
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Lower_Byte;
--  end read only


--  begin read only
   procedure Test_Get_Upper_Byte (Gnattest_T : in out Test);
   procedure Test_Get_Upper_Byte_d89a4b (Gnattest_T : in out Test) renames Test_Get_Upper_Byte;
--  id:2.2/d89a4b734d8093ff/Get_Upper_Byte/1/0/
   procedure Test_Get_Upper_Byte (Gnattest_T : in out Test) is
   --  dg_types.ads:124:5:Get_Upper_Byte
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Upper_Byte;
--  end read only


--  begin read only
   procedure Test_Swap_Bytes (Gnattest_T : in out Test);
   procedure Test_Swap_Bytes_7d46a7 (Gnattest_T : in out Test) renames Test_Swap_Bytes;
--  id:2.2/7d46a7da5fb7f014/Swap_Bytes/1/0/
   procedure Test_Swap_Bytes (Gnattest_T : in out Test) is
   --  dg_types.ads:125:5:Swap_Bytes
--  end read only

      pragma Unreferenced (Gnattest_T);
      WD : Word_T := 16#1122#;
   begin

      AUnit.Assertions.Assert
        (Swap_Bytes(WD) = 16#2211#, "Byte swap error.");

--  begin read only
   end Test_Swap_Bytes;
--  end read only


--  begin read only
   procedure Test_Get_Bytes_From_Word (Gnattest_T : in out Test);
   procedure Test_Get_Bytes_From_Word_075975 (Gnattest_T : in out Test) renames Test_Get_Bytes_From_Word;
--  id:2.2/075975fd1f9f3187/Get_Bytes_From_Word/1/0/
   procedure Test_Get_Bytes_From_Word (Gnattest_T : in out Test) is
   --  dg_types.ads:126:5:Get_Bytes_From_Word
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Bytes_From_Word;
--  end read only


--  begin read only
   procedure Test_Word_From_Bytes (Gnattest_T : in out Test);
   procedure Test_Word_From_Bytes_ea4510 (Gnattest_T : in out Test) renames Test_Word_From_Bytes;
--  id:2.2/ea45105dc52ddee5/Word_From_Bytes/1/0/
   procedure Test_Word_From_Bytes (Gnattest_T : in out Test) is
   --  dg_types.ads:127:5:Word_From_Bytes
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Word_From_Bytes;
--  end read only


--  begin read only
   procedure Test_Byte_To_String (Gnattest_T : in out Test);
   procedure Test_Byte_To_String_d93915 (Gnattest_T : in out Test) renames Test_Byte_To_String;
--  id:2.2/d93915cf2befb78d/Byte_To_String/1/0/
   procedure Test_Byte_To_String (Gnattest_T : in out Test) is
   --  dg_types.ads:128:5:Byte_To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Byte_To_String;
--  end read only


--  begin read only
   procedure Test_Low_Byte_To_Char (Gnattest_T : in out Test);
   procedure Test_Low_Byte_To_Char_00e4b2 (Gnattest_T : in out Test) renames Test_Low_Byte_To_Char;
--  id:2.2/00e4b210debe9611/Low_Byte_To_Char/1/0/
   procedure Test_Low_Byte_To_Char (Gnattest_T : in out Test) is
   --  dg_types.ads:134:5:Low_Byte_To_Char
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Low_Byte_To_Char;
--  end read only


--  begin read only
   procedure Test_Byte_Arr_To_Unbounded (Gnattest_T : in out Test);
   procedure Test_Byte_Arr_To_Unbounded_8c1bb0 (Gnattest_T : in out Test) renames Test_Byte_Arr_To_Unbounded;
--  id:2.2/8c1bb0c360a7942b/Byte_Arr_To_Unbounded/1/0/
   procedure Test_Byte_Arr_To_Unbounded (Gnattest_T : in out Test) is
   --  dg_types.ads:135:5:Byte_Arr_To_Unbounded
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Byte_Arr_To_Unbounded;
--  end read only


--  begin read only
   procedure Test_Get_Data_Sensitive_Portion (Gnattest_T : in out Test);
   procedure Test_Get_Data_Sensitive_Portion_6df315 (Gnattest_T : in out Test) renames Test_Get_Data_Sensitive_Portion;
--  id:2.2/6df315e4f76f06cb/Get_Data_Sensitive_Portion/1/0/
   procedure Test_Get_Data_Sensitive_Portion (Gnattest_T : in out Test) is
   --  dg_types.ads:136:5:Get_Data_Sensitive_Portion
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Data_Sensitive_Portion;
--  end read only


--  begin read only
   procedure Test_Lower_Word (Gnattest_T : in out Test);
   procedure Test_Lower_Word_3da74d (Gnattest_T : in out Test) renames Test_Lower_Word;
--  id:2.2/3da74da3a565e72e/Lower_Word/1/0/
   procedure Test_Lower_Word (Gnattest_T : in out Test) is
   --  dg_types.ads:142:5:Lower_Word
--  end read only

      pragma Unreferenced (Gnattest_T);
      DW : Dword_T := 16#11223344#;
   begin

      AUnit.Assertions.Assert
        (Lower_Word(DW) = 16#3344#, "Lower_Word wrong");

--  begin read only
   end Test_Lower_Word;
--  end read only


--  begin read only
   procedure Test_Upper_Word (Gnattest_T : in out Test);
   procedure Test_Upper_Word_05c493 (Gnattest_T : in out Test) renames Test_Upper_Word;
--  id:2.2/05c4935ae3cbbbf7/Upper_Word/1/0/
   procedure Test_Upper_Word (Gnattest_T : in out Test) is
   --  dg_types.ads:143:5:Upper_Word
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Upper_Word(16#11223344#) = 16#1122#, "Upper_Word failed");

--  begin read only
   end Test_Upper_Word;
--  end read only


--  begin read only
   procedure Test_Dword_From_Two_Words (Gnattest_T : in out Test);
   procedure Test_Dword_From_Two_Words_3c389f (Gnattest_T : in out Test) renames Test_Dword_From_Two_Words;
--  id:2.2/3c389fe54f92a263/Dword_From_Two_Words/1/0/
   procedure Test_Dword_From_Two_Words (Gnattest_T : in out Test) is
   --  dg_types.ads:144:5:Dword_From_Two_Words
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Dword_From_Two_Words;
--  end read only


--  begin read only
   procedure Test_Dword_To_String (Gnattest_T : in out Test);
   procedure Test_Dword_To_String_55061d (Gnattest_T : in out Test) renames Test_Dword_To_String;
--  id:2.2/55061dad65fd6556/Dword_To_String/1/0/
   procedure Test_Dword_To_String (Gnattest_T : in out Test) is
   --  dg_types.ads:145:5:Dword_To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Dword_To_String;
--  end read only


--  begin read only
   procedure Test_String_To_Dword (Gnattest_T : in out Test);
   procedure Test_String_To_Dword_3c5214 (Gnattest_T : in out Test) renames Test_String_To_Dword;
--  id:2.2/3c52147c1eb36d6a/String_To_Dword/1/0/
   procedure Test_String_To_Dword (Gnattest_T : in out Test) is
   --  dg_types.ads:151:5:String_To_Dword
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_String_To_Dword;
--  end read only


--  begin read only
   procedure Test_Sext_Word_To_Dword (Gnattest_T : in out Test);
   procedure Test_Sext_Word_To_Dword_45b429 (Gnattest_T : in out Test) renames Test_Sext_Word_To_Dword;
--  id:2.2/45b429bd73d9b9ab/Sext_Word_To_Dword/1/0/
   procedure Test_Sext_Word_To_Dword (Gnattest_T : in out Test) is
   --  dg_types.ads:152:5:Sext_Word_To_Dword
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Sext_Word_To_Dword;
--  end read only


--  begin read only
   procedure Test_Lower_Dword (Gnattest_T : in out Test);
   procedure Test_Lower_Dword_d008b0 (Gnattest_T : in out Test) renames Test_Lower_Dword;
--  id:2.2/d008b0b4e18ac86d/Lower_Dword/1/0/
   procedure Test_Lower_Dword (Gnattest_T : in out Test) is
   --  dg_types.ads:155:5:Lower_Dword
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Lower_Dword;
--  end read only


--  begin read only
   procedure Test_Upper_Dword (Gnattest_T : in out Test);
   procedure Test_Upper_Dword_1042da (Gnattest_T : in out Test) renames Test_Upper_Dword;
--  id:2.2/1042da3a021bee1e/Upper_Dword/1/0/
   procedure Test_Upper_Dword (Gnattest_T : in out Test) is
   --  dg_types.ads:156:5:Upper_Dword
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Upper_Dword;
--  end read only


--  begin read only
   procedure Test_Qword_From_Two_Dwords (Gnattest_T : in out Test);
   procedure Test_Qword_From_Two_Dwords_e0cb2c (Gnattest_T : in out Test) renames Test_Qword_From_Two_Dwords;
--  id:2.2/e0cb2ccaa3d68d7d/Qword_From_Two_Dwords/1/0/
   procedure Test_Qword_From_Two_Dwords (Gnattest_T : in out Test) is
   --  dg_types.ads:157:5:Qword_From_Two_Dwords
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Qword_From_Two_Dwords;
--  end read only


--  begin read only
   procedure Test_Int_To_String (Gnattest_T : in out Test);
   procedure Test_Int_To_String_506364 (Gnattest_T : in out Test) renames Test_Int_To_String;
--  id:2.2/506364fd134f9088/Int_To_String/1/0/
   procedure Test_Int_To_String (Gnattest_T : in out Test) is
   --  dg_types.ads:161:5:Int_To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Int_To_String;
--  end read only


--  begin read only
   procedure Test_String_To_Integer (Gnattest_T : in out Test);
   procedure Test_String_To_Integer_fdacf8 (Gnattest_T : in out Test) renames Test_String_To_Integer;
--  id:2.2/fdacf80ee5f11633/String_To_Integer/1/0/
   procedure Test_String_To_Integer (Gnattest_T : in out Test) is
   --  dg_types.ads:168:5:String_To_Integer
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_String_To_Integer;
--  end read only


--  begin read only
   procedure Test_Decode_Dec_Data_Type (Gnattest_T : in out Test);
   procedure Test_Decode_Dec_Data_Type_056a82 (Gnattest_T : in out Test) renames Test_Decode_Dec_Data_Type;
--  id:2.2/056a821434fb65a9/Decode_Dec_Data_Type/1/0/
   procedure Test_Decode_Dec_Data_Type (Gnattest_T : in out Test) is
   --  dg_types.ads:171:5:Decode_Dec_Data_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Decode_Dec_Data_Type;
--  end read only


--  begin read only
   procedure Test_DG_Double_To_Long_Float (Gnattest_T : in out Test);
   procedure Test_DG_Double_To_Long_Float_2d8c8f (Gnattest_T : in out Test) renames Test_DG_Double_To_Long_Float;
--  id:2.2/2d8c8ff77fd9dc46/DG_Double_To_Long_Float/1/0/
   procedure Test_DG_Double_To_Long_Float (Gnattest_T : in out Test) is
   --  dg_types.ads:177:5:DG_Double_To_Long_Float
--  end read only

      pragma Unreferenced (Gnattest_T);
      DG_Dbl : Double_Overlay;
   begin
      DG_Dbl.Double_QW := 0;
      AUnit.Assertions.Assert (DG_Double_To_Long_Float(DG_Dbl) = 0.0, "DG Dbl to LF - Zero conversion error.");

      -- Example from https://en.wikipedia.org/wiki/IBM_hexadecimal_floating-point
      DG_Dbl.Double_QW := 2#1_100_0010_0111_0110_1010_0000_0000_0000_00000000_00000000_00000000_00000000#;
      AUnit.Assertions.Assert (DG_Double_To_Long_Float(DG_Dbl) = (-118.625), 
                              "DG Dbl to LF - Known value conversion error.");


--  begin read only
   end Test_DG_Double_To_Long_Float;
--  end read only


--  begin read only
   procedure Test_Long_Float_To_DG_Double (Gnattest_T : in out Test);
   procedure Test_Long_Float_To_DG_Double_8175dc (Gnattest_T : in out Test) renames Test_Long_Float_To_DG_Double;
--  id:2.2/8175dc59a873cf72/Long_Float_To_DG_Double/1/0/
   procedure Test_Long_Float_To_DG_Double (Gnattest_T : in out Test) is
   --  dg_types.ads:178:5:Long_Float_To_DG_Double
--  end read only

      pragma Unreferenced (Gnattest_T);
      LF     : Long_Float := 0.0;
      QW     : Qword_T;

   begin

      QW := Long_Float_To_DG_Double(LF);
      AUnit.Assertions.Assert (QW = 0, "LF to DG - Zero conversion error");

      LF := (-118.625);
      QW := Long_Float_To_DG_Double(LF);
      AUnit.Assertions.Assert (QW = 2#1_100_0010_0111_0110_1010_0000_0000_0000_00000000_00000000_00000000_00000000#,
                              "LF to DG Dbl - known value conversion error");

--  begin read only
   end Test_Long_Float_To_DG_Double;
--  end read only


--  begin read only
   procedure Test_Byte_To_Integer_8 (Gnattest_T : in out Test);
   procedure Test_Byte_To_Integer_8_6ac9f0 (Gnattest_T : in out Test) renames Test_Byte_To_Integer_8;
--  id:2.2/6ac9f0ecd0d28fa6/Byte_To_Integer_8/1/0/
   procedure Test_Byte_To_Integer_8 (Gnattest_T : in out Test) is
   --  dg_types.ads:181:5:Byte_To_Integer_8
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Byte_To_Integer_8;
--  end read only


--  begin read only
   procedure Test_Char_To_Byte (Gnattest_T : in out Test);
   procedure Test_Char_To_Byte_11390e (Gnattest_T : in out Test) renames Test_Char_To_Byte;
--  id:2.2/11390e3d9572d1a6/Char_To_Byte/1/0/
   procedure Test_Char_To_Byte (Gnattest_T : in out Test) is
   --  dg_types.ads:182:5:Char_To_Byte
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Char_To_Byte;
--  end read only


--  begin read only
   procedure Test_Byte_To_Char (Gnattest_T : in out Test);
   procedure Test_Byte_To_Char_7742da (Gnattest_T : in out Test) renames Test_Byte_To_Char;
--  id:2.2/7742da8507778a8d/Byte_To_Char/1/0/
   procedure Test_Byte_To_Char (Gnattest_T : in out Test) is
   --  dg_types.ads:183:5:Byte_To_Char
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Byte_To_Char;
--  end read only


--  begin read only
   procedure Test_Dword_To_Integer_32 (Gnattest_T : in out Test);
   procedure Test_Dword_To_Integer_32_e9169d (Gnattest_T : in out Test) renames Test_Dword_To_Integer_32;
--  id:2.2/e9169d27b9e53e9c/Dword_To_Integer_32/1/0/
   procedure Test_Dword_To_Integer_32 (Gnattest_T : in out Test) is
   --  dg_types.ads:184:5:Dword_To_Integer_32
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Dword_To_Integer_32;
--  end read only


--  begin read only
   procedure Test_Integer_32_To_Dword (Gnattest_T : in out Test);
   procedure Test_Integer_32_To_Dword_67c35b (Gnattest_T : in out Test) renames Test_Integer_32_To_Dword;
--  id:2.2/67c35b0907b09678/Integer_32_To_Dword/1/0/
   procedure Test_Integer_32_To_Dword (Gnattest_T : in out Test) is
   --  dg_types.ads:185:5:Integer_32_To_Dword
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Integer_32_To_Dword;
--  end read only


--  begin read only
   procedure Test_Integer_32_To_Phys (Gnattest_T : in out Test);
   procedure Test_Integer_32_To_Phys_0e179b (Gnattest_T : in out Test) renames Test_Integer_32_To_Phys;
--  id:2.2/0e179b57ce953892/Integer_32_To_Phys/1/0/
   procedure Test_Integer_32_To_Phys (Gnattest_T : in out Test) is
   --  dg_types.ads:186:5:Integer_32_To_Phys
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Integer_32_To_Phys;
--  end read only


--  begin read only
   procedure Test_Word_To_Integer_16 (Gnattest_T : in out Test);
   procedure Test_Word_To_Integer_16_a3a65b (Gnattest_T : in out Test) renames Test_Word_To_Integer_16;
--  id:2.2/a3a65b382bb1e065/Word_To_Integer_16/1/0/
   procedure Test_Word_To_Integer_16 (Gnattest_T : in out Test) is
   --  dg_types.ads:187:5:Word_To_Integer_16
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Word_To_Integer_16;
--  end read only


--  begin read only
   procedure Test_Integer_16_To_Word (Gnattest_T : in out Test);
   procedure Test_Integer_16_To_Word_40bf8b (Gnattest_T : in out Test) renames Test_Integer_16_To_Word;
--  id:2.2/40bf8b3b3af6c36c/Integer_16_To_Word/1/0/
   procedure Test_Integer_16_To_Word (Gnattest_T : in out Test) is
   --  dg_types.ads:188:5:Integer_16_To_Word
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Integer_16_To_Word;
--  end read only


--  begin read only
   procedure Test_Word_To_Unsigned_16 (Gnattest_T : in out Test);
   procedure Test_Word_To_Unsigned_16_d3e005 (Gnattest_T : in out Test) renames Test_Word_To_Unsigned_16;
--  id:2.2/d3e00505fea693e8/Word_To_Unsigned_16/1/0/
   procedure Test_Word_To_Unsigned_16 (Gnattest_T : in out Test) is
   --  dg_types.ads:189:5:Word_To_Unsigned_16
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Word_To_Unsigned_16;
--  end read only


--  begin read only
   procedure Test_Integer_64_To_Unsigned_64 (Gnattest_T : in out Test);
   procedure Test_Integer_64_To_Unsigned_64_f5c627 (Gnattest_T : in out Test) renames Test_Integer_64_To_Unsigned_64;
--  id:2.2/f5c627cc3d2eb8ab/Integer_64_To_Unsigned_64/1/0/
   procedure Test_Integer_64_To_Unsigned_64 (Gnattest_T : in out Test) is
   --  dg_types.ads:190:5:Integer_64_To_Unsigned_64
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Integer_64_To_Unsigned_64;
--  end read only


--  begin read only
   procedure Test_Unsigned_32_To_Integer (Gnattest_T : in out Test);
   procedure Test_Unsigned_32_To_Integer_5e891f (Gnattest_T : in out Test) renames Test_Unsigned_32_To_Integer;
--  id:2.2/5e891fba56f59941/Unsigned_32_To_Integer/1/0/
   procedure Test_Unsigned_32_To_Integer (Gnattest_T : in out Test) is
   --  dg_types.ads:191:5:Unsigned_32_To_Integer
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Unsigned_32_To_Integer;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end DG_Types.Test_Data.Tests;
