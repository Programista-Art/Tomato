{ Created by Programista Art }
{ https://github.com/Programista-Art }
{ https://dimitalart.pl/ }
{$MODE DELPHI}
{$H+}
{$APPTYPE CONSOLE}

program tomato;

uses
  SysUtils, fpexprpars, Classes, Windows, Math,
  Generics.Collections, Generics.Defaults;

type
  TVarType = (vtFloat, vtInt, vtBoolean, vtString, vtChar);

  TVariable = record
    VarType: TVarType;
    FloatValue: Extended;
    BoolValue: Boolean;
    StrValue: string;
    CharValue: char;
  end;

  TCodeBlock = array of string;

  TLoopState = record
    BreakCalled: Boolean;
    ContinueCalled: Boolean;
  end;

var
  Variables: TDictionary<string, TVariable>;
  CurrentLoopState: TLoopState;
  GlobalCodeLines: TStringList; // Global list for parsing

procedure ExecuteStatement(Code: string); forward;
procedure RunProgram(CodeLines: TStringList); forward;
procedure ExecuteCodeBlock(const Block: TCodeBlock); forward;

//Helper Functions

{ Function to remove comments }
function RemoveComments(Line: string): string;
var
  InString: Boolean;
  i: Integer;
begin
  Result := '';
  InString := False;
  for i := 1 to Length(Line) do
  begin
    if Line[i] = '''' then
      InString := not InString;
    if not InString and (Line[i] = '/') and (i < Length(Line)) and (Line[i+1] = '/') then
      Break;
    Result := Result + Line[i];
  end;
end;

{ Helper function to convert TVarType to string }
function VarTypeToString(VarType: TVarType): string;
begin
  case VarType of
    vtFloat: Result := 'float';
    vtInt: Result := 'int';
    vtBoolean: Result := 'bool';
    vtString: Result := 'string';
    vtChar: Result := 'char';
  else
    Result := 'UnknownType';
  end;
end;

function GetIntValue(VarName: string): Extended;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    if V.VarType = vtInt then
      Exit(V.FloatValue)
    else
      raise Exception.Create('Error: Attempt to read non-int value for variable ' + VarName);
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist.');
end;

procedure SetIntValue(VarName: string; Value: Extended);
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) and (V.VarType <> vtInt) then
  begin
    raise Exception.Create('Error: Cannot assign an int to a variable of type ' + VarTypeToString(V.VarType));
  end;

  // Create (or overwrite) a record
  V.VarType := vtInt;
  V.FloatValue := Round(Value);
  Variables.AddOrSetValue(VarName, V);
end;

// Variable Management Functions

function GetVariableDisplay(VarName: string): string;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    case V.VarType of
      vtFloat: Result := FloatToStr(V.FloatValue);
      vtInt: Result := FloatToStr(Round(V.FloatValue));
      vtBoolean: Result := BoolToStr(V.BoolValue, True);
      vtString: Result := V.StrValue;
      vtChar: Result := V.CharValue;
    end;
    Exit;
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist.');
end;

function GetFloatValue(VarName: string): Extended;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    if V.VarType = vtFloat then
      Exit(V.FloatValue)
    else
      raise Exception.Create('Error: Attempt to read non-float value as float for variable ' + VarName);
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist.');
end;

function GetBooleanValue(VarName: string): Boolean;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    if V.VarType = vtBoolean then
      Exit(V.BoolValue)
    else
      raise Exception.Create('Error: Attempt to read non-boolean value for variable ' + VarName);
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist');
end;

function GetStringValue(VarName: string): string;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    if V.VarType = vtString then
      Exit(V.StrValue)
    else
      raise Exception.Create('Error: Attempt to read non-string value for variable ' + VarName);
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist.');
end;

function GetCharValue(VarName: string): Char;
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) then
  begin
    if V.VarType = vtChar then
      Exit(V.CharValue)
    else
      raise Exception.Create('Error: Attempt to read non-char value for variable ' + VarName);
  end;
  raise Exception.Create('Error: Variable ' + VarName + ' does not exist.');
end;

// Set Variable Functions

procedure SetFloatValue(VarName: string; Value: Extended);
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) and (V.VarType <> vtFloat) then
  begin
    raise Exception.Create('Error: Cannot assign a float to a variable of type ' + VarTypeToString(V.VarType));
  end;
  V.VarType := vtFloat;
  V.FloatValue := Value;
  Variables.AddOrSetValue(VarName, V);
end;

procedure SetBooleanValue(VarName: string; Value: Boolean);
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) and (V.VarType <> vtBoolean) then
  begin
    raise Exception.Create('Error: Cannot assign a boolean to a variable of type ' + VarTypeToString(V.VarType));
  end;
  V.VarType := vtBoolean;
  V.BoolValue := Value;
  Variables.AddOrSetValue(VarName, V);
end;

procedure SetStringValue(VarName: string; Value: string);
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) and (V.VarType <> vtString) then
  begin
    raise Exception.Create('Error: Cannot assign a string to a variable of type ' + VarTypeToString(V.VarType));
  end;
  V.VarType := vtString;
  V.StrValue := Value;
  Variables.AddOrSetValue(VarName, V);
end;

procedure SetCharValue(VarName: string; Value: Char);
var
  V: TVariable;
begin
  if Variables.TryGetValue(VarName, V) and (V.VarType <> vtChar) then
  begin
    raise Exception.Create('Error: Cannot assign a char to a variable of type ' + VarTypeToString(V.VarType));
  end;
  V.VarType := vtChar;
  V.CharValue := Value;
  Variables.AddOrSetValue(VarName, V);
end;

//Expression Evaluation

procedure AddVariablesToParser(Parser: TFPExpressionParser);
var
  Pair: TPair<string, TVariable>;
begin
  for Pair in Variables do
  begin
    if (Pair.Value.VarType = vtFloat) or (Pair.Value.VarType = vtInt) then
    Parser.Identifiers.AddFloatVariable(Pair.Key, Pair.Value.FloatValue);
  end;
end;

function Eval(Expression: string): Extended;
var
  Parser: TFPExpressionParser;
  Res: TFPExpressionResult;
begin
  Parser := TFPExpressionParser.Create(nil);
  try
    AddVariablesToParser(Parser);
    Parser.Expression := Expression;
    Res := Parser.Evaluate;
    case Res.ResultType of
      rtInteger: Result := Res.ResInteger;
      rtFloat: Result := Res.ResFloat;
      rtBoolean: if Res.ResBoolean then Result := 1 else Result := 0;
    else
      raise Exception.Create('Error: Invalid expression.');
    end;
  finally
    Parser.Free;
  end;
end;

function EvaluateTextExpression(Expression: string): string;
var
  InnerExpr: string;
  Parts: TStringList;
  i, StartIndex: Integer;
  Operand: string;
  ResultStr: string;
begin
  Expression := Trim(Expression);

  if (Length(Expression) >= 2) and (Expression[1] = '(') and (Expression[Length(Expression)] = ')') then
    InnerExpr := Copy(Expression, 2, Length(Expression) - 2)
  else
    InnerExpr := Expression;

  Parts := TStringList.Create;
  try
    StartIndex := 1;
    for i := 1 to Length(InnerExpr) do
    begin
      if InnerExpr[i] = '+' then
      begin
        Parts.Add(Copy(InnerExpr, StartIndex, i - StartIndex));
        StartIndex := i + 1;
      end;
    end;
    Parts.Add(Copy(InnerExpr, StartIndex, Length(InnerExpr) - StartIndex + 1));

    ResultStr := '';
    for i := 0 to Parts.Count - 1 do
    begin
      Operand := Trim(Parts[i]);
      if (Length(Operand) >= 2) and (Operand[1] = '''') and (Operand[Length(Operand)] = '''') then
        Operand := Copy(Operand, 2, Length(Operand) - 2)
      else
        Operand := GetVariableDisplay(Operand); // Get variable value
      ResultStr := ResultStr + Operand;
    end;
  finally
    Parts.Free;
  end;

  Result := ResultStr;
end;

//Type Conversion Functions

function StrToFloat(const s: string): Extended;
var
  Value: Extended;
begin
  if not TryStrToFloat(s, Value) then
    raise Exception.Create('Conversion Error StrToFloat: Invalid number format: ' + s);
  Result := Value;
end;

function FloatToStr(const n: Extended): string;
begin
  Result := FloatToStr(n);
end;

// Block Parsing and Execution

{ Parses a block of code from a list of lines }
function ParseCodeBlock(const CodeLines: TStringList; var StartIndex: Integer;
  const EndKeyword: string; const StartKeyword: string = ''): TCodeBlock;
var
  Block: TCodeBlock;
  i: Integer;
  Line: String;
  NestLevel: Integer;
begin
  SetLength(Block, 0);
  i := StartIndex;
  NestLevel := 0;

  while i < CodeLines.Count do
  begin
    Line := Trim(RemoveComments(CodeLines[i]));

    // Handle nested blocks
    if (StartKeyword <> '') and (Pos(StartKeyword, LowerCase(Line)) = 1) then
      Inc(NestLevel);

    if (Pos(EndKeyword, LowerCase(Line)) = 1) then
    begin
      if NestLevel = 0 then
      begin
         StartIndex := i;
        Exit(Block);
      end
      else
        Dec(NestLevel); // End of a nested block
    end;

    SetLength(Block, Length(Block) + 1);
    Block[High(Block)] := Line;
    Inc(i);
  end;

  // If we are here, the end keyword was not found
  if EndKeyword <> '' then
    raise Exception.Create('Error: Expected "' + EndKeyword + '" to end code block.');

  StartIndex := i;
  Exit(Block);
end;

{ Executes a pre-parsed block of code }
procedure ExecuteCodeBlock(const Block: TCodeBlock);
var
  i: Integer;
  Lines: TStringList;
begin
  // We must convert TCodeBlock (array of string) to TStringList for RunProgram
  Lines := TStringList.Create;
  try
    for i := 0 to High(Block) do
      Lines.Add(Block[i]);

    // Call RunProgram to execute the block, which correctly handles nested blocks
    RunProgram(Lines);
  finally
    Lines.Free;
  end;
end;

{ Main program execution loop }
procedure RunProgram(CodeLines: TStringList);
var
  i: Integer;
  Line, Condition, ThenPart, ElsePart: string;
  LoopBlock, IfBlock, ElseBlock: TCodeBlock;
  posThen, posElse: Integer;
  bCondition: Boolean;
begin
  i := 0;
  while i < CodeLines.Count do
  begin
    Line := Trim(RemoveComments(CodeLines[i]));
    Inc(i); // Consume the line immediately

    if Line = '' then Continue;

    // IF Statement ---
    if LowerCase(Copy(Line, 1, 2)) = 'if' then
    begin
      posThen := Pos('then', LowerCase(Line));
      if posThen = 0 then
        raise Exception.Create('Error: Missing "then" in "if" statement.');

      Condition := Trim(Copy(Line, 3, posThen - 3));

      // Parse the 'if' block (all lines until 'else' or 'end_if')
      IfBlock := ParseCodeBlock(CodeLines, i, 'else', 'if');

     if (i < CodeLines.Count) and (LowerCase(Trim(RemoveComments(CodeLines[i]))) = 'else') then
      begin
        Inc(i); // Consume 'else'
        ElseBlock := ParseCodeBlock(CodeLines, i, 'end_if', 'if');
        // 'i' teraz wskazuje na linię 'end_if'
        if (i < CodeLines.Count) then // <-- DODAJ TO
           Inc(i);
      end
      else
      begin
        SetLength(ElseBlock, 0); // No else block
        // We must still find the 'end_if'
        if (i < CodeLines.Count) and (LowerCase(Trim(RemoveComments(CodeLines[i]))) <> 'end_if') then
           Inc(i) // Skonsumuj 'end_if'
        else
           raise Exception.Create('Error: Expected "end_if" or "else".');
        //if (i < CodeLines.Count) then Inc(i); // Consume 'end_if'
      end;

      // Evaluate condition
      if LowerCase(Condition) = 'true' then bCondition := True
      else if LowerCase(Condition) = 'false' then bCondition := False
      else bCondition := (Eval(Condition) <> 0);

      // Execute the correct block
      if bCondition then
        ExecuteCodeBlock(IfBlock)
      else
        ExecuteCodeBlock(ElseBlock);
    end

    // WHILE Loop
    else if LowerCase(Copy(Line, 1, 5)) = 'while' then
    begin
      posThen := Pos('do', LowerCase(Line)); // 'while ... do'
      if posThen = 0 then
        raise Exception.Create('Error: Missing "do" in "while" loop.');

      Condition := Trim(Copy(Line, 6, posThen - 6));
      LoopBlock := ParseCodeBlock(CodeLines, i, 'end_while', 'while');

      //CurrentLoopState.BreakCalled := False;
      //if (i < CodeLines.Count) then // <-- DODAJ TO
      //   Inc(i); // <-- DODAJ TO (aby skonsumować 'end_while')

      CurrentLoopState.ContinueCalled := False;

      while (Eval(Condition) <> 0) and (not CurrentLoopState.BreakCalled) do
      begin
        ExecuteCodeBlock(LoopBlock);
        if CurrentLoopState.ContinueCalled then
        begin
          CurrentLoopState.ContinueCalled := False;
          Continue;
        end;
      end;
      CurrentLoopState.BreakCalled := False;
      if (i < CodeLines.Count) then
         Inc(i);
      CurrentLoopState.BreakCalled := False; // Reset state
    end

// REPEAT Loop
else if LowerCase(Copy(Line, 1, 6)) = 'repeat' then
begin
  LoopBlock := ParseCodeBlock(CodeLines, i, 'until', 'repeat');
  if i < CodeLines.Count then
  begin
    Condition := Trim(Copy(RemoveComments(CodeLines[i]), 6, 255));
    Inc(i);
  end
  else
    raise Exception.Create('Error: Missing "until" condition.');

  CurrentLoopState.BreakCalled := False;
  CurrentLoopState.ContinueCalled := False;

  repeat
    ExecuteCodeBlock(LoopBlock);
    if CurrentLoopState.ContinueCalled then
    begin
      CurrentLoopState.ContinueCalled := False;
      Continue;
    end;
    if CurrentLoopState.BreakCalled then
      Break;
  until (Eval(Condition) <> 0);
  CurrentLoopState.BreakCalled := False;
end
    else
    begin
      ExecuteStatement(Line); // Execute single line
    end;

    // Handle break/continue that bubbled up from ExecuteStatement
    if CurrentLoopState.BreakCalled then Break;
    if CurrentLoopState.ContinueCalled then Continue;

  end;
end;


{ SIMPLIFIED: Executes only a single, simple statement }
procedure ExecuteStatement(Code: string);
var
V: TVariable;
ResultValue: Extended;
var
  Keyword, Rest, VarName, VarValue, Arg, InputPrompt, UserInput: string;
  FunctionName: AnsiString;
  FunctionArg: AnsiString;
begin
  Code := Trim(RemoveComments(Code));
  if Code = '' then Exit;

  // Loop Control
  if LowerCase(Code) = 'break' then
  begin
    CurrentLoopState.BreakCalled := True;
    Exit;
  end;
  if LowerCase(Code) = 'continue' then
  begin
    CurrentLoopState.ContinueCalled := True;
    Exit;
  end;

  // Block statements are NOT allowed in REPL / single statement context ---
  if (LowerCase(Copy(Code, 1, 2)) = 'if') or
     (LowerCase(Copy(Code, 1, 5)) = 'while') or
     (LowerCase(Copy(Code, 1, 6)) = 'repeat') then
  begin
     raise Exception.Create('Error: Block statements (if, while, repeat) are only supported in file mode.');
  end;

  // Variable Declaration
  if Pos(' ', Code) > 0 then
  begin
    Keyword := LowerCase(Copy(Code, 1, Pos(' ', Code)-1));
    if (Keyword = 'string') or (Keyword = 'float') or (Keyword = 'bool') or (Keyword = 'char') or (Keyword = 'int') then
    begin
      Rest := Trim(Copy(Code, Length(Keyword)+1, Length(Code)));
      if Pos('=', Rest) = 0 then
        raise Exception.Create('Error: Missing "=" in variable declaration.');
      VarName := Trim(Copy(Rest, 1, Pos('=', Rest)-1));
      VarValue := Trim(Copy(Rest, Pos('=', Rest)+1, Length(Rest)));

      if Keyword = 'string' then
      begin
        if (Length(VarValue) > 0) and (VarValue[1] = '(') then
          VarValue := EvaluateTextExpression(VarValue)
        else if (Length(VarValue) >= 2) and (VarValue[1] = '''') and (VarValue[Length(VarValue)] = '''') then
          VarValue := Copy(VarValue, 2, Length(VarValue)-2);
        SetStringValue(VarName, VarValue);
        Writeln('Variable ', VarName, ' (string) = ', GetStringValue(VarName));
      end
      else if Keyword = 'float' then
      begin
        SetFloatValue(VarName, Eval(VarValue));
        Writeln('Variable ', VarName, ' (float) = ', GetFloatValue(VarName):0:2);
      end
      else if Keyword = 'int' then
      begin
        SetIntValue(VarName, Eval(VarValue));
        Writeln('Variable ', VarName, ' (int) = ', GetIntValue(VarName):0:0);
      end

      else if Keyword = 'bool' then
      begin
        if (VarValue = 'true') or (VarValue = 'false') then
          SetBooleanValue(VarName, VarValue = 'true')
        else
          raise Exception.Create('Error: Value for bool must be true or false.');
        Writeln('Variable ', VarName, ' (bool) = ', BoolToStr(GetBooleanValue(VarName), True));
      end
      else if Keyword = 'char' then
      begin
         if (Length(VarValue) = 3) and (VarValue[1] = '''') and (VarValue[3] = '''') then
           SetCharValue(VarName, VarValue[2])
         else
           raise Exception.Create('Error: Value for char must be a single character in quotes (e.g. ''a'').');
         Writeln('Variable ', VarName, ' (char) = ', GetCharValue(VarName));
      end;
      Exit;
    end;
  end;

  // Assignment
  if Pos('=', Code) > 0 then
  begin
    VarName := Trim(Copy(Code, 1, Pos('=', Code)-1));
    VarValue := Trim(Copy(Code, Pos('=', Code)+1, Length(Code)));

    //getTick()
    if LowerCase(Trim(VarValue)) = 'gettick()' then
    begin
      SetIntValue(VarName, GetTickCount64);
      Writeln('Variable ', VarName, ' = ', GetIntValue(VarName):0:0, ' ms');
      Exit;
    end;

    // read('prompt')
    if LowerCase(Copy(VarValue, 1, 5)) = 'read(' then
    begin
      InputPrompt := Copy(VarValue, 6, Length(VarValue)-6);
      if (Length(InputPrompt) > 0) and (InputPrompt[Length(InputPrompt)] = ')') then
        Delete(InputPrompt, Length(InputPrompt), 1);
      InputPrompt := Trim(InputPrompt);
      if (Length(InputPrompt) >= 2) and (InputPrompt[1] = '''') and (InputPrompt[Length(InputPrompt)] = '''') then
      begin
        InputPrompt := Copy(InputPrompt, 2, Length(InputPrompt)-2);
      end;
      Write(InputPrompt + ': ');
      ReadLn(UserInput);
      SetStringValue(VarName, UserInput); // 'read' always returns a string
      Writeln('Variable ', VarName, ' = ', GetStringValue(VarName));
      Exit;
    end

    // strToFloat('text')
    else if LowerCase(Copy(VarValue, 1, 11)) = 'strtofloat(' then
    begin
      Arg := Copy(VarValue, 12, Length(VarValue)-12);
      if (Length(Arg) > 0) and (Arg[Length(Arg)] = ')') then Delete(Arg, Length(Arg), 1);
      Arg := Trim(Arg);
      if (Length(Arg) >= 2) and (Arg[1] = '''') and (Arg[Length(Arg)] = '''') then
        Arg := Copy(Arg, 2, Length(Arg)-2)
      else
        Arg := GetVariableDisplay(Arg); // Get value from variable
      try
        SetFloatValue(VarName, StrToFloat(Arg));
        Writeln('Variable ', VarName, ' = ', GetFloatValue(VarName):0:2);
      except
        on E: Exception do
          raise Exception.Create('Conversion Error strToFloat: ' + E.Message);
      end;
      Exit;
    end

    // floatToStr(number)
    else if LowerCase(Copy(VarValue, 1, 11)) = 'floattostr(' then
    begin
      Arg := Copy(VarValue, 12, Length(VarValue)-12);
      if (Length(Arg) > 0) and (Arg[Length(Arg)] = ')') then Delete(Arg, Length(Arg), 1);
      Arg := Trim(Arg);
      try
        SetStringValue(VarName, FloatToStr(Eval(Arg)));
        Writeln('Variable ', VarName, ' = ', GetStringValue(VarName));
      except
        on E: Exception do
          raise Exception.Create('Conversion Error floatToStr: ' + E.Message);
      end;
      Exit;
    end

    // Assigning boolean
    else if (VarValue = 'true') or (VarValue = 'false') then
    begin
      SetBooleanValue(VarName, VarValue = 'true');
      Writeln('Variable ', VarName, ' = ', BoolToStr(GetBooleanValue(VarName), True));
      Exit;
    end

    // Assigning char
    else if (Length(VarValue) = 3) and (VarValue[1] = '''') and (VarValue[3] = '''') then
    begin
        SetCharValue(VarName, VarValue[2]);
        Writeln('Variable ', VarName, ' = ', GetCharValue(VarName));
        Exit;
    end

    // Default: Assigning float expression
    else
    begin
    //  SetFloatValue(VarName, Eval(VarValue));
    // // Writeln('Variable ', VarName, ' = ', GetFloatValue(VarName):0:2);
    //   //Writeln('VarName', GetFloatValue(VarName):0:2);
    //  Exit;
    //end;
      ResultValue := Eval(VarValue); // Oblicz wartość tylko raz

      if Variables.TryGetValue(VarName, V) then
      begin
        // Zmienna istnieje, uszanuj jej typ
        case V.VarType of
          vtInt: SetIntValue(VarName, ResultValue);
          vtFloat: SetFloatValue(VarName, ResultValue);
        else
          // Próba przypisania liczby do np. stringu lub bool
          raise Exception.Create('Error: Cannot assign a number to a variable of type ' + VarTypeToString(V.VarType));
        end;
      end
      else
      begin
        // Zmienna nie istnieje - domyślnie stwórz float
        // (lub int, jeśli wolisz, wtedy zmień na SetIntValue)
        SetFloatValue(VarName, ResultValue);
      end;
      Exit;
    end;
  end;

  // --- Built-in Functions / Commands ---

  // print()
  if LowerCase(Copy(Code, 1, 6)) = 'print(' then
  begin
    Arg := Copy(Code, 7, Length(Code)-7);
    if (Length(Arg) > 0) and (Arg[Length(Arg)] = ')') then
      Delete(Arg, Length(Arg), 1);
    Arg := Trim(Arg);

    if (Pos('+', Arg) > 0) or (Pos('''', Arg) > 0) then
      Writeln(EvaluateTextExpression(Arg))
    else
    begin
      try
        Writeln(GetVariableDisplay(Arg)); // Try to print a variable
      except
        on E: Exception do
        try
          Writeln(Eval(Arg):0:2); // Try to print a math expression
        except
          on E2: Exception do
            raise Exception.Create('Error in print: Cannot interpret argument "' + Arg + '".');
        end;
      end;
    end;
    Exit;
  end;

  //sin
  if LowerCase(Copy(Code, 1, 4)) = 'sin(' then
    begin
      FunctionName := 'sin';
      FunctionArg := Copy(Code, 5, Length(Code)-5);
    end
    // cos()
    else if LowerCase(Copy(Code, 1, 4)) = 'cos(' then
    begin
      FunctionName := 'cos';
      FunctionArg := Copy(Code, 5, Length(Code)-5);
    end
    // sqrt()
    else if LowerCase(Copy(Code, 1, 5)) = 'sqrt(' then
    begin
      FunctionName := 'sqrt';
      FunctionArg := Copy(Code, 6, Length(Code)-6);
    end
    // Default: Evaluate expression
    else
    begin
      // If it's not 'print' or any function, treat it as an expression
      Writeln('Result: ', Eval(Code):0:2);
      Exit;
    end;

    // This code will only execute if sin, cos or sqrt is found

    if (Length(FunctionArg) > 0) and (FunctionArg[Length(FunctionArg)] = ')') then
      Delete(FunctionArg, Length(FunctionArg), 1);
    FunctionArg := Trim(FunctionArg);
    try
      if FunctionName = 'sin' then
        Writeln('Result: ', Sin(Eval(FunctionArg)):0:2)

      else if FunctionName = 'cos' then
        Writeln('Result: ', Cos(Eval(FunctionArg)):0:2)

      else if FunctionName = 'sqrt' then
        Writeln('Result: ', Sqrt(Eval(FunctionArg)):0:2);

    except
      on E: Exception do
        Writeln('Error in function ', FunctionName, ': ', E.Message);
    end;
    Exit;
end;

// Main Program

procedure LoadCodeFromFile(FilePath: string);
begin
  GlobalCodeLines := TStringList.Create;
  try
    GlobalCodeLines.LoadFromFile(FilePath);
    try
      RunProgram(GlobalCodeLines); // Run the whole program
    except
      on E: Exception do
        Writeln('Runtime Error: ', E.Message);
    end;
  finally
    GlobalCodeLines.Free;
  end;
end;

var
  Code: string;
begin
  Writeln('Tomato V1.0.0.0');
 // Writeln('Created by Programista Art');
 // Writeln('https://github.com/Programista-Art');
 // Writeln('-------------------');
  SetConsoleOutputCP(65001);

  Variables := TDictionary<string, TVariable>.Create;

  try
    if ParamCount > 0 then
      LoadCodeFromFile(ParamStr(1))
    else
    begin
      // Start REPL (Interactive Mode)
      repeat
        Write('> ');
        ReadLn(Code);
        if LowerCase(Code) = 'exit' then Break;
        try
          ExecuteStatement(Code); // REPL calls ExecuteStatement directly
        except
          on E: Exception do
            Writeln('An error occurred: ', E.Message);
        end;
      until False;
    end;
  finally
    Variables.Free;
  end;

  Writeln('End of program.');
end.
