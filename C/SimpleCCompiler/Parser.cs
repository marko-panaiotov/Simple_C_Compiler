using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace SimpleCCompiler
{
	public class Parser
	{
        public const string specialSymbol = "Expect a special symbol";

        private Scanner scanner;
		private Emit emit;
		private Table symbolTable;
		private Token token;
		private Diagnostics diag;
		
		private Stack<Label> breakStack = new Stack<Label>();
		private Stack<Label> continueStack = new Stack<Label>();

        private const String program = "Program";
        private const String system = "System";


        public Parser(Scanner scanner, Emit emit, Table symbolTable, Diagnostics diag)
		{
			this.scanner = scanner;
			this.emit = emit;
			this.symbolTable = symbolTable;
			this.diag = diag;
		}
		
		public void AddPredefinedSymbols()
		{
			symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1,-1, "int"), typeof(System.Int32)));
            symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1, -1, "short"), typeof(System.Int16)));
            symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1, -1, "long"), typeof(System.Int64)));
            symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1,-1, "bool"), typeof(System.Boolean)));
			symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1,-1, "double"), typeof(System.Double)));
			symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1,-1, "char"), typeof(System.Char)));
			symbolTable.AddToUniverse(new PrimitiveTypeSymbol(new IdentToken(-1,-1, "string"), typeof(System.String)));
		}
		
		public bool Parse()
		{
			ReadNextToken();
			AddPredefinedSymbols();
			return IsProgram() && token is EOFToken;
		}
		
		public void ReadNextToken()
		{
			token = scanner.Next();
		}
		
		public bool CheckKeyword(string keyword)
		{
			bool result = (token is KeywordToken) && ((KeywordToken)token).value==keyword;
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckSpecialSymbol(string symbol)
		{
			bool result = (token is SpecialSymbolToken) && ((SpecialSymbolToken)token).value==symbol;
			if (result) ReadNextToken();
			return result;
		}

        public bool CheckEndOfFile()
        {
            bool result = (token is EOFToken);
            return result;
        }

        public bool CheckIdent()
		{
			bool result = (token is IdentToken);
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckNumber()
		{
			bool result = (token is NumberToken);
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckDouble()
		{
			bool result = (token is DoubleToken);
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckBoolean()
		{
			bool result = (token is BooleanToken);
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckChar()
		{
			bool result = (token is CharToken);
			if (result) ReadNextToken();
			return result;
		}
		
		public bool CheckString()
		{
			bool result = (token is StringToken);
			if (result) ReadNextToken();
			return result;
		}
		
		void SkipUntilSemiColon() {
			Token Tok;
			do {
				Tok = scanner.Next();
			} while (!((Tok is EOFToken) ||
				  	   (Tok is SpecialSymbolToken) && ((Tok as SpecialSymbolToken).value == ";")));
		}
		
		public void Error(string message)
		{
			diag.Error(token.line, token.column, message);
			SkipUntilSemiColon();
		}
		
		public void Error(string message, Token token)
		{
			diag.Error(token.line, token.column, message);
			SkipUntilSemiColon();
		}
		
		public void Error(string message, Token token, params object[] par)
		{
			diag.Error(token.line, token.column, string.Format(message, par));
			SkipUntilSemiColon();
		}
		
		public void Warning(string message)
		{
			diag.Warning(token.line, token.column, message);
		}
		
		public void Warning(string message, Token token)
		{
			diag.Warning(token.line, token.column, message);
		}
		
		public void Warning(string message, Token token, params object[] par)
		{
			diag.Warning(token.line, token.column, string.Format(message, par));
		}
		
		public void Note(string message)
		{
			diag.Note(token.line, token.column, message);
		}
		
		public void Note(string message, Token token)
		{
			diag.Note(token.line, token.column, message);
		}
		
		public void Note(string message, Token token, params object[] par)
		{
			diag.Note(token.line, token.column, string.Format(message, par));
		}

        // [1] Program = {Statement}.
        public bool IsProgram()
        {
            AddToUniverse();
            AddMethodInfo();

            while (IsStatement()) ;

            symbolTable.EndScope();
            return diag.GetErrorCount() == 0;
        }

        private void AddToUniverse()
        {
            IdentToken identityToken = new IdentToken(1, 1, program);
            symbolTable.AddUsingNamespace(system);
            symbolTable.AddToUniverse(new PrimitiveTypeSymbol(identityToken, emit.InitProgramClass(identityToken.value)));
        }

        public void AddMethodInfo()
        {
            IdentToken MainMethodName = new IdentToken(1, 1, "main");
            Type MainMathodType = typeof(Int32);

            List<FormalParamSymbol> formalParams = new List<FormalParamSymbol>();
            List<Type> formalParamTypes = new List<Type>();

            MethodSymbol mainMethodToken = symbolTable.AddMethod(MainMethodName, MainMathodType, formalParams.ToArray(), null);

            symbolTable.BeginScope();
            mainMethodToken.methodInfo = emit.AddMethod(MainMethodName.value, MainMathodType, formalParamTypes.ToArray());
        }

        private void DeclareVariable()
        {
            if (token is IdentToken)
            {
                AddLocalVar();
            }
        }

        private void AddLocalVar()
        {
            IdentToken name = token as IdentToken;
            if (!symbolTable.ExistCurrentScopeSymbol(name.value))
            {
                symbolTable.AddLocalVar(name, emit.AddLocalVar(name.value, typeof(System.Int32)));
            }
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        // [2] Statement = [Expression] ';'

        // се използва за анализиране на конкретни части от програмния код като програмни блокове, оператори, изрази
        public bool IsStatement()
        {
            Type type = null;
            LocationInfo location;

            DeclareVariable();

            if (IsLocation(out location))
            {
                LocalVarSymbol localvar = location.id as LocalVarSymbol;
                if (localvar != null)
                {
                    if (CheckSpecialSymbol("="))
                    {
                        if (!IsExpression(null, out type))
                            Error("Expect an expression");

                        if (!CheckSpecialSymbol(";"))
                            Error(specialSymbol + "';'");

                        emit.AddLocalVarAssigment(localvar.localVariableInfo);
                        return true;
                    }
                }
                CheckExpression(location, type);
            }
            else if (IsExpression(null, out type))
            {
                if (!CheckSpecialSymbol(";"))
                    Error(specialSymbol + "';'");
            }
            else
            {
                return false;
            }

            if (CheckEndOfFile())
                return false;
            return true;

            // задача - изпит 'skip' statement
            if (CheckKeyword("skip"))
            {
                if (!IsStatement())
                {
                    Error("Expect statement!");
                }

                MethodInfo bestMethodInfo = typeof(Console).GetMethod("WriteLine", new Type[] { type });

                if (bestMethodInfo != null)
                {
                    emit.AddMethodCall(bestMethodInfo);
                }
                else Error("Skip cannot be used!");

                type = bestMethodInfo.ReturnType;
                return true;
            }
        }

        private void CheckExpression(LocationInfo location, Type type)
        {
            if (!IsExpression(location, out type))
                Error("NOT FOUND IDENTIFIER", location.id);

            if (!CheckSpecialSymbol(";"))
                Error(specialSymbol + "' ; '");
        }

        public bool IsLocation(out LocationInfo location)
        {
            IdentToken id = token as IdentToken;

            if (!CheckIdent())
            {
                location = null;
                return false;
            }

            location = new LocationInfo();
            location.id = symbolTable.GetSymbol(id.value);
            // Семантична грешка - деклариран ли е вече идентификатора?
            if (location.id == null) Error("Identificator has not been declared!!! {0}", id, id.value);

            return true;
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        // [3] Expression = BitwiseAndExpression {'|' BitwiseAndExpression}.

        // се използва за анализиране на конкретни части от програмния код като програмни блокове, оператори, изрази
        public bool IsExpression(LocationInfo location, out Type type)
        {
            SpecialSymbolToken opToken;

            if (!IsBitwiseAndExpression(location, out type))
                return false;

            opToken = token as SpecialSymbolToken;

            while (CheckSpecialSymbol("|"))
            {
                if (!IsBitwiseAndExpression(null, out type))
                    Error("Expected BitwiseAndExpression");

                emit.AddAdditiveOp(opToken.value);

                opToken = token as SpecialSymbolToken;
            }

            return true;
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        //[4] BitwiseAndExpression = AdditiveExpression {'&' AdditiveExpression}.
        public bool IsBitwiseAndExpression(LocationInfo location, out Type type)
        {
            SpecialSymbolToken opToken;
            if (!IsAdditiveExpr(location, out type))
                return false;

            opToken = token as SpecialSymbolToken;
            while (CheckSpecialSymbol("&"))
            {
                if (!IsAdditiveExpr(null, out type)) Error("Expect AdditiveExpression");

                emit.AddMultiplicativeOp(opToken.value);
                opToken = token as SpecialSymbolToken;
            }

            return true;
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        //[5] AdditiveExpression = MultiplicativeExpression {('+' | '-') MultiplicativeExpression}.
        public bool IsAdditiveExpr(LocationInfo location, out Type type)
        {
            SpecialSymbolToken opToken;

            if (!IsMultiplicativeExpr(location, out type)) return false;

            opToken = token as SpecialSymbolToken;

            while (CheckSpecialSymbol("+") || CheckSpecialSymbol("-"))
            {

                if (!IsMultiplicativeExpr(null, out type)) Error("Expected MultiplicativeExpression");

                emit.AddAdditiveOp(opToken.value);
                opToken = token as SpecialSymbolToken;
            }
            return true;
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        //[6] MultiplicativeExpression = PrimaryExpression {('*' | '/' | '%') PrimaryExpression}.
        public bool IsMultiplicativeExpr(LocationInfo location, out Type type)
        {
            SpecialSymbolToken opToken;
            if (!isDoubleNumber(location, out type)) return false;

            opToken = token as SpecialSymbolToken;
            while (CheckSpecialSymbol("*") || CheckSpecialSymbol("/") || CheckSpecialSymbol("%"))
            {
                if (!IsPrimaryExpression(null, out type))
                    Error("Expect PrimaryExpression");

                if (opToken != null)
                {
                    emit.AddMultiplicativeOp(opToken.value);

                    opToken = token as SpecialSymbolToken;
                }
                else
                    Error("MultiplicativeExpression -> opToken == null");
            }
            return true;
        }

        public bool isDoubleNumber(LocationInfo location, out Type type)
        {
            DoubleToken opToken;
            if (!IsPrimaryExpression(location, out type)) return false;

            opToken = token as DoubleToken;
            while (CheckSpecialSymbol("."))
            {
                if (!IsPrimaryExpression(null, out type))
                    Error("Expect decimal");

                if (opToken != null)
                {
                    emit.AddGetDouble(opToken.value);

                    opToken = token as DoubleToken;
                }
                else
                    Error("MultiplicativeExpression -> opToken == null");
            }
            return true;
        }

        //-----------------------------------------------------------------------------------------------------------------------------------

        //[7] PrimaryExpression = Ident ['=' Expression] | '~' PrimaryExpression | '++' Ident |
        // '--' Ident | Ident '++' | Ident '--' | Number | PrintFunc | ScanfFunc | '(' Expression ')'.
        public enum IncDecOps { None, PreInc, PreDec, PostInc, PostDec }
        public bool IsPrimaryExpression(LocationInfo location, out Type type)
        {
            SpecialSymbolToken opToken;
            Token Numbliteral = token;
            IncDecOps incDecOp = IncDecOps.None;

            if (location != null)
            {
                opToken = null;
            }
            else
            {
                opToken = token as SpecialSymbolToken;
                if (CheckSpecialSymbol("++"))
                    incDecOp = IncDecOps.PreInc;

                else if (CheckSpecialSymbol("--"))
                    incDecOp = IncDecOps.PreDec;

                if (!IsLocation(out location) && incDecOp != IncDecOps.None)
                    Error("Expect a variable, argument or a field");
            }

            if (incDecOp == IncDecOps.None)
            {
                opToken = token as SpecialSymbolToken;
                if (CheckSpecialSymbol("++"))
                    incDecOp = IncDecOps.PostInc;

                else if (CheckSpecialSymbol("--"))
                    incDecOp = IncDecOps.PostDec;
            }

            if (location != null)
            {
                LocalVarSymbol localvar = location.id as LocalVarSymbol;
                if (localvar != null)
                {
                    type = localvar.localVariableInfo.LocalType;

                    emit.AddGetLocalVar(localvar.localVariableInfo);
                    emit.AddIncLocalVar(localvar.localVariableInfo, incDecOp);

                    return true;
                }
            }

            if (CheckSpecialSymbol("~"))
            {
                if (!IsPrimaryExpression(null, out type))
                    Error("Expect PrimaryExpression");

                emit.AddUnaryOp(opToken.value);
            }

            if (CheckKeyword("printf"))
            {
                if (!CheckSpecialSymbol("("))
                    Error(specialSymbol + "(");

                if (!IsExpression(null, out type))
                    Error("Expect a expression");

                if (!CheckSpecialSymbol(")"))
                    Error(specialSymbol + ")");

                MethodInfo bestMethodInfo = typeof(Console).GetMethod("WriteLine", new Type[] { typeof(int) });

                if (bestMethodInfo != null)
                {
                    emit.AddMethodCall(bestMethodInfo);
                }
                else
                    Error("ScanF cannot be Used!");

                type = bestMethodInfo.ReturnType;
                return true;
            }

            if (CheckKeyword("printd"))
            {
                if (!CheckSpecialSymbol("("))
                    Error(specialSymbol + "(");

                if (!IsExpression(null, out type))
                    Error("Expect a expression");

                if (!CheckSpecialSymbol(")"))
                    Error(specialSymbol + ")");

                MethodInfo bestMethodInfo = typeof(Console).GetMethod("WriteLine", new Type[] { typeof(double) });

                if (bestMethodInfo != null)
                {
                    emit.AddMethodCall(bestMethodInfo);
                }
                else
                    Error("ScanD cannot be Used!");

                type = bestMethodInfo.ReturnType;
                return true;
            }
            if (CheckDouble())
            {
                type = typeof(System.Double);

                emit.AddGetDouble(((DoubleToken)Numbliteral).value);
                return true;
            }
            if (CheckNumber())
            {
                type = typeof(System.Int32);

                emit.AddGetNumber(((NumberToken)Numbliteral).value);
                return true;
            }

            if (CheckKeyword("scanf"))
            {
                if (!CheckSpecialSymbol("("))
                    Error(specialSymbol + "(");

                if (!CheckSpecialSymbol(")"))
                    Error(specialSymbol + ")");

                MethodInfo bestMethodInfo = typeof(Console).GetMethod("ReadLine");
                MethodInfo convertInt32M = typeof(Convert).GetMethod("ToInt32", new Type[] { typeof(string) });

                if (bestMethodInfo != null)
                {
                    emit.AddMethodCall(bestMethodInfo);
                    emit.AddMethodCall(convertInt32M);
                }
                else
                    Error("There is no suitable combination of parameter types for the method scanf");

                type = bestMethodInfo.ReturnType;
                return true;
            }

            if (CheckKeyword("scand"))
            {
                if (!CheckSpecialSymbol("("))
                    Error(specialSymbol + "(");

                if (!CheckSpecialSymbol(")"))
                    Error(specialSymbol + ")");

                MethodInfo bestMethodInfo = typeof(Console).GetMethod("ReadLine");
                MethodInfo convertDouble = typeof(Convert).GetMethod("ToDouble", new Type[] { typeof(string) });

                if (bestMethodInfo != null)
                {
                    emit.AddMethodCall(bestMethodInfo);
                    emit.AddMethodCall(convertDouble);
                }
                else
                    Error("There is no suitable combination of parameter types for the method scanf");

                type = bestMethodInfo.ReturnType;
                return true;
            }

            //Check Symbol
            if (CheckSpecialSymbol("("))
            {
                if (!IsExpression(location, out type))
                    Error("Exepect an expression");

                if (!CheckSpecialSymbol(")"))
                    Error(specialSymbol + ")");

                return true;
            }

            type = null;
            return true;
        }

        public class LocationInfo
        {
            public TableSymbol id;
            public bool isArray;
        }
    }
}
