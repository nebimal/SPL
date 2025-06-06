package edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store SPL declarations */
  var st = new SymbolTable

  def expandType(tp: Type): Type
  def typecheck(e: Expr): Type
  def typecheck(e: Lvalue): Type
  def typecheck(e: Stmt, expected_type: Type)
  def typecheck(e: Definition)
  def typecheck(e: Program)
}

class TypeCheck extends TypeChecker {

  /** typechecking error */
  def error( msg: String ): Type = {
    System.err.println("*** Typechecking Error: "+msg)
    System.err.println("*** Symbol Table: "+st)
    System.exit(1)
    null 
  }

  /** if tp is a named type, expand it */
  def expandType( tp: Type ): Type =
    tp match {
      case NamedType(nm) 
      =>  st.lookup(nm) match {
          case Some(TypeDeclaration(t)) 
          => expandType(t)
          case _ => error("Undeclared type: "+tp)
        }
      case _ => tp
    }

  def isRecordType(tp: Type): Boolean = expandType(tp) match {
    case RecordType(_) => true
    case _             => false
  }

  /** returns true if the types tp1 and tp2 are equal under structural equivalence */
  def typeEquivalence ( tp1: Type, tp2: Type ): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else expandType(tp1) match {
      case ArrayType(t1)
        => expandType(tp2) match {
              case ArrayType(t2)
                => typeEquivalence(t1,t2)
              case _ => false
           }
      case RecordType(fs1)
        => expandType(tp2) match {
              case RecordType(fs2)
                => fs1.length == fs2.length &&
                   (fs1 zip fs2).map{ case (Bind(v1,t1),Bind(v2,t2))
                                        => v1==v2 && typeEquivalence(t1,t2) }
                                .reduce(_&&_)
              case _ => false
           }
      case TupleType(ts1)
        => expandType(tp2) match {
              case TupleType(ts2)
                => ts1.length == ts2.length &&
                   (ts1 zip ts2).map{ case (t1,t2) => typeEquivalence(t1,t2) }
                                .reduce(_&&_)
              case _ => false
           }
      case _
        => tp2 match {
             case NamedType(n) => typeEquivalence(tp1,expandType(tp2))
             case _ => false
           }
    }

  /* tracing level */
  var level: Int = -1

 /** trace typechecking */
  def trace[T] ( e: Any, result: => T ): T = {
    if (trace_typecheck) {
       level += 1
       println(" "*(3*level)+"** "+e)
    }
    val res = result
    if (trace_typecheck) {
       print(" "*(3*level))
       if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition])
          println("->")
       else println("-> "+res)
       level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  /** typecheck an expression AST */
  def typecheck ( e: Expr ): Type =
    trace(e,e match {
      case BinOpExp(op,l,r)
        => val ltp = typecheck(l)
           val rtp = typecheck(r)
           if (!typeEquivalence(ltp,rtp))
              error("Incompatible types in binary operation: "+e)
           else if (op.equals("and") || op.equals("or"))
                   if (typeEquivalence(ltp,BooleanType()))
                      ltp
                   else error("AND/OR operation can only be applied to booleans: "+e)
           else if (op.equals("eq") || op.equals("neq"))
                   BooleanType()
           else if (!typeEquivalence(ltp,IntType()) && !typeEquivalence(ltp,FloatType()))
                   error("Binary arithmetic operations can only be applied to integer or real numbers: "+e)
           else if (op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq"))
                   BooleanType()
           else ltp

      case IntConst(value)    => IntType()
      case FloatConst(value)  => FloatType()
      case StringConst(value) => StringType()
      case BooleanConst(value)=> BooleanType()
      case LvalExp(value)     => typecheck(value)
      case NullExp()          => AnyType()

      case ArrayExp(el) =>
        if (el.isEmpty)
          ArrayType(AnyType()) 
        else {
          val headType = typecheck(el.head)
          el.tail.foreach { e =>
            val t = typecheck(e)
            if (!typeEquivalence(t, headType))
              error("Array elements must have the same type: " + e)
          }
          ArrayType(headType)
        }

      case ArrayGen(length, value) =>
        val lengthType = typecheck(length)
        if (!typeEquivalence(lengthType, IntType()))
          error("ArrayGen length must be an integer: " + length)
        val valueType = typecheck(value)
        ArrayType(valueType)

      case TupleExp(elements) =>
        TupleType(elements.map(typecheck))

      case RecordExp(components) =>
        RecordType(components.map { case Bind(name, expr) =>
          Bind(name, typecheck(expr))
        })

      case UnOpExp(op, operand) =>
        if (op == "minus") {
          val operandType = typecheck(operand)
          if (typeEquivalence(operandType, IntType()) || typeEquivalence(operandType, FloatType()))
            operandType
          else
            error("Unary minus can only be applied to integers or floats: " + operand)
        } else error("Unknown unary operator: " + op)

      case CallExp(name, args) =>
        st.lookup(name) match {
          case Some(FuncDeclaration(outType, params, _, _, _)) =>
            if (params.length != args.length)
              error("Function " + name + " expects " + params.length +
                    " arguments, but got " + args.length)
            else {
              params.zip(args).foreach { case (Bind(paramName, paramType), argExpr) =>
                val argType = typecheck(argExpr)
                if (!typeEquivalence(paramType, argType))
                  error("Function " + name + " expects argument of type " + paramType +
                        " for parameter " + paramName + " but got " + argType)
              }
              outType
            }
          case Some(_) => error(name + " is not a function")
          case None    => error("Undefined function: " + name)
        }

      case _ => throw new Error("Wrong expression: " + e)
    })

  /** typecheck an Lvalue AST */
  def typecheck(e: Lvalue): Type =
    trace(e, e match {
      case Var(name) 
      =>  st.lookup(name) match {
          case Some(VarDeclaration(t,_,_)) => t
          case Some(_) => error(name+" is not a variable")
          case None    => error("Undefined variable: " + name)
        }
      case ArrayDeref(array, index) =>
        val indexType = typecheck(index)
        val arrayType = typecheck(array)
        if (!typeEquivalence(indexType, IntType()))
          error("Array index must be an integer: " + index)
        expandType(arrayType) match {
          case ArrayType(elemType) => elemType
          case _ => error("Attempting to index a non-array: " + array)
        }
      case RecordDeref(record, attribute) =>
        expandType(typecheck(record)) match {
          case RecordType(components) =>
            components.find(_.name == attribute) match {
              case Some(Bind(_, t)) => t
              case None => error("Attribute " + attribute + " not found in record: " + record)
            }
          case _ => error("Attempting to dereference a non-record: " + record)
        }
      case TupleDeref(tuple, index) =>
        expandType(typecheck(tuple)) match {
          case TupleType(components) =>
            if (index >= 0 && index < components.length)
              components(index)
            else error("Tuple index out of bounds: " + index)
          case _ => error("Attempting to dereference a non-tuple: " + tuple)
        }
      case _ => throw new Error("Wrong lvalue: " + e)
    })

  /** typecheck a statement AST using the expected type of the return value from the current function */
  def typecheck(e: Stmt, expected_type: Type) {
    trace(e, e match {
      case AssignSt(d, s)
      =>  if (!typeEquivalence(typecheck(d), typecheck(s)))
          error("Incompatible types in assignment: " + e)

      case BlockSt(dl, sl) =>
        st.begin_scope()
        dl.foreach(typecheck)
        sl.foreach(typecheck(_, expected_type))
        st.end_scope()

      case PrintSt(a) =>
        a.foreach(typecheck)

      case CallSt(name, args) =>
        st.lookup(name) match {
          case Some(FuncDeclaration(outType, params, _, _, _)) =>
            if (params.length != args.length)
              error("Function " + name + " expects " + params.length +
                    " arguments, but got " + args.length)
            else {
              params.zip(args).foreach { case (Bind(paramName, paramType), argExpr) =>
                val argType = typecheck(argExpr)
                if (!typeEquivalence(paramType, argType))
                  error("Function " + name + " expects argument of type " +
                        paramType + " for parameter " + paramName +
                        " but got " + argType)
              }
            }
          case Some(_) => error(name + " is not a function")
          case None    => error("Undefined function: " + name)
        }

      case ReadSt(args) =>
        args.foreach(typecheck)

      case ForSt(variable, init, step, incr, body) =>
        val initialType = typecheck(init)
        if (!typeEquivalence(initialType, IntType()))
          error("For loop initial value must be of type Int: " + init)
        val stepType = typecheck(step)
        if (!typeEquivalence(stepType, IntType()))
          error("For loop step value must be of type Int: " + step)
        val incrementType = typecheck(incr)
        if (!typeEquivalence(incrementType, IntType()))
          error("For loop increment value must be of type Int: " + incr)
        st.begin_scope()
        st.insert(variable, VarDeclaration(IntType(), 0, 0))
        typecheck(body, NoType())
        st.end_scope()

      case IfSt(cond, then_stmt, else_stmt) =>
        if (!typeEquivalence(typecheck(cond), BooleanType()))
          error("Condition in if-statement must be boolean: " + cond)
        typecheck(then_stmt, expected_type)
        if (else_stmt != null) typecheck(else_stmt, expected_type)

      case WhileSt(cond, body) =>
        if (!typeEquivalence(typecheck(cond), BooleanType()))
          error("Condition in while-statement must be boolean: " + cond)
        typecheck(body, expected_type)

      case LoopSt(body) =>
        typecheck(body, expected_type)

      case ReturnValueSt(value) =>
        val vType = typecheck(value)
        if (!typeEquivalence(vType, expected_type))
          error("Return statement type mismatch: Expected " + expected_type + " but got " + vType)

      case ReturnSt() =>
        if (!typeEquivalence(NoType(), expected_type))
          error("Return statement not expected in this context")

      case ExitSt() =>
        () 

      case _ => throw new Error("Wrong statement: " + e)
    })
  }

  /** typecheck a definition */
  def typecheck(e: Definition) {
    trace(e, e match {
      case FuncDef(f, ps, ot, b) =>
        st.insert(f, FuncDeclaration(ot, ps, "", 0, 0))
        st.begin_scope()
        ps.foreach { case Bind(v, tp) => st.insert(v, VarDeclaration(tp, 0, 0)) }
        typecheck(b, ot)
        st.end_scope()

      case VarDef(name, hasType, value) =>
        val valueType = typecheck(value)
        val finalType =
          if (hasType.isInstanceOf[AnyType])
            valueType
          else if (!typeEquivalence(hasType, valueType))
            error("Variable definition type mismatch in: " + e)
          else
            hasType
        st.insert(name, VarDeclaration(finalType, 0, 0))

      case TypeDef(name, definedType) =>
        st.insert(name, TypeDeclaration(definedType))

      case _ => throw new Error("Wrong definition: " + e)
    })
  }
  
  /** typecheck the main program */
  def typecheck( e: Program ) {
    typecheck(e.body,NoType())
  }
}
