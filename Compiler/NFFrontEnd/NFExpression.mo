/*
 * This file is part of OpenModelica.
 *
 * Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
 * c/o Linköpings universitet, Department of Computer and Information Science,
 * SE-58183 Linköping, Sweden.
 *
 * All rights reserved.
 *
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
 * THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
 * ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
 * RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
 * ACCORDING TO RECIPIENTS CHOICE.
 *
 * The OpenModelica software and the Open Source Modelica
 * Consortium (OSMC) Public License (OSMC-PL) are obtained
 * from OSMC, either from the above address,
 * from the URLs: http://www.ida.liu.se/projects/OpenModelica or
 * http://www.openmodelica.org, and in the OpenModelica distribution.
 * GNU version 3 is obtained from: http://www.gnu.org/copyleft/gpl.html.
 *
 * This program is distributed WITHOUT ANY WARRANTY; without
 * even the implied warranty of  MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
 *
 * See the full OSMC Public License conditions for more details.
 *
 */

encapsulated package NFExpression

import NFInstNode.InstNode;
import NFPrefix.Prefix;
import NFType.Type;
import DAE;

protected
import ExpressionSimplify;

public
uniontype CallAttributes
  record CALL_ATTR
    Type ty "The type of the return value, if several return values this is undefined";
    Boolean tuple_ "tuple" ;
    Boolean builtin "builtin Function call" ;
    Boolean isImpure "if the function has prefix *impure* is true, else false";
    Boolean isFunctionPointerCall;
    DAE.InlineType inlineType;
    DAE.TailCall tailCall "Input variables of the function if the call is tail-recursive";
  end CALL_ATTR;
end CallAttributes;

public constant CallAttributes callAttrBuiltinBool = CALL_ATTR(Type.BOOLEAN(),false,true,false,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinInteger = CALL_ATTR(Type.INTEGER(),false,true,false,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinReal = CALL_ATTR(Type.REAL(),false,true,false,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinString = CALL_ATTR(Type.STRING(),false,true,false,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinOther = CALL_ATTR(Type.UNKNOWN(),false,true,false,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinImpureBool = CALL_ATTR(Type.BOOLEAN(),false,true,true,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinImpureInteger = CALL_ATTR(Type.INTEGER(),false,true,true,false,DAE.NO_INLINE(),DAE.NO_TAIL());
public constant CallAttributes callAttrBuiltinImpureReal = CALL_ATTR(Type.REAL(),false,true,true,false,DAE.NO_INLINE(),DAE.NO_TAIL());

uniontype Expression
  record INTEGER
    Integer value;
  end INTEGER;

  record REAL
    Real value;
  end REAL;

  record STRING
    String value;
  end STRING;

  record BOOLEAN
    Boolean value;
  end BOOLEAN;

  record CREF
    InstNode component;
    Prefix prefix;
  end CREF;

  record ARRAY
    Type ty;
    list<Expression> elements;
  end ARRAY;

  record RANGE
    Type ty;
    Expression start;
    Option<Expression> step;
    Expression stop;
  end RANGE;

  record CALL
    Absyn.Path path;
    list<Expression> arguments;
    CallAttributes attr;
  end CALL;

  record SIZE
    Expression exp;
    Option<Expression> dimIndex;
  end SIZE;

  record BINARY "Binary operations, e.g. a+4"
    Expression exp1;
    DAE.Operator operator;
    Expression exp2;
  end BINARY;

  record UNARY "Unary operations, -(4x)"
    DAE.Operator operator;
    Expression exp;
  end UNARY;

  record LBINARY "Logical binary operations: and, or"
    Expression exp1;
    DAE.Operator operator;
    Expression exp2;
  end LBINARY;

  record LUNARY "Logical unary operations: not"
    DAE.Operator operator;
    Expression exp;
  end LUNARY;

  record RELATION "Relation, e.g. a <= 0"
    Expression exp1;
    DAE.Operator operator;
    Expression exp2;
  end RELATION;

  record IF
    Expression condition;
    Expression trueBranch;
    Expression falseBranch;
  end IF;

  record UNBOX "MetaModelica value unboxing (similar to a cast)"
    Expression exp;
    Type ty;
  end UNBOX;

  function isTrue
    input Expression exp;
    output Boolean isTrue;
  algorithm
    isTrue := match exp
      case BOOLEAN(true) then true;
      else false;
    end match;
  end isTrue;

  function typeOf
    input Expression exp;
    output Type ty;
  algorithm
    ty := match exp
      case INTEGER() then Type.INTEGER();
      case REAL() then Type.REAL();
      case STRING() then Type.STRING();
      case BOOLEAN() then Type.BOOLEAN();
      else Type.UNKNOWN();
    end match;
  end typeOf;

  function typeCast
    input output Expression exp;
    input Type ty;
  algorithm
    //exp := DAE.CAST(ty, exp);
    //exp := ExpressionSimplify.simplify1(exp);
  end typeCast;

  function typeCastElements
    input output Expression exp;
    input Type ty;
  protected
    Type t;
  algorithm
    t := typeOf(exp);
    t := Type.setArrayElementType(t, ty);
    exp := typeCast(exp, t);
  end typeCastElements;

  function realValue
    input Expression exp;
    output Real value;
  algorithm
    value := match exp
      case REAL() then exp.value;
      case INTEGER() then intReal(exp.value);
    end match;
  end realValue;

  function subscript
    input output Expression exp;
    input list<DAE.Subscript> subscripts;
  algorithm

  end subscript;

  function toString
    input Expression exp;
    output String str;
  algorithm
    str := match exp
      case INTEGER() then String(exp.value);
      case REAL() then String(exp.value);
      case STRING() then exp.value;
      case BOOLEAN() then String(exp.value);
      else "NFExpression.toString: IMPLEMENT ME";
    end match;
  end toString;

  function toDAEExp
    input Expression exp;
    output DAE.Exp dexp;
  algorithm
    dexp := match exp
      case INTEGER() then DAE.ICONST(exp.value);
      case REAL() then DAE.RCONST(exp.value);
      case STRING() then DAE.SCONST(exp.value);
      case BOOLEAN() then DAE.BCONST(exp.value);

      case CREF()
        then DAE.CREF(DAE.CREF_IDENT(InstNode.name(exp.component),
          DAE.T_UNKNOWN_DEFAULT, {}), DAE.T_UNKNOWN_DEFAULT);

      case ARRAY()
        then DAE.ARRAY(Type.toDAEType(exp.ty), Type.isScalarArray(exp.ty),
          list(toDAEExp(e) for e in exp.elements));

      case BINARY()
        then DAE.BINARY(toDAEExp(exp.exp1), exp.operator, toDAEExp(exp.exp2));
      case UNARY()
        then DAE.UNARY(exp.operator, toDAEExp(exp.exp));

      else
        algorithm
          assert(false, getInstanceName() + " got unknown expression");
        then
          fail();

    end match;
  end toDAEExp;

  function makeBuiltinCall
    "Create a CALL with the given data for a call to a builtin function."
    input String name;
    input list<Expression> args;
    input Type result_type;
    input Boolean isImpure;
    output Expression call;
    annotation(__OpenModelica_EarlyInline = true);
  algorithm
    call := Expression.CALL(Absyn.IDENT(name), args,
      CallAttributes.CALL_ATTR(result_type, false, true, isImpure, false, DAE.NO_INLINE(), DAE.NO_TAIL()));
  end makeBuiltinCall;

  function makePureBuiltinCall
    "Create a CALL with the given data for a call to a builtin function."
    input String name;
    input list<Expression> args;
    input Type result_type;
    output Expression call;
    annotation(__OpenModelica_EarlyInline = true);
  algorithm
    call := makeBuiltinCall(name, args, result_type, false);
  end makePureBuiltinCall;
end Expression;

annotation(__OpenModelica_Interface="frontend");
end NFExpression;
