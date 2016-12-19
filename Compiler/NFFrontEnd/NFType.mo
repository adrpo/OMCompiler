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

encapsulated package NFType

import NFDimension.Dimension;

uniontype Type
  record INTEGER
  end INTEGER;

  record REAL
  end REAL;

  record STRING
  end STRING;

  record BOOLEAN
  end BOOLEAN;

  record CLOCK
  end CLOCK;

  record ENUMERATION
  end ENUMERATION;

  record ARRAY
    Type elementType;
    list<Dimension> dimensions;
  end ARRAY;

  record NORETCALL
  end NORETCALL;

  record UNKNOWN
  end UNKNOWN;

  record COMPLEX
  end COMPLEX;

  record FUNCTION
    Type resultType;
  end FUNCTION;

  function liftArrayLeft
    "Adds an array dimension to a type on the left side, e.g.
       listArrayLeft(Real[2, 3], [4]) => Real[4, 2, 3]."
    input output Type ty;
    input Dimension dim;
  algorithm
    ty := match ty
      case ARRAY() then ARRAY(ty.elementType, dim :: ty.dimensions);
      else ARRAY(ty, {dim});
    end match;
  end liftArrayLeft;

  function liftArrayLeftList
    "Adds array dimensions to a type on the left side, e.g.
       listArrayLeft(Real[2, 3], [4, 5]) => Real[4, 5, 2, 3]."
    input output Type ty;
    input list<Dimension> dims;
  algorithm
    if listEmpty(dims) then
      return;
    end if;

    ty := match ty
      case ARRAY() then ARRAY(ty.elementType, listAppend(dims, ty.dimensions));
      else ARRAY(ty, dims);
    end match;
  end liftArrayLeftList;

  function isInteger
    input Type ty;
    output Boolean isInteger;
  algorithm
    isInteger := match ty
      case INTEGER() then true;
      else false;
    end match;
  end isInteger;

  function isReal
    input Type ty;
    output Boolean isReal;
  algorithm
    isReal := match ty
      case REAL() then true;
      else false;
    end match;
  end isReal;

  function isBoolean
    input Type ty;
    output Boolean isBool;
  algorithm
    isBool := match ty
      case BOOLEAN() then true;
      else false;
    end match;
  end isBoolean;

  function isString
    input Type ty;
    output Boolean isString;
  algorithm
    isString := match ty
      case STRING() then true;
      else false;
    end match;
  end isString;

  function isArray
    input Type ty;
    output Boolean isArray;
  algorithm
    isArray := match ty
      case ARRAY() then true;
      else false;
    end match;
  end isArray;

  function isEnumeration
    input Type ty;
    output Boolean isEnum;
  algorithm
    isEnum := match ty
      case ENUMERATION() then true;
      else false;
    end match;
  end isEnumeration;

  function isScalarArray
    input Type ty;
    output Boolean isScalar;
  algorithm
    isScalar := match ty
      case ARRAY(dimensions = {_}) then true;
      else false;
    end match;
  end isScalarArray;

  function isBasicNumeric
    input Type ty;
    output Boolean isNumeric;
  algorithm
    isNumeric := match ty
      case REAL() then true;
      case INTEGER() then true;
      else false;
    end match;
  end isBasicNumeric;

  function isNumeric
    input Type ty;
    output Boolean isNumeric;
  algorithm
    isNumeric := match ty
      case ARRAY() then isBasicNumeric(ty.elementType);
      else isBasicNumeric(ty);
    end match;
  end isNumeric;

  function arrayElementType
    input Type ty;
    output Type elementTy;
  algorithm
    elementTy := match ty
      case ARRAY() then ty.elementType;
      else ty;
    end match;
  end arrayElementType;

  function setArrayElementType
    input Type arrayTy;
    input Type elementTy;
    output Type ty;
  algorithm
    ty := match arrayTy
      case ARRAY() then ARRAY(elementTy, arrayTy.dimensions);
      else elementTy;
    end match;
  end setArrayElementType;

  function elementType
    input Type ty;
    output Type elementTy;
  algorithm
    elementTy := match ty
      case ARRAY() then ty.elementType;
      case FUNCTION() then ty.resultType;
      else ty;
    end match;
  end elementType;

  function arrayDims
    input Type ty;
    output list<Dimension> dims;
  algorithm
    dims := match ty
      case ARRAY() then ty.dimensions;
      case FUNCTION() then arrayDims(ty.resultType);
    end match;
  end arrayDims;

  function dimensionCount
    input Type ty;
    output Integer dimCount;
  algorithm
    dimCount := match ty
      case ARRAY() then listLength(ty.dimensions);
      case FUNCTION() then dimensionCount(ty.resultType);
      else 0;
    end match;
  end dimensionCount;

  function scalarSuperType
    "Checks that the given types are scalar and that one is a subtype of the other."
    input Type ty1;
    input Type ty2;
    output Type ty;
  algorithm
    ty := match (ty1, ty2)
      case (INTEGER(), INTEGER()) then INTEGER();
      case (REAL(), REAL()) then REAL();
      case (INTEGER(), REAL()) then REAL();
      case (REAL(), INTEGER()) then REAL();
      case (BOOLEAN(), BOOLEAN()) then BOOLEAN();
    end match;
  end scalarSuperType;

  function toString
    input Type ty;
    output String str;
  algorithm
    str := "IMPLEMENT ME";
  end toString;

  function toDAEType
    input Type ty;
    output DAE.Type daeTy;
  algorithm
    daeTy := match ty
      case Type.INTEGER() then DAE.T_INTEGER_DEFAULT;
      case Type.REAL() then DAE.T_REAL_DEFAULT;
      case Type.STRING() then DAE.T_STRING_DEFAULT;
      case Type.BOOLEAN() then DAE.T_BOOL_DEFAULT;
      case Type.CLOCK() then DAE.T_CLOCK_DEFAULT;
      case Type.ARRAY()
        then DAE.T_ARRAY(toDAEType(ty.elementType),
          list(Dimension.toDAEDim(d) for d in ty.dimensions), DAE.emptyTypeSource);
      case Type.NORETCALL() then DAE.T_NORETCALL_DEFAULT;
      case Type.UNKNOWN() then DAE.T_UNKNOWN_DEFAULT;
      else
        algorithm
          assert(false, getInstanceName() + " got unknown type.");
        then
          fail();
    end match;
  end toDAEType;
end Type;

annotation(__OpenModelica_Interface="frontend");
end NFType;
